;;; helm-project-buffer.el -*- lexical-binding: t -*-

(eval-when-compile (require 'cl-lib))
(require 'subr-x)
(require 'seq)
(require 'helm)
(require 'helm-buffers)
(require 'helm-utils)

(cl-defun helm-project-buffer-buffer-backend (_buffer)
  (vc-backend (buffer-file-name _buffer)))

(cl-defun helm-project-buffer-buffer-registerd (_buffer)
  (helm-aif (buffer-file-name _buffer)
      (if (and (not (tramp-tramp-file-p it))
               (file-exists-p it))
          (vc-registered it)
        nil)
    nil))

(cl-defun helm-project-buffer-buffer-root-and-backend (_buffer)
  (cons (helm-project-buffer-buffer-backend _buffer)
        (helm-project-buffer-buffer-root _buffer)))

(cl-defun helm-project-buffer-buffer-backend (_buffer)
  (cl-letf* ((file (buffer-file-name _buffer))
             (backend (vc-backend file)))
    backend))

(cl-defun helm-project-buffer-buffer-root (_buffer)
  (cl-letf* ((file (buffer-file-name _buffer))
             (backend (vc-backend file)))
    (pcase backend
      (`Git (vc-git-root file))
      (`SVN (vc-svn-root file))
      (`Hg (vc-hg-root file))
      (`Bzr (vc-bzr-root file)))))

(cl-defun helm-project-buffer-buffer-git-branch (_buffer)
  (cl-letf* ((file (buffer-file-name _buffer))
             (backend (vc-backend file)))
    (if (file-exists-p file)
        (pcase backend
          (`Git (helm-aif (with-current-buffer
                              _buffer (car (vc-git-branches)))
                    it ""))
          (_ ""))
      "")))

(cl-defun helm-project-buffer-find-buffer-root-and-backend (_buffers)
  (seq-uniq
   (seq-remove #'null
               (seq-map
                #'helm-project-buffer-buffer-root-and-backend
                _buffers))
   (lambda (rb1 rb2) (cl-equalp (cdr rb1) (cdr rb2)))))

(cl-defun helm-project-buffer-source-buffers-alist (_vc-buffers _rb-buffers)
  (seq-map
   (lambda (rb)
     (list :root (cdr rb)
           :backend (car rb)
           :branch (helm-project-buffer-buffer-git-branch
                    (cl-find-if
                     (lambda (b)
                       (and
                        (file-exists-p (buffer-file-name b))
                        (cl-equalp (cdr rb)
                                   (helm-project-buffer-buffer-root b))))
                     _vc-buffers))
           :buffers (seq-filter
                     (lambda (b)
                       (cl-equalp (cdr rb)
                                  (helm-project-buffer-buffer-root b)))
                     _vc-buffers)))
   _rb-buffers))

(cl-defun helm-project-buffer-create-vc-buffer-source (_buffers)
  (cl-letf* ((vc-buffers (seq-filter #'helm-project-buffer-buffer-registerd
                                     _buffers))
             (buffer-root-and-backend (helm-project-buffer-find-buffer-root-and-backend
                                       vc-buffers))
             (source-buffers-alist
              (helm-project-buffer-source-buffers-alist vc-buffers buffer-root-and-backend)))
    (seq-map
     (lambda (l)
       (cl-letf ((buffers (seq-map
                           (lambda (b) (cons (buffer-name b) b))
                           (cl-getf l :buffers))))
         `((name . ,(format "%s%s: %s"
                            (cl-getf l :backend)
                            (if (string-blank-p (cl-getf l :branch))
                                ""
                              (format "@%s" (cl-getf l :branch)))
                            (cl-getf l :root)))
           (candidates . ,buffers)
           (action . ,(helm-project-buffer-actions))
           (candidate-transformer
            helm-project-buffer-transformer-format-buffer))))
     source-buffers-alist)))

(cl-defun helm-project-buffer-create-other-buffer-source (_buffers)
  (cl-letf* ((other-buffers (seq-remove #'helm-project-buffer-buffer-backend _buffers))
             (buffers (seq-map
                       (lambda (b) (cons (buffer-name b) b))
                       other-buffers)))
    `((name . "Other")
      (candidates . ,buffers)
      (action . ,(helm-project-buffer-actions))
      (candidate-transformer
       helm-project-buffer-transformer-skip-boring-buffers
       helm-project-buffer-transformer-format-other-buffer))))

(cl-defun helm-project-buffer-create-source (_buffers)
  (append
   (helm-project-buffer-create-vc-buffer-source _buffers)
   (list (helm-project-buffer-create-other-buffer-source _buffers))))

(cl-defun helm-project-buffer-actions ()
  (helm-make-actions
   "Open buffer"  'helm-project-buffer-action-open-buffer))

(cl-defun helm-project-buffer-action-open-buffer (candidate)
  (switch-to-buffer candidate))

(cl-defun helm-project-buffer-transformer-skip-boring-buffers (candidates)
  (helm-project-buffer-skip-entries candidates helm-boring-buffer-regexp-list))

(cl-defun helm-project-buffer-skip-entries (seq regexp-list)
  "Remove entries which matches one of REGEXP-LIST from SEQ."
  (cl-loop for i in seq
     unless (cl-loop for regexp in regexp-list
               thereis (and (stringp (car i))
                            (string-match regexp (car i))))
     collect i))

(cl-defun helm-project-buffer-longest-string-width (strings)
  (length
   (seq-reduce
    (lambda (a b) (if (> (length a) (length b))
                      a b))
    strings
    "")))

(cl-defun helm-project-buffer-highlight-buffer-name (buffer)
  (cl-letf* ((mode (with-current-buffer buffer
                     (format-mode-line mode-name)))
             (buf (get-buffer buffer))
             (size (propertize (helm-buffer-size buf)
                               'face 'helm-buffer-size))
             (proc (get-buffer-process buf))
             (dir (with-current-buffer buffer (expand-file-name default-directory)))
             (file-name (helm-aif (buffer-file-name buf) (expand-file-name it)))
             (name (buffer-name buf))
             (name-prefix (when (file-remote-p dir)
                            (propertize "@ " 'face 'helm-ff-prefix))))
    (cl-labels ((prop (face) (propertize name 'face face)))
      (cond
        ( ;; A dired buffer.
         (rassoc buf dired-buffers)
         (prop 'helm-buffer-directory))
        ;; A buffer file modified somewhere outside of emacs.=>red
        ((and file-name (file-exists-p file-name)
              (not (verify-visited-file-modtime buf)))
         (prop 'helm-buffer-saved-out))
        ;; A new buffer file not already saved on disk.=>indianred2
        ((and file-name (not (verify-visited-file-modtime buf)))
         (prop 'helm-buffer-not-saved))
        ;; A buffer file modified and not saved on disk.=>orange
        ((and file-name (buffer-modified-p buf))
         (prop 'helm-ff-symlink))
        ;; A buffer file not modified and saved on disk.=>green
        (file-name
         (prop 'font-lock-type-face))
        ;; Any non--file buffer.=>grey italic
        (t
         (prop 'italic))))))

(cl-defun helm-project-buffer-pad-right (elem len)
  (cl-letf ((offset 1))
    (pcase (length elem)
      ((pred (< len))
       (seq-concatenate 'string
                        elem (make-string offset ?\s)))
      (_
       (seq-concatenate 'string
                        elem (make-string (+ (- len (length elem)) offset) ?\s))))))

(cl-defun helm-project-buffer-pad-left (elem len)
  (cl-letf ((offset 1))
    (pcase (length elem)
      ((pred (< len))
       (seq-concatenate 'string
                        (make-string offset ?\s)
                        elem))
      (_
       (seq-concatenate 'string
                        (make-string (+ (- len (length elem)) offset) ?\s)
                        elem)))))

(cl-defun helm-project-buffer-format-name (name len)
  (pcase (length name)
    ((pred (< len))
     (seq-concatenate 'string name (make-string 2 ?\s)))
    (_
     (seq-concatenate 'string name (make-string (+ (- len (length name)) 2)
                                                ?\s)))))

(cl-defun helm-project-buffer-format-mode (_buffer)
  (cl-letf ((mode (with-current-buffer _buffer (format-mode-line mode-name))))
    mode))

(cl-defun helm-project-buffer-format-state (_buffer)
  (cl-letf ((state (vc-state (buffer-file-name _buffer))))
    (cl-labels ((prop (face) (propertize (helm-stringify state) 'face face)))
      (pcase state
        (`edited (prop 'font-lock-builtin-face))
        (`up-to-date (prop 'font-lock-variable-name-face))
        (`needs-update (prop 'font-lock-comment-delimiter-face))
        (`needs-merge (prop 'font-lock-constant-face))
        (`unlocked-changes (prop 'font-lock-doc-face))
        (`added (prop 'font-lock-function-name-face))
        (`removed (prop 'font-lock-keyword-face))
        (`conflict (prop 'font-lock-negation-char-face))
        (`missing (prop 'font-lock-builtin-face))
        (`ignored (prop 'font-lock-builtin-face))
        (`unregistered (prop 'font-lock-builtin-face))
        (_ state)))))

(cl-defun helm-project-buffer-format-file-name (_buffer)
  (cl-letf ((filename (buffer-file-name _buffer)))
    (helm-aif filename
        (propertize (abbreviate-file-name it)
                    'face 'helm-buffer-process)
      "")))

(cl-defun helm-project-buffer-format-directory (_buffer)
  (cl-letf ((dir (with-current-buffer _buffer default-directory)))
    (helm-aif dir
        (propertize (abbreviate-file-name it)
                    'face 'helm-buffer-process)
      "")))

(cl-defun helm-project-buffer-transformer-format-buffer (_candidates)
  (cl-letf ((longest-buffer-width (helm-project-buffer-longest-string-width
                                   (seq-map #'car _candidates)))
            (longest-mode-width (helm-project-buffer-longest-string-width
                                 (seq-map
                                  (lambda (b) (helm-project-buffer-format-mode
                                          (cdr b)))
                                  _candidates))))
    (seq-map
     (lambda (b)
       (cl-letf ((buffer (cdr b)))
         (cons (format "%s%s  %s  %s"
                       (helm-project-buffer-format-name
                        (helm-project-buffer-highlight-buffer-name
                         buffer)
                        longest-buffer-width)
                       (helm-project-buffer-pad-left
                        (helm-project-buffer-format-mode buffer)
                        longest-mode-width)
                       (helm-project-buffer-format-state buffer)
                       (helm-project-buffer-format-file-name buffer))
               buffer)))
     _candidates)))

(cl-defun helm-project-buffer-transformer-format-other-buffer (_candidates)
  (cl-letf ((longest-buffer-width (helm-project-buffer-longest-string-width
                                   (seq-map #'car _candidates)))
            (longest-mode-width (helm-project-buffer-longest-string-width
                                 (seq-map
                                  (lambda (b) (helm-project-buffer-format-mode
                                               (cdr b)))
                                  _candidates))))
    (seq-map
     (lambda (b)
       (cl-letf ((buffer (cdr b)))
         (cons (format "%s%s  %s"
                       (helm-project-buffer-format-name
                        (helm-project-buffer-highlight-buffer-name
                         buffer)
                        longest-buffer-width)
                       (helm-project-buffer-pad-left
                        (helm-project-buffer-format-mode buffer)
                        longest-mode-width)
                       (helm-project-buffer-format-directory buffer))
               buffer)))
     _candidates)))

;;;###autoload
(cl-defun helm-project-buffer ()
  (interactive)
  (cl-letf* ((buffers (buffer-list))
             (sources (helm-project-buffer-create-source buffers)))
    (helm :sources sources
          :buffer "*helm project buffers*")))

(provide 'helm-project-buffer)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
