;;; helm-project-buffer.el -*- lexical-binding: t -*-

(eval-when-compile (require 'cl-lib))
(require 'helm)
(require 'helm-buffers)

(cl-defun helm-project-buffer-buffer-backend (_buffer)
  (vc-backend (buffer-file-name _buffer)))

(cl-defun helm-project-buffer-buffer-registerd (_buffer)
  (cl-letf ((file (buffer-file-name _buffer)))
    (if file
        (vc-registered file)
      nil)))

(cl-defun helm-project-buffer-buffer-root-and-backend (_buffer)
  (cl-letf* ((file (buffer-file-name _buffer))
             (backend (vc-backend file)))
    (cl-case backend
      (Git (cons backend (vc-git-root file)))
      (SVN (cons backend (vc-svn-root file)))
      (Hg (cons backend (vc-hg-root file))))))

(cl-defun helm-project-buffer-buffer-root (_buffer)
  (cl-letf* ((file (abbreviate-file-name (buffer-file-name _buffer)))
             (backend (vc-backend file)))
    (cl-case backend
      (Git (vc-git-root file))
      (SVN (vc-svn-root file))
      (Hg (vc-hg-root file)))))

(cl-defun helm-project-buffer-find-buffer-root-and-backend (_buffers)
  (cl-remove-duplicates
   (cl-remove nil
              (cl-mapcar
               'helm-project-buffer-buffer-root-and-backend
               _buffers))
   :test (lambda (rb1 rb2) (cl-equalp (cdr rb1) (cdr rb2)))))

(cl-defun helm-project-buffer-create-vc-buffer-source (_buffers)
  (cl-letf* ((vc-buffers (cl-remove-if-not 'helm-project-buffer-buffer-registerd
                                           _buffers))
             (buffer-root-and-backend (helm-project-buffer-find-buffer-root-and-backend
                                       vc-buffers))
             (source-buffers-alist
              (cl-mapcar
               (lambda (rb)
                 (list :root (cdr rb)
                       :backend (car rb)
                       :buffers (cl-remove-if-not
                                 (lambda (b)
                                   (cl-equalp (cdr rb)
                                              (helm-project-buffer-buffer-root b)))
                                 vc-buffers)))
               buffer-root-and-backend)))
    (cl-mapcar
     (lambda (l)
       (cl-letf ((buffers (cl-mapcar
                           (lambda (b) (cons (buffer-name b) b))
                           (plist-get l :buffers))))
         `((name .       ,(format "%s: %s" (plist-get l :backend) (plist-get l :root)))
           (candidates . ,buffers)
           (action . (("Open buffer" . helm-project-buffer-action-open-buffer)))
           (filtered-candidate-transformer
            helm-project-buffer-transformer-format-buffer))))
     source-buffers-alist)))

(cl-defun helm-project-buffer-create-other-buffer-source (_buffers)
  (cl-letf* ((other-buffers (cl-remove-if 'helm-project-buffer-buffer-backend _buffers))
             (buffers (cl-mapcar
                       (lambda (b) (cons (buffer-name b) b))
                       other-buffers)))
    `((name . "Other")
      (candidates . ,buffers)
      (action . (("Open buffer" . helm-project-buffer-action-open-buffer)))
      (filtered-candidate-transformer
       helm-project-buffer-transformer-skip-boring-buffers
       ))))

(cl-defun helm-project-buffer-create-source (_buffers)
  (append
   (helm-project-buffer-create-vc-buffer-source _buffers)
   (list (helm-project-buffer-create-other-buffer-source _buffers))))

(cl-defun helm-project-buffer-action-open-buffer (candidate)
  (switch-to-buffer candidate))

(cl-defun helm-project-buffer-transformer-skip-boring-buffers (candidates _source)
  (helm-project-buffer-skip-entries candidates helm-boring-buffer-regexp-list))

(cl-defun helm-project-buffer-skip-entries (seq regexp-list)
  "Remove entries which matches one of REGEXP-LIST from SEQ."
  (cl-loop for i in seq
           unless (cl-loop for regexp in regexp-list
                           thereis (and (stringp (car i))
                                        (string-match regexp (car i))))
           collect i))

(cl-defun helm-project-buffer-longeth-string-width (strings)
  (length
   (cl-reduce
    (lambda (a b) (if (> (length a) (length b))
                      a b))
    strings)))

(defun helm-project-buffer-highlight-buffer-name (buffer)
  (let* ((mode (with-current-buffer buffer (symbol-name major-mode)))
         (buf (get-buffer buffer))
         (size (propertize (helm-buffer-size buf)
                           'face 'helm-buffer-size))
         (proc (get-buffer-process buf))
         (dir (with-current-buffer buffer (abbreviate-file-name default-directory)))
         (file-name (helm-aif (buffer-file-name buf) (abbreviate-file-name it)))
         (name (buffer-name buf))
         (name-prefix (when (file-remote-p dir)
                        (propertize "@ " 'face 'helm-ff-prefix))))
    (cond
     ( ;; A dired buffer.
      (rassoc buf dired-buffers)
      (propertize name 'face 'helm-ff-directory))
     ;; A buffer file modified somewhere outside of emacs.=>red
     ((and file-name (file-exists-p file-name)
           (not (verify-visited-file-modtime buf)))
      (propertize name 'face 'helm-buffer-saved-out))
     ;; A new buffer file not already saved on disk.=>indianred2
     ((and file-name (not (verify-visited-file-modtime buf)))
      (propertize name 'face 'helm-buffer-not-saved))
     ;; A buffer file modified and not saved on disk.=>orange
     ((and file-name (buffer-modified-p buf))
      (propertize name 'face 'helm-ff-symlink))
     ;; A buffer file not modified and saved on disk.=>green
     (file-name
      (propertize name 'face 'font-lock-type-face))
     ;; Any non--file buffer.=>grey italic
     (t
      (propertize name 'face 'italic)))))

(cl-defun helm-project-buffer-format-name (_name _length)
  (if (< _length (length _name))
      (cl-concatenate 'string _name (make-string 2 ?\ ))
    (cl-concatenate 'string _name (make-string (+ (- _length (length _name)) 2)
                                               ?\ ))))

(cl-defun helm-project-buffer-format-mode (_buffer)
  (cl-letf ((mode (with-current-buffer _buffer major-mode)))
    mode))

(cl-defun helm-project-buffer-format-state (_buffer)
  (cl-letf ((state (vc-state (buffer-file-name _buffer))))
    (cl-flet ((prop (face) (propertize (symbol-name state) 'face face)))
      (cl-case state
        (edited (prop 'font-lock-builtin-face))
        (up-to-date (prop 'font-lock-reference-face))
        (needs-update (prop 'font-lock-comment-delimiter-face))
        (needs-merge (prop 'font-lock-constant-face))
        (unlocked-changes (prop 'font-lock-doc-face))
        (added (prop 'font-lock-function-name-face))
        (removed (prop 'font-lock-keyword-face))
        (conflict (prop 'font-lock-negation-char-face))
        (missing (prop 'font-lock-builtin-face))
        (ignored (prop 'font-lock-builtin-face))
        (unregistered (prop 'font-lock-builtin-face))
        (t state)))))

(cl-defun helm-project-buffer-transformer-format-buffer (_candidates source)
  (cl-letf ((longest-buffer-width (helm-project-buffer-longeth-string-width
                                   (cl-mapcar 'car _candidates))))
    (cl-mapcar
     (lambda (b)
       (cons (format "%s%s %s"
                     (helm-project-buffer-format-name
                      (helm-project-buffer-highlight-buffer-name
                       (cdr b))
                      longest-buffer-width)
                     (helm-project-buffer-format-mode (cdr b))
                     (helm-project-buffer-format-state (cdr b)))
             (cdr b)))
     _candidates)))

;;;###autoload
(cl-defun helm-project-buffer ()
  (interactive)
  (cl-letf* ((buffers (buffer-list))
             (sources (helm-project-buffer-create-source buffers)))
    (helm :sources sources
          :buffer "*helm project buffers")))

(provide 'helm-project-buffer)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
