;;; helm-project-buffer.el -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'helm)
(require 'helm-buffers)
(require 'helm-utils)

(require 'colle)
(require 'glof)

(cl-defun helm-project-buffer-buffer-backend (buffer)
  (vc-backend (buffer-file-name buffer)))

(cl-defun helm-project-buffer-buffer-registerd (buffer)
  (helm-aif (buffer-file-name buffer)
      (if (and (not (tramp-tramp-file-p it))
             (file-exists-p it))
          (vc-registered it)
        nil)
    nil))

(cl-defun helm-project-buffer-buffer-root-and-backend (buffer)
  (cons (helm-project-buffer-buffer-backend buffer)
        (helm-project-buffer-buffer-root buffer)))

(cl-defun helm-project-buffer-buffer-backend (buffer)
  (cl-letf* ((file (buffer-file-name buffer))
             (backend (vc-backend file)))
    backend))

(cl-defun helm-project-buffer-buffer-root (buffer)
  (cl-letf* ((file (buffer-file-name buffer))
             (backend (vc-backend file)))
    (pcase backend
      (`Git (vc-git-root file))
      (`SVN (vc-svn-root file))
      (`Hg (vc-hg-root file))
      (`Bzr (vc-bzr-root file)))))

(cl-defun helm-project-buffer-buffer-git-branch (buffer)
  (cl-letf* ((file (buffer-file-name buffer))
             (backend (vc-backend file)))
    (if (file-exists-p file)
        (pcase backend
          (`Git (helm-aif (with-current-buffer
                              buffer (colle:first (vc-git-branches)))
                    it ""))
          (_ ""))
      "")))

(cl-defun helm-project-buffer-find-buffer-root-and-backend (buffers)
  (seq-uniq
   (colle:remove #'null
              (colle:map
               #'helm-project-buffer-buffer-root-and-backend
               buffers))
   (lambda (rb1 rb2) (cl-equalp (colle:rest rb1) (colle:rest rb2)))))

(cl-defun helm-project-buffer-source-buffers-plist (vc-buffers rb-buffers)
  (colle:map
   (lambda (rb)
     (list :root (colle:rest rb)
           :backend (colle:first rb)
           :branch (helm-project-buffer-buffer-git-branch
                    (cl-find-if
                     (lambda (b)
                       (and
                        (file-exists-p (buffer-file-name b))
                        (cl-equalp (colle:rest rb)
                                   (helm-project-buffer-buffer-root b))))
                     vc-buffers))
           :buffers (colle:filter
                     (lambda (b)
                       (cl-equalp (colle:rest rb)
                                  (helm-project-buffer-buffer-root b)))
                     vc-buffers)))
   rb-buffers))

(defclass helm-source-project-buffer-vc-buffer (helm-source-sync)
  ())

(cl-defun helm-project-buffer-create-vc-buffer-source (buffers)
  (cl-letf* ((vc-buffers (colle:filter #'helm-project-buffer-buffer-registerd
                                    buffers))
             (buffer-root-and-backend (helm-project-buffer-find-buffer-root-and-backend
                                       vc-buffers))
             (source-buffers-plist
              (helm-project-buffer-source-buffers-plist vc-buffers buffer-root-and-backend)))
    (colle:map
     (lambda (l)
       (cl-letf ((buffers (colle:map
                           (lambda (b) (cons (buffer-name b) b))
                           (glof:get l :buffers)))
                 (name (format "%s%s: %s"
                               (glof:get l :backend)
                               (if (string-blank-p (glof:get l :branch))
                                   ""
                                 (format "@%s" (glof:get l :branch)))
                               (glof:get l :root))))
         (helm-make-source name 'helm-source-project-buffer-vc-buffer
           :candidates buffers
           :action (helm-project-buffer-actions)
           :candidate-transformer
           'helm-project-buffer-transformer-format-buffer)))
     source-buffers-plist)))

(defclass helm-source-project-buffer (helm-source-sync)
  ())

(cl-defun helm-project-buffer-create-other-buffer-source (buffers)
  (cl-letf* ((other-buffers (colle:remove #'helm-project-buffer-buffer-backend buffers))
             (buffers (colle:map
                       (lambda (b) (cons (buffer-name b) b))
                       other-buffers)))
    (helm-make-source "Buffers" 'helm-source-project-buffer
      :candidates buffers
      :action (helm-project-buffer-actions)
      :candidate-transformer
      '(helm-project-buffer-transformer-skip-boring-buffers
        helm-project-buffer-transformer-format-other-buffer))))

(cl-defun helm-project-buffer-create-source (buffers)
  (append
   (helm-project-buffer-create-vc-buffer-source buffers)
   (list (helm-project-buffer-create-other-buffer-source buffers))))

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
                           thereis (and (stringp (colle:first i))
                                      (string-match-p regexp (colle:first i))))
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

(cl-defun helm-project-buffer-format-mode (buffer)
  (cl-letf ((mode (with-current-buffer buffer (format-mode-line mode-name))))
    mode))

(cl-defun helm-project-buffer-format-state (buffer)
  (cl-letf ((state (vc-state (buffer-file-name buffer))))
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

(cl-defun helm-project-buffer-format-file-name (buffer)
  (cl-letf ((filename (buffer-file-name buffer)))
    (helm-aif filename
        (propertize (abbreviate-file-name it)
                    'face 'helm-buffer-process)
      "")))

(cl-defun helm-project-buffer-format-directory (buffer)
  (cl-letf ((dir (with-current-buffer buffer default-directory)))
    (helm-aif dir
        (propertize (abbreviate-file-name it)
                    'face 'helm-buffer-process)
      "")))

(cl-defun helm-project-buffer-transformer-format-buffer (candidates)
  (cl-letf ((longest-buffer-width (helm-project-buffer-longest-string-width
                                   (colle:map #'colle:first candidates)))
            (longest-mode-width (helm-project-buffer-longest-string-width
                                 (colle:map
                                  (pcase-lambda (`(,_ . ,b))
                                    (helm-project-buffer-format-mode b))
                                  candidates))))
    (colle:map
     (pcase-lambda (`(,_ . ,buffer))
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
             buffer))
     candidates)))

(cl-defun helm-project-buffer-transformer-format-other-buffer (candidates)
  (cl-letf ((longest-buffer-width (helm-project-buffer-longest-string-width
                                   (colle:map #'colle:first candidates)))
            (longest-mode-width (helm-project-buffer-longest-string-width
                                 (colle:map
                                  (pcase-lambda (`(,_ . ,b))
                                    (helm-project-buffer-format-mode b))
                                  candidates))))
    (colle:map
     (pcase-lambda (`(,_ . ,buffer))
       (cons (format "%s%s  %s"
                     (helm-project-buffer-format-name
                      (helm-project-buffer-highlight-buffer-name
                       buffer)
                      longest-buffer-width)
                     (helm-project-buffer-pad-left
                      (helm-project-buffer-format-mode buffer)
                      longest-mode-width)
                     (helm-project-buffer-format-directory buffer))
             buffer))
     candidates)))

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
