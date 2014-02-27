;;; helm-project-buffer.el -*- lexical-binding: t -*-

(eval-when-compile (require 'cl-lib))
(require 'helm)

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
  (cl-letf* ((file (buffer-file-name _buffer))
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
           )))
     source-buffers-alist)))

(cl-defun helm-project-buffer-create-other-buffer-source (_buffers)
  (cl-letf* ((other-buffers (cl-remove-if 'helm-project-buffer-buffer-backend _buffers))
             (buffers (cl-mapcar
                       (lambda (b) (cons (buffer-name b) b))
                       other-buffers)))
    `((name . "Other")
      (candidates . ,buffers)
      (action . (("Open buffer" . helm-project-buffer-action-open-buffer)))
      (filtered-candidate-transformer helm-project-buffer-skip-boring-buffers
                                      ;; helm-highlight-buffers
                                      ))))

(cl-defun helm-project-buffer-create-source (_buffers)
  (append
   (helm-project-buffer-create-vc-buffer-source _buffers)
   (list (helm-project-buffer-create-other-buffer-source _buffers))))

(cl-defun helm-project-buffer-action-open-buffer (candidate)
  (switch-to-buffer candidate))

(defun helm-project-buffer-skip-boring-buffers (buffers _source)
  (helm-project-buffer-skip-entries buffers helm-boring-buffer-regexp-list))

(defun helm-project-buffer-skip-entries (seq regexp-list)
  "Remove entries which matches one of REGEXP-LIST from SEQ."
  (cl-loop for i in seq
           unless (cl-loop for regexp in regexp-list
                           thereis (and (stringp (car i))
                                        (string-match regexp (car i))))
           collect i))

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
