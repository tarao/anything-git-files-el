;;; anything-git-files.el --- anything for git files

;; Author: INA Lintaro <tarao.gnn at gmail.com>
;; Version: 0.1
;; Keywords: anything, git

;; This file is NOT part of GNU Emacs.

;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'vc-git)
(require 'anything-config)

(defconst anything-git-files:ls-args
  '((modified . ("--modified"))
    (untracked . ("--others" "--exclude-standard"))
    (all . nil)))

(defconst anything-git-files:status-expire 1)

(defsubst anything-git-files:chomp (str)
  (replace-regexp-in-string "[\r\n]+$" "" str))

(defsubst anything-git-files:hash (obj)
  (secure-hash 'sha1 obj))

(defun anything-git-files:command-to-string (&rest args)
  (with-output-to-string
    (with-current-buffer standard-output
      (apply 'vc-git-command (current-buffer) 0 nil args))))

(defmacro anything-git-files:define-rev-parse (name arg)
  (let* ((fun (intern (format "anything-git-files:%s" name)))
         (fun1 (intern (format "%s-1" fun)))
         (prop (intern (format "git-%s" name))))
    `(progn
       (defun ,fun1 ()
         (file-name-as-directory
          (anything-git-files:chomp
           (anything-git-files:command-to-string "rev-parse" ,arg))))
       (defun ,fun ()
         (or (vc-file-getprop default-directory ',prop)
             (vc-file-setprop default-directory ',prop (,fun1)))))))
(anything-git-files:define-rev-parse root "--show-toplevel")
(anything-git-files:define-rev-parse root-relative "--show-cdup")

(defun anything-git-files:ls (buffer &rest args)
  (let ((rel (anything-git-files:root-relative)))
    (apply 'vc-git-command buffer 0 rel "ls-files" args)))

(defun anything-git-files:status-1 ()
  (anything-git-files:command-to-string "status" "--porcelain"))

(defun anything-git-files:status-hash (&optional root)
  "Get hash value of \"git status\" for ROOT repository.
The status and its hash value will be reused until
`anything-git-files:status-expire' seconds after the last time
they have been updated."
  (let* ((default-directory (or root default-directory))
         (prop 'anything-git-files:status-hash)
         (info (vc-file-getprop default-directory prop))
         (last (plist-get info :last))
         (now (float-time)))
    (when (or (not (numberp last))
              (> now (+ anything-git-files:status-expire last)))
      (let ((hash (secure-hash 'sha1 (anything-git-files:status-1))))
        (setq info (plist-put info :hash hash))))
    (setq info (plist-put info :last now))
    (vc-file-setprop default-directory prop info)
    (plist-get info :hash)))

(defun anything-git-files:updated-p (root &optional key)
  "Check if the status hash value for ROOT repository is updated.
Update states are tracked for each KEY separately."
  (let* ((key (or (and key (format "-%s" key)) ""))
         (prop (intern (format "anything-git-files:last-status%s" key)))
         (last-status (vc-file-getprop root prop))
         (status (anything-git-files:status-hash root)))
    (unless (and last-status (string= status last-status))
      (vc-file-setprop root prop status)
      t)))

(defun anything-git-files:init-fun (what)
  `(lambda ()
     (let* ((root (anything-git-files:root))
            (buffer-name (format "*anything candidates:%s:%s*" root ',what))
            (buffer (get-buffer-create buffer-name)))
       (anything-attrset 'default-directory root) ; saved for `display-to-real'
       (anything-candidate-buffer buffer)
       (when (anything-git-files:updated-p root ',what)
         (let ((args (cdr (assq ',what anything-git-files:ls-args))))
           (apply 'anything-git-files:ls buffer "--full-name" args))))))

(defun anything-git-files:display-to-real (name)
  (expand-file-name name (anything-attr 'default-directory)))

(defun anything-git-files:source (name what)
  `((name . ,name)
    (init . ,(anything-git-files:init-fun what))
    (candidates-in-buffer)
    (type . file)
    (display-to-real . anything-git-files:display-to-real)))

;;;###autoload
(defun anything-git-files:git-p (&optional root)
  (ignore-errors (anything-git-files:status-hash root)))

;;;###autoload
(defvar anything-git-files:modified-source
  (anything-git-files:source "Modified files" 'modified))

;;;###autoload
(defvar anything-git-files:untracked-source
  (anything-git-files:source "Untracked files" 'untracked))

;;;###autoload
(defvar anything-git-files:all-source
  (anything-git-files:source "All files" 'all))

;;;###autoload
(defun anything-git-files ()
  "`anything' for opening files managed by Git."
  (interactive)
  (anything-other-buffer '(anything-git-files:modified-source
                           anything-git-files:untracked-source
                           anything-git-files:all-source)
                         "*anything for git files*"))

(provide 'anything-git-files)
;;; anything-git-files.el ends here
