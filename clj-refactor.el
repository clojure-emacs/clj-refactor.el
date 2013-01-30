;;; clj-refactor.el --- A collection of clojure refactoring functions

;; Copyright (C) 2012 Magnar Sveen <magnars@gmail.com>

;; Author: Magnar Sveen <magnars@gmail.com>
;; Version: 0.1.0
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'dash)
(require 's)

(defvar clj-refactor-map (make-sparse-keymap) "")

(define-key clj-refactor-map (kbd "C-x C-r") 'cljr-rename-file)

(defun cljr--project-dir ()
  (file-truename
   (locate-dominating-file default-directory "project.clj")))

(defun cljr--project-files ()
  (split-string (shell-command-to-string
                 (format "find %s -type f \\( %s \\) %s | head -n %s"
                         (cljr--project-dir)
                         (format "-name \"%s\"" "*.clj")
                         "-not -regex \".*svn.*\""
                         1000))))

(defun cljr--rename-file (filename new-name)
  (let ((old-ns (clojure-expected-ns)))
    (rename-file filename new-name 1)
    (rename-buffer new-name)
    (set-visited-file-name new-name)
    (clojure-update-ns)
    (save-window-excursion
      (save-excursion
        (ignore-errors
          (tags-query-replace old-ns (clojure-expected-ns) nil
                              '(cljr--project-files)))))
    (save-buffer)
    (save-some-buffers)))

;;;###autoload
(defun cljr-rename-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (cljr--rename-file filename new-name)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

;;;###autoload
(define-minor-mode clj-refactor-mode
  "A mode to keep the clj-refactor keybindings."
  nil " cljr" clj-refactor-map)

(provide 'clj-refactor)
;;; clj-refactor.el ends here
