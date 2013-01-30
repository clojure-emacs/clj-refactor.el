;;; clj-refactor.el --- A collection of clojure refactoring functions

;; Copyright (C) 2012 Magnar Sveen <magnars@gmail.com>

;; Author: Magnar Sveen <magnars@gmail.com>
;; Version: 0.2.0
;; Keywords: convenience
;; Package-Requires: ((s "1.3.1") (dash "1.0.3") (yasnippet "0.6.1"))

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

;; ## Installation
;;
;; I highly recommended installing clj-refactor through elpa.
;;
;; It's available on [marmalade](http://marmalade-repo.org/):
;;
;;     M-x package-install clj-refactor
;;
;; You can also install the dependencies on your own, and just dump
;; clj-refactor in your path somewhere:
;;
;;  - <a href="https://github.com/magnars/s.el">s.el</a>
;;  - <a href="https://github.com/magnars/dash.el">dash.el</a>
;;

;; ## Setup
;;
;;     (require 'clj-refactor)
;;     (add-hook 'clojure-mode-hook (lambda ()
;;                                    (clj-refactor-mode 1)
;;                                    ;; insert keybinding setup here
;;                                    ))
;;
;; You'll also have to set up the keybindings in the lambda. Read on.

;; ## Setup keybindings
;;
;; All functions in clj-refactor have a two-letter mnemonic shortcut. You
;; get to choose how those are bound. Here's how:
;;
;;     (cljr-add-keybindings-with-prefix "C-c C-m")
;;     ;; eg. rename files with `C-c C-m rf`.
;;
;; If you would rather have a modifier key, instead of a prefix, do:
;;
;;     (cljr-add-keybindings-with-modifier "C-s-")
;;     ;; eg. rename files with `C-s-r C-s-f`.
;;
;; If neither of these appeal to your sense of keyboard layout aesthetics, feel free
;; to pick and choose your own keybindings with a smattering of:
;;
;;     (define-key clj-refactor-map (kbd "C-x C-r") 'cljr-rename-file)

;; ## Use
;;
;; This is it so far:
;;
;;  - `rf`: rename file, update ns-declaration, and then query-replace new ns in project.
;;  - `ar`: add :require to namespace declaration
;;  - `au`: add :use to namespace declaration
;;
;; Combine with your keybinding prefix/modifier.

;;; Code:

(require 'dash)
(require 's)
(require 'yasnippet)

(defvar clj-refactor-map (make-sparse-keymap) "")

(defun cljr--key-pairs-with-modifier (modifier keys)
  (->> (string-to-list keys)
    (--map (concat modifier (char-to-string it)))
    (s-join " ")
    (read-kbd-macro)))

(defun cljr--key-pairs-with-prefix (prefix keys)
  (read-kbd-macro (concat prefix " " keys)))

(defun cljr--add-keybindings (key-fn)
  (define-key clj-refactor-map (funcall key-fn "rf") 'cljr-rename-file)
  (define-key clj-refactor-map (funcall key-fn "au") 'cljr-add-use-to-ns)
  (define-key clj-refactor-map (funcall key-fn "ar") 'cljr-add-require-to-ns))

;;;###autoload
(defun cljr-add-keybindings-with-prefix (prefix)
  (cljr--add-keybindings (-partial 'cljr--key-pairs-with-prefix prefix)))

;;;###autoload
(defun cljr-add-keybindings-with-modifier (modifier)
  (cljr--add-keybindings (-partial 'cljr--key-pairs-with-modifier modifier)))

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
  (let ((old-ns (clojure-find-ns)))
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

(defun cljr--goto-ns ()
  (goto-char (point-min))
  (unless (re-search-forward clojure-namespace-name-regex nil t)
    (error "No namespace declaration found")))

;;;###autoload
(defun cljr-add-require-to-ns ()
  (interactive)
  (push-mark)
  (cljr--goto-ns)
  (newline-and-indent)
  (cljr--pop-mark-after-yasnippet)
  (yas/expand-snippet "(:require [$1 :as $2])$0"))

;;;###autoload
(defun cljr-add-use-to-ns ()
  (interactive)
  (push-mark)
  (cljr--goto-ns)
  (newline-and-indent)
  (cljr--pop-mark-after-yasnippet)
  (yas/expand-snippet "(:use ${1:[$2 :only ($3)]})$0"))

(defun cljr--pop-mark-after-yasnippet ()
  (add-hook 'yas/after-exit-snippet-hook 'cljr--pop-mark-after-yasnippet-1 nil t))

(defun cljr--pop-mark-after-yasnippet-1 (&rest ignore)
  (pop-to-mark-command)
  (remove-hook 'yas/after-exit-snippet-hook 'cljr--pop-mark-after-yasnippet-1 t))

;;;###autoload
(define-minor-mode clj-refactor-mode
  "A mode to keep the clj-refactor keybindings."
  nil " cljr" clj-refactor-map)

(provide 'clj-refactor)
;;; clj-refactor.el ends here
