;;; cljr-key-mode.el --- interactively perform refactoring with clj-refactor.el

;; Copyright © 2014 Akhil Wali <akhil.wali.10@gmail.com>

;; Author: Akhil Wali <akhil.wali.10@gmail.com>
;; Version: 0.10.0
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

;; Copyright (C) 2010-2014  The Magit Project Developers
;;
;; For a full list of contributors, see the AUTHORS.md file
;; at the top-level directory of this distribution and at
;; https://raw.github.com/magit/magit/master/AUTHORS.md

;;; Commentary:

;; This library implements `cljr-key-mode' which provides interactive help
;; while refactoring using clj-refactor.el.
;; Based on magit-key-mode.el by Phil Jackson <phil@shellarchive.co.uk>

;;; Code:

(eval-when-compile (require 'cl-lib))

(defvar cljr-key-mode-keymaps)
(defvar cljr-key-mode-last-buffer)
(defvar cljr-pre-key-mode-window-conf)

;;; Options

(defcustom cljr-key-mode-show-usage t
  "Whether to show usage information when entering a popup."
  :group 'cljr
  :type 'boolean)

;;; Faces

(defface cljr-key-mode-header-face
  '((t :inherit font-lock-keyword-face))
  "Face for key mode header lines."
  :group 'cljr-faces)

(defface cljr-key-mode-button-face
  '((t :inherit font-lock-builtin-face))
  "Face for key mode buttons."
  :group 'cljr-faces)

(defface cljr-key-mode-switch-face
  '((t :inherit font-lock-warning-face))
  "Face for key mode switches."
  :group 'cljr-faces)

(defface cljr-key-mode-args-face
  '((t :inherit widget-field))
  "Face for key mode switch arguments."
  :group 'cljr-faces)

;;; Keygroups
;;;###autoload
(defvar cljr-key-mode-groups
  '((dispatch
     (actions
      ("a" "Add"               cljr-key-mode-popup-add)
      ("c" "Cycle"             cljr-key-mode-popup-cycle)
      ("d" "Destructure keys"  cljr-destructure-keys)
      ("e" "Expand let"        cljr-expand-let)
      ("i" "Introduce Let"     cljr-introduce-let)
      ("m" "Move"              cljr-key-mode-popup-move)
      ("n" "Sort ns declaration" cljr-sort-ns)
      ("r" "Rename/Replace"    cljr-key-mode-popup-rename)
      ("s" "Stop referring"    cljr-stop-referring)
      ("t" "Thread"            cljr-key-mode-popup-thread)
      ("u" "Unwind"            cljr-key-mode-popup-unwind)))

    (add
     (actions
      ("d" "Add declaration"   cljr-add-declaration)
      ("i" "Add :import to ns" cljr-add-import-to-ns)
      ("r" "Add :require to ns" cljr-add-require-to-ns)
      ("u" "Add :use to ns"    cljr-add-use-to-ns)))

    (cycle
     (actions
      ("c" "Cycle collection" cljr-cycle-coll)
      ("p" "Cycle definition privacy" cljr-cycle-privacy)
      ("s" "Cycle string/symbol" cljr-cycle-stringlike)))

    (move
     (actions
      ("f" "Move form to file" cljr-move-form)
      ("l" "Move to let" cljr-move-to-let)))

    (rename
     (actions
      ("f" "Rename file and ns" cljr-rename-file)
      ("u" "Replace :use with :refer :all" cljr-replace-use)))

    (thread
     (actions
      ("f" "Thread with ->" cljr-thread-first-all)
      ("l" "Thread with ->>" cljr-thread-last-all)
      ("h" "Thread expression" cljr-thread)))

    (unwind
     (actions
      ("a" "Fully unwind threaded expression" cljr-unwind-all)
      ("w" "Unwind threaded expression" cljr-unwind)))

    ))

(defun cljr-key-mode-delete-group (group)
  "Delete a group from `cljr-key-mode-keymaps'."
  (let ((items (assoc group cljr-key-mode-groups)))
    (when items
      ;; reset the cache
      (setq cljr-key-mode-keymaps nil)
      ;; delete the whole group
      (setq cljr-key-mode-groups
            (delq items cljr-key-mode-groups))
      ;; unbind the defun
      (cljr-key-mode-de-generate group))
    cljr-key-mode-groups))

(defun cljr-key-mode-add-group (group)
  "Add a new group to `cljr-key-mode-keymaps'.
If there already is a group of that name then this will
completely remove it and put in its place an empty one of the
same name."
  (when (assoc group cljr-key-mode-groups)
    (cljr-key-mode-delete-group group))
  (setq cljr-key-mode-groups
        (cons (list group (list 'actions) (list 'switches))
              cljr-key-mode-groups)))

(defun cljr-key-mode-key-defined-p (for-group key)
  "Return t if KEY is defined as any option within FOR-GROUP.
The option may be a switch, argument or action."
  (catch 'result
    (let ((options (cljr-key-mode-options-for-group for-group)))
      (dolist (type '(actions switches arguments))
        (when (assoc key (assoc type options))
          (throw 'result t))))))

(defun cljr-key-mode-update-group (for-group thing &rest args)
  "Abstraction for setting values in `cljr-key-mode-keymaps'."
  (let* ((options (cljr-key-mode-options-for-group for-group))
         (things (assoc thing options))
         (key (car args)))
    (if (cdr things)
        (if (cljr-key-mode-key-defined-p for-group key)
            (error "%s is already defined in the %s group." key for-group)
          (setcdr (cdr things) (cons args (cddr things))))
      (setcdr things (list args)))
    (setq cljr-key-mode-keymaps nil)
    things))

(defun cljr-key-mode-insert-argument (for-group key desc arg read-func)
  "Add a new binding KEY in FOR-GROUP which will use READ-FUNC
to receive input to apply to argument ARG git is run.  DESC should
be a brief description of the binding."
  (cljr-key-mode-update-group for-group 'arguments key desc arg read-func))

(defun cljr-key-mode-insert-switch (for-group key desc switch)
  "Add a new binding KEY in FOR-GROUP which will add SWITCH to git's
command line when it runs.  DESC should be a brief description of
the binding."
  (cljr-key-mode-update-group for-group 'switches key desc switch))

(defun cljr-key-mode-insert-action (for-group key desc func)
  "Add a new binding KEY in FOR-GROUP which will run command FUNC.
DESC should be a brief description of the binding."
  (cljr-key-mode-update-group for-group 'actions key desc func))

(defun cljr-key-mode-options-for-group (for-group)
  "Retrieve the options for the group FOR-GROUP.
This includes switches, commands and arguments."
  (or (cdr (assoc for-group cljr-key-mode-groups))
      (error "Unknown group '%s'" for-group)))

;;; Commands

(defun cljr-key-mode-help (for-group)
  "Provide help for a key within FOR-GROUP.
The user is prompted for the key."
  (let* ((opts (cljr-key-mode-options-for-group for-group))
         (seq (read-key-sequence "Enter command prefix: "))
         (actions (cdr (assoc 'actions opts))))
    (cond
      ;; if it is an action popup the help for the to-be-run function
      ((assoc seq actions) (describe-function (nth 2 (assoc seq actions))))
      (t (error "No help associated with `%s'" seq)))))

(defun cljr-key-mode-exec-at-point ()
  "Run action/args/option at point."
  (interactive)
  (let ((key (or (get-text-property (point) 'key-group-executor)
                 (error "Nothing at point to do."))))
    (call-interactively (lookup-key (current-local-map) key))))

(defun cljr-key-mode-jump-to-next-exec ()
  "Jump to the next action/args/option point."
  (interactive)
  (let* ((oldp (point))
         (old  (get-text-property oldp 'key-group-executor))
         (p    (if (= oldp (point-max)) (point-min) (1+ oldp))))
    (while (let ((new (get-text-property p 'key-group-executor)))
             (and (not (= p oldp)) (or (not new) (eq new old))))
      (setq p (if (= p (point-max)) (point-min) (1+ p))))
    (goto-char p)
    (skip-chars-forward " ")))

;;; Keymaps

(defvar cljr-key-mode-keymaps nil
  "This will be filled lazily with proper keymaps.
These keymaps are created using `define-key' as they're requested.")

(defun cljr-key-mode-build-keymap (for-group)
  "Construct a normal looking keymap for the key mode to use.
Put it in `cljr-key-mode-keymaps' for fast lookup."
  (let* ((options (cljr-key-mode-options-for-group for-group))
         (actions (cdr (assoc 'actions options)))
         (switches (cdr (assoc 'switches options)))
         (arguments (cdr (assoc 'arguments options)))
         (map (make-sparse-keymap)))
    (suppress-keymap map 'nodigits)
    ;; ret dwim
    (define-key map (kbd "RET") 'cljr-key-mode-exec-at-point)
    ;; tab jumps to the next "button"
    (define-key map (kbd "TAB") 'cljr-key-mode-jump-to-next-exec)

    ;; all maps should `quit' with `C-g' or `q'
    (define-key map (kbd "C-g") `(lambda ()
                                   (interactive)
                                   (cljr-key-mode-command nil)))
    (define-key map (kbd "q")   `(lambda ()
                                   (interactive)
                                   (cljr-key-mode-command nil)))
    ;; run help
    (define-key map (kbd "?") `(lambda ()
                                 (interactive)
                                 (cljr-key-mode-help ',for-group)))

    (let ((defkey (lambda (k action)
                    (when (and (lookup-key map (car k))
                               (not (numberp (lookup-key map (car k)))))
                      (message "Warning: overriding binding for `%s' in %S"
                               (car k) for-group)
                      (ding)
                      (sit-for 2))
                    (define-key map (car k)
                      `(lambda () (interactive) ,action)))))
      (dolist (k actions)
        (funcall defkey k `(cljr-key-mode-command ',(nth 2 k))))
      (dolist (k switches)
        (funcall defkey k `(cljr-key-mode-toggle-option ',for-group ,(nth 2 k))))
      (dolist (k arguments)
        (funcall defkey k `(cljr-key-mode-add-argument
                            ',for-group ,(nth 2 k) ',(nth 3 k)))))

    (push (cons for-group map) cljr-key-mode-keymaps)
    map))

;;; Toggling and Running

(defvar cljr-key-mode-prefix nil
  "Prefix argument to the command that brought up the key-mode window.
For internal use.  Used by the command that's eventually invoked.")

(defvar cljr-key-mode-current-args nil
  "A hash-table of current argument set.
These will eventually make it to the git command-line.")

(defvar cljr-key-mode-current-options nil
  "Current option set.
These will eventually make it to the git command-line.")

(defvar cljr-custom-options nil
  "List of custom options to pass to Git.
Do not customize this (used in the `cljr-key-mode' implementation).")

(defun cljr-key-mode-command (func)
  (let ((current-prefix-arg (or current-prefix-arg cljr-key-mode-prefix))
        (cljr-custom-options cljr-key-mode-current-options))
    (maphash (lambda (k v)
               (push (concat k v) cljr-custom-options))
             cljr-key-mode-current-args)
    (set-window-configuration cljr-pre-key-mode-window-conf)
    (kill-buffer cljr-key-mode-last-buffer)
    (when func
      (call-interactively func))))

(defun cljr-key-mode-add-argument (for-group arg-name input-func)
  (let ((input (funcall input-func (concat arg-name ": "))))
    (puthash arg-name input cljr-key-mode-current-args)
    (cljr-key-mode-redraw for-group)))

(defun cljr-key-mode-toggle-option (for-group option-name)
  "Toggles the appearance of OPTION-NAME in `cljr-key-mode-current-options'."
  (if (member option-name cljr-key-mode-current-options)
      (setq cljr-key-mode-current-options
            (delete option-name cljr-key-mode-current-options))
    (add-to-list 'cljr-key-mode-current-options option-name))
  (cljr-key-mode-redraw for-group))

;;; Mode

(defvar cljr-key-mode-buf-name "*cljr-key: %s*"
  "Format string to create the name of the cljr-key buffer.")

(defvar cljr-key-mode-last-buffer nil
  "Store the last cljr-key buffer used.")

(defvar cljr-pre-key-mode-window-conf nil
  "Will hold the pre-menu configuration of cljr.")

(defun cljr-key-mode (for-group &optional original-opts)
  "Mode for cljr key selection.
All commands, switches and options can be toggled/actioned with
the key combination highlighted before the description."
  (interactive)
  ;; save the window config to restore it as was (no need to make this
  ;; buffer local)
  (setq cljr-pre-key-mode-window-conf
        (current-window-configuration))
  ;; setup the mode, draw the buffer
  (let ((buf (get-buffer-create (format cljr-key-mode-buf-name
                                        (symbol-name for-group)))))
    (setq cljr-key-mode-last-buffer buf)
    (split-window-vertically)
    (other-window 1)
    (switch-to-buffer buf)
    (kill-all-local-variables)
    (set (make-local-variable 'scroll-margin) 0)
    (set (make-local-variable
          'cljr-key-mode-current-options)
         original-opts)
    (set (make-local-variable
          'cljr-key-mode-current-args)
         (make-hash-table))
    (set (make-local-variable 'cljr-key-mode-prefix) current-prefix-arg)
    (cljr-key-mode-redraw for-group))
  (when cljr-key-mode-show-usage
    (message (concat "Run 'actions' with their prefixes. "
                     "'?' for more help."))))

(defun cljr-key-mode-get-key-map (for-group)
  "Get or build the keymap for FOR-GROUP."
  (or (cdr (assoc for-group cljr-key-mode-keymaps))
      (cljr-key-mode-build-keymap for-group)))

(defun cljr-key-mode-redraw (for-group)
  "(re)draw the cljr key buffer."
  (let ((buffer-read-only nil)
        (current-exec (get-text-property (point) 'key-group-executor))
        (new-exec-pos)
        (old-point (point))
        (is-first (zerop (buffer-size)))
        (actions-p nil))
    (erase-buffer)
    (make-local-variable 'font-lock-defaults)
    (use-local-map (cljr-key-mode-get-key-map for-group))
    (setq actions-p (cljr-key-mode-draw for-group))
    (delete-trailing-whitespace)
    (setq mode-name "cljr-key-mode" major-mode 'cljr-key-mode)
    (when current-exec
      (setq new-exec-pos
            (cdr (assoc current-exec
                        (cljr-key-mode-build-exec-point-alist)))))
    (cond ((and is-first actions-p)
           (goto-char actions-p)
           (cljr-key-mode-jump-to-next-exec))
          (new-exec-pos
           (goto-char new-exec-pos)
           (skip-chars-forward " "))
          (t
           (goto-char old-point))))
  (setq buffer-read-only t)
  (fit-window-to-buffer))

(defun cljr-key-mode-build-exec-point-alist ()
  (save-excursion
    (goto-char (point-min))
    (let* ((exec (get-text-property (point) 'key-group-executor))
           (exec-alist (and exec `((,exec . ,(point))))))
      (cl-do nil ((eobp) (nreverse exec-alist))
        (when (not (eq exec (get-text-property (point) 'key-group-executor)))
          (setq exec (get-text-property (point) 'key-group-executor))
          (when exec (push (cons exec (point)) exec-alist)))
        (forward-char)))))

;;; Draw Buffer

(defun cljr-key-mode-draw-header (header)
  "Draw a header with the correct face."
  (insert (propertize header 'face 'cljr-key-mode-header-face) "\n"))

(defvar cljr-key-mode-args-in-cols nil
  "When true, draw arguments in columns as with switches and options.")

(defun cljr-key-mode-draw-args (args)
  "Draw the args part of the menu."
  (cljr-key-mode-draw-buttons
   "Args"
   args
   (lambda (x)
     (format "(%s) %s"
             (nth 2 x)
             (propertize (gethash (nth 2 x) cljr-key-mode-current-args "")
                         'face 'cljr-key-mode-args-face)))
   (not cljr-key-mode-args-in-cols)))

(defun cljr-key-mode-draw-switches (switches)
  "Draw the switches part of the menu."
  (cljr-key-mode-draw-buttons
   "Switches"
   switches
   (lambda (x)
     (format "(%s)" (let ((s (nth 2 x)))
                      (if (member s cljr-key-mode-current-options)
                          (propertize s 'face 'cljr-key-mode-switch-face)
                        s))))))

(defun cljr-key-mode-draw-actions (actions)
  "Draw the actions part of the menu."
  (cljr-key-mode-draw-buttons "Actions" actions nil))

(defun cljr-key-mode-draw-buttons (section xs maker
                                    &optional one-col-each)
  (when xs
    (cljr-key-mode-draw-header section)
    (cljr-key-mode-draw-in-cols
     (mapcar (lambda (x)
               (let* ((head (propertize (car x) 'face 'cljr-key-mode-button-face))
                      (desc (nth 1 x))
                      (more (and maker (funcall maker x)))
                      (text (format " %s: %s%s%s"
                                    head desc (if more " " "") (or more ""))))
                 (propertize text 'key-group-executor (car x))))
             xs)
     one-col-each)))

(defun cljr-key-mode-draw-in-cols (strings one-col-each)
  "Given a list of strings, print in columns (using `insert').
If ONE-COL-EACH is true then don't columify, but rather, draw
each item on one line."
  (let ((longest-act (apply 'max (mapcar 'length strings))))
    (while strings
      (let ((str (car strings)))
        (let ((padding (make-string (- (+ longest-act 3) (length str)) ? )))
          (insert str)
          (if (or one-col-each
                  (and (> (+ (length padding) ;
                             (current-column)
                             longest-act)
                          (window-width))
                       (cdr strings)))
              (insert "\n")
            (insert padding))))
      (setq strings (cdr strings))))
  (insert "\n"))

(defun cljr-key-mode-draw (for-group)
  "Draw actions, switches and parameters.
Return the point before the actions part, if any, nil otherwise."
  (let* ((options (cljr-key-mode-options-for-group for-group))
         (switches (cdr (assoc 'switches options)))
         (arguments (cdr (assoc 'arguments options)))
         (actions (cdr (assoc 'actions options)))
         (p nil))
    (cljr-key-mode-draw-switches switches)
    (cljr-key-mode-draw-args arguments)
    (when actions (setq p (point-marker)))
    (cljr-key-mode-draw-actions actions)
    (insert "\n")
    p))

;;; Generate Groups

(defun cljr-key-mode-de-generate (group)
  "Unbind the function for GROUP."
  (fmakunbound
   (intern (concat "cljr-key-mode-popup-" (symbol-name group)))))

(defun cljr-key-mode-generate (group)
  "Generate the key-group menu for GROUP."
  (let ((opts (cljr-key-mode-options-for-group group)))
    (eval
     `(defun ,(intern (concat "cljr-key-mode-popup-" (symbol-name group))) nil
        ,(concat "Key menu for " (symbol-name group))
        (interactive)
        (cljr-key-mode
         (quote ,group)
         ;; As a tempory kludge it is okay to do this here.
         ,(cl-case group
            (logging
             '(list "--graph"))
            (diff-options
             '(when (local-variable-p 'cljr-diff-options)
                cljr-diff-options))))))))

;; create the interactive functions for the key mode popups (which are
;; applied in the top-level key maps)
(mapc (lambda (g)
        (cljr-key-mode-generate (car g)))
      cljr-key-mode-groups)

;;;###autoload (mapc (lambda (g) (eval `(autoload ',(intern (concat "cljr-key-mode-popup-" (symbol-name (car g)))) "cljr-key-mode" ,(concat "Key menu for " (symbol-name (car g))) t))) cljr-key-mode-groups)

(provide 'cljr-key-mode)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; cljr-key-mode.el ends here
