;;; helpers-table-test.el --- Specs for the table of refactorings -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cljr-test-helpers)

;; An entry of `cljr--all-helpers' is (KEYS COMMAND DESCRIPTION CHAR TYPES).
;; KEYS and COMMAND make the key bindings.  CHAR and TYPES are what the
;; menus of each type used to be built from: nothing reads them now that
;; `clj-refactor-menu' lists its commands itself, but they still say which
;; of its columns a command belongs in.

(defconst cljr-test-menu-columns
  '(("ns" . "Namespace")
    ("code" . "Code")
    ("toplevel-form" . "Top-level form")
    ("project" . "Project")
    ("cljr" . "clj-refactor"))
  "The types of `cljr--all-helpers' and the menu column each stands for.")

(defun cljr-test-menu-column-of (command)
  "Return the heading of the column of `clj-refactor-menu' COMMAND is in."
  ;; How transient stores a layout depends on its version.  In all of
  ;; them a column's heading comes before its commands, and a command is a
  ;; list with a :command in it, which is all this relies on.
  (let ((headings (mapcar #'cdr cljr-test-menu-columns))
        heading found)
    (cl-labels ((walk (node)
                  (cond
                   ((vectorp node) (walk (append node nil)))
                   ((not (consp node)))
                   ((memq :command node)
                    (when (eq (cadr (memq :command node)) command)
                      (setq found heading)))
                   (t
                    (let ((description (cadr (memq :description node))))
                      (when (stringp description)
                        (setq heading (car (member description headings)))))
                    (while (and (consp node) (not found))
                      (walk (pop node)))))))
      (walk (get 'clj-refactor-menu 'transient--layout)))
    found))

(describe "cljr--all-helpers"
  (it "has no two commands of a type with the same menu key"
    (dolist (type (mapcar #'car cljr-test-menu-columns))
      (let (seen)
        (pcase-dolist (`(,_keys ,command ,_description ,char ,types) cljr--all-helpers)
          (when (member type types)
            (expect (cons type (assq char seen)) :to-equal (list type))
            (push (cons char command) seen))))))

  (it "has every command under keys of its own"
    (let ((keys (mapcar #'car cljr--all-helpers))
          (commands (mapcar #'cadr cljr--all-helpers)))
      (expect (cl-remove-duplicates keys :test #'equal) :to-equal keys)
      (expect (cl-remove-duplicates commands) :to-equal commands)))

  (it "has only commands that exist"
    (pcase-dolist (`(,_keys ,command . ,_) cljr--all-helpers)
      (expect (cons command (fboundp command)) :to-equal (cons command t))
      (expect (cons command (commandp command)) :to-equal (cons command t))))

  (it "describes every command"
    (pcase-dolist (`(,_keys ,command ,description . ,_) cljr--all-helpers)
      (expect (cons command (and (stringp description)
                                 (not (string-empty-p description))))
              :to-equal (cons command t))))

  (it "has only types the menu has a column for"
    (pcase-dolist (`(,_keys ,command ,_description ,_char ,types) cljr--all-helpers)
      (expect types :not :to-be nil)
      (dolist (type types)
        (expect (cons command (car (assoc type cljr-test-menu-columns)))
                :to-equal (cons command type)))))

  (it "puts every command in a menu column of one of its types"
    (pcase-dolist (`(,_keys ,command ,_description ,_char ,types) cljr--all-helpers)
      ;; The menu doesn't list itself.
      (unless (eq command 'clj-refactor-menu)
        (let ((column (cljr-test-menu-column-of command))
              (columns (mapcar (lambda (type)
                                 (cdr (assoc type cljr-test-menu-columns)))
                               types)))
          (expect (list command column (and (member column columns) t))
                  :to-equal (list command column t)))))))

(describe "cljr-add-keybindings-with-prefix"
  (it "binds every command under the prefix"
    (let ((clj-refactor-map (make-sparse-keymap)))
      (cljr-add-keybindings-with-prefix "C-c C-m")
      (pcase-dolist (`(,keys ,command . ,_) cljr--all-helpers)
        (expect (cons keys (lookup-key clj-refactor-map (kbd (concat "C-c C-m " keys))))
                :to-equal (cons keys command)))))

  (it "binds nothing outside of the prefix"
    (let ((clj-refactor-map (make-sparse-keymap)))
      (cljr-add-keybindings-with-prefix "C-c C-m")
      (expect (mapcar #'car (cdr clj-refactor-map)) :to-equal (list ?\C-c))))

  (it "makes the keys work in a Clojure buffer"
    (expect (cljr-test-refactor "(defn add [a b]\n  |(+ a b))"
              (cljr-test-keys "C-! cp"))
            :to-equal "(defn- add [a b]\n  (+ a b))")))

(describe "cljr-add-keybindings-with-modifier"
  (it "binds every command to its keys pressed with the modifier"
    (let ((clj-refactor-map (make-sparse-keymap)))
      (cljr-add-keybindings-with-modifier "C-s-")
      (expect (lookup-key clj-refactor-map (kbd "C-s-e C-s-l")) :to-be 'cljr-expand-let)
      (expect (lookup-key clj-refactor-map (kbd "C-s-s C-s-p"))
              :to-be 'cljr-sort-project-dependencies))))

;;; helpers-table-test.el ends here
