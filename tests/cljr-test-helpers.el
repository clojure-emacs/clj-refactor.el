;;; cljr-test-helpers.el --- Helpers for clj-refactor's specs -*- lexical-binding: t; -*-

;;; Commentary:

;; What the specs that exercise whole commands are built from.  A spec
;; sets up a buffer (or a project on disk), runs a command the way a user
;; would, and looks at what's left in the buffer:
;;
;;   (it "expands the let one level"
;;     (expect (cljr-test-refactor "(inc (let [a 1]\n       |a))"
;;               (cljr-expand-let))
;;             :to-equal "(let [a 1]\n  (inc a))"))
;;
;; The `|' in the text says where point is.  Commands that prompt are run
;; with `cljr-test-call', which also answers the prompts, and anything
;; that depends on actual key presses with `cljr-test-keys' and
;; `cljr-test-type'.

;;; Code:

(require 'cl-lib)
(require 'paredit)
(require 'yasnippet)
(require 'clojure-mode)
(require 'clj-refactor)

;; The prefix the refactoring commands are bound under in the specs.
(cljr-add-keybindings-with-prefix "C-!")

;; The specs are about what the refactorings do to the code.  Their
;; asking for permission first is for the specs about that to turn on.
(setq cljr-warn-on-eval nil
      cljr-preview-refactorings nil)

;;; Buffers

(defun cljr-test-insert (text)
  "Insert TEXT and leave point where the `|' in it was.
Point is left after TEXT if it has no `|'."
  (let ((start (point)))
    (insert text)
    (let ((end (point)))
      (goto-char start)
      (if (search-forward "|" end t)
          (delete-char -1)
        (goto-char end)))))

(defun cljr-test-buffer-string (&optional with-point)
  "Return the text of the current buffer.
With WITH-POINT non-nil, mark where point is with a `|'."
  (if with-point
      (concat (buffer-substring-no-properties (point-min) (point))
              "|"
              (buffer-substring-no-properties (point) (point-max)))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun cljr-test--enable-modes ()
  "Turn on in the current buffer what a user of clj-refactor has on."
  (clj-refactor-mode 1)
  (yas-minor-mode 1))

;; The setup the README suggests, so that the buffers a command opens on
;; its own are set up like the ones the specs open.
(add-hook 'clojure-mode-hook #'cljr-test--enable-modes)

;; Off in batch Emacs, which would leave the region inactive in the specs
;; that select one.
(transient-mark-mode 1)

(defmacro cljr-test-with-buffer (content &rest body)
  "Evaluate BODY in a Clojure buffer holding CONTENT.
Point is where the `|' in CONTENT is.  The buffer is the selected
window's, so that key presses end up in it.  It isn't visiting a
file: see `cljr-test-with-project' for the commands that need one."
  (declare (indent 1) (debug t))
  (let ((buffer (make-symbol "buffer")))
    `(let ((,buffer (generate-new-buffer "*cljr-test*")))
       (unwind-protect
           (save-window-excursion
             (switch-to-buffer ,buffer)
             (clojure-mode)
             (cljr-test-insert ,content)
             ,@body)
         (kill-buffer ,buffer)))))

(defmacro cljr-test-refactor (content &rest body)
  "Evaluate BODY in a Clojure buffer holding CONTENT; return its text.
See `cljr-test-with-buffer'."
  (declare (indent 1) (debug t))
  `(cljr-test-with-buffer ,content
     ,@body
     (cljr-test-buffer-string)))

(defun cljr-test-goto (text)
  "Move point to before the first occurrence of TEXT in the buffer."
  (goto-char (point-min))
  (search-forward text)
  (goto-char (match-beginning 0)))

(defun cljr-test-select (from to)
  "Make the region what's between the texts FROM and TO."
  (cljr-test-goto from)
  (set-mark (point))
  (cljr-test-goto to))

;;; Input

(defvar cljr-test--command nil
  "The command `cljr-test-call' is running.")

(defun cljr-test--call-command ()
  "Run `cljr-test--command' as if the user had invoked it."
  (interactive)
  (setq this-command cljr-test--command)
  (call-interactively cljr-test--command))

(global-set-key [cljr-test-call] #'cljr-test--call-command)

(defun cljr-test--no-keys-left (prompt &rest _)
  "Fail if there are no keys left to answer PROMPT with.
A prompt that runs out of keys would otherwise get its default for
an answer, or wait for one forever."
  (when (and executing-kbd-macro
             (>= executing-kbd-macro-index (length executing-kbd-macro)))
    (error "Nothing left to answer the prompt with: %s" prompt)))

(defconst cljr-test--prompting-functions
  '(read-from-minibuffer read-string completing-read
    y-or-n-p yes-or-no-p read-char-choice)
  "The functions the commands under test ask their questions with.")

(dolist (function cljr-test--prompting-functions)
  (advice-add function :before #'cljr-test--no-keys-left))

(defun cljr-test-keys (&rest keys)
  "Press KEYS, each a key sequence in the notation `kbd' takes.
All of them go through the command loop in one go, so the keys that
follow a command answer whatever it prompts for.  A prompt that's
left without an answer is an error."
  (execute-kbd-macro (apply #'vconcat (mapcar #'kbd keys))))

(defun cljr-test-type (text)
  "Type TEXT, one character at a time."
  (execute-kbd-macro (string-to-vector text)))

(defun cljr-test-call (command &rest answers)
  "Call COMMAND interactively, answering its prompts with ANSWERS.
Each answer is typed into the minibuffer and confirmed with RET.  A
prompt that's left without an answer is an error."
  (let ((cljr-test--command command))
    (execute-kbd-macro
     (apply #'vconcat [cljr-test-call]
            (mapcar (lambda (answer) (vconcat answer "\r")) answers)))))

;;; Projects

(defvar cljr-test-project-root nil
  "The root of the project `cljr-test-with-project' has set up.")

(defun cljr-test-file (path)
  "Return the absolute name of PATH in the test project."
  (expand-file-name path cljr-test-project-root))

(defun cljr-test-write-file (path content)
  "Write CONTENT to PATH in the test project, without visiting it."
  (let ((file (cljr-test-file path)))
    (make-directory (file-name-directory file) t)
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region content nil file nil 'silent))))

(defun cljr-test-file-string (path &optional on-disk)
  "Return what PATH in the test project holds.
That's the text of the buffer visiting it, if there's one, as the
refactorings don't necessarily save what they change.  With ON-DISK
non-nil it's what has been saved, whatever a buffer has to say."
  (let* ((file (cljr-test-file path))
         (buffer (and (not on-disk) (find-buffer-visiting file))))
    (if buffer
        (with-current-buffer buffer
          (cljr-test-buffer-string))
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string)))))

(defun cljr-test-visit (path &optional content)
  "Visit PATH in the test project, in the selected window.
With CONTENT, the buffer is made to hold it first, with point where
the `|' in it is."
  ;; In batch Emacs a new file in a directory that isn't there yet makes
  ;; for a read-only buffer.
  (make-directory (file-name-directory (cljr-test-file path)) t)
  (find-file (cljr-test-file path))
  (when content
    (erase-buffer)
    (cljr-test-insert content)))

(defun cljr-test--kill-project-buffers ()
  "Kill the buffers visiting the files of the test project."
  (dolist (buffer (buffer-list))
    (let ((file (buffer-local-value 'buffer-file-name buffer)))
      (when (and file (file-in-directory-p file cljr-test-project-root))
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(defun cljr-test--make-project (type name)
  "Create a project of TYPE called NAME in a new directory; return it.
TYPE is `lein' or `deps'."
  (let* ((root (file-name-as-directory
                (file-truename (make-temp-file "cljr-test-" t))))
         (cljr-test-project-root root))
    (pcase type
      ('lein (cljr-test-write-file
              "project.clj"
              (format "(defproject %s \"0.1.0-SNAPSHOT\"
  :dependencies [[org.clojure/clojure \"1.9.0\"]])" name)))
      ('deps (cljr-test-write-file
              "deps.edn"
              "{:paths [\"src\" \"resources\"]
 :deps {org.clojure/clojure {:mvn/version \"1.9.0\"}}}"))
      (_ (error "Unknown project type: %s" type)))
    (make-directory (cljr-test-file (concat "src/" name)) t)
    (make-directory (cljr-test-file (concat "test/" name)) t)
    root))

(defmacro cljr-test-with-project (spec &rest body)
  "Evaluate BODY with a Clojure project on disk.
SPEC is a plist:

  :name   what the project is called (\"example\" by default); it has
          a src/NAME and a test/NAME directory
  :type   `lein' (the default) or `deps'
  :files  an alist of file names, relative to the root, and contents

BODY runs with the root as `default-directory'.  It gets at the
files with `cljr-test-visit' and `cljr-test-file-string'.  The
project and the buffers visiting its files are gone afterwards."
  (declare (indent 1) (debug t))
  `(let* ((cljr-test-project-root
           (cljr-test--make-project (or ,(plist-get spec :type) 'lein)
                                    (or ,(plist-get spec :name) "example")))
          (default-directory cljr-test-project-root))
     (unwind-protect
         (save-window-excursion
           (pcase-dolist (`(,path . ,content) ,(plist-get spec :files))
             (cljr-test-write-file path content))
           ,@body)
       (cljr-test--kill-project-buffers)
       (delete-directory cljr-test-project-root t))))

(defmacro cljr-test-refactor-file (path content &rest body)
  "Evaluate BODY in a file of a project holding CONTENT; return its text.
The file is PATH, relative to the root of the project.  This is
`cljr-test-refactor' for the commands that need a file to work on."
  (declare (indent 2) (debug t))
  `(cljr-test-with-project ()
     (cljr-test-visit ,path ,content)
     ,@body
     (cljr-test-file-string ,path)))

;;; The middleware

(defconst cljr-test-example-one "(ns example.one
  (:require [example.two :as two]))

(defn bar []
  (str \"bar\" (two/foo) \"goo\"))

(defn sky []
  (str \"sky: \" (two/star*)))
"
  "A namespace that uses `cljr-test-example-two', for the project-wide specs.")

(defconst cljr-test-example-two "(ns example.two)

(defn foo []
  \"goo\")

(defn star* []
  \"star\")
"
  "A namespace with two functions, for the project-wide specs.")

(defmacro cljr-test-with-example-project (&rest body)
  "Evaluate BODY in example.one, in a project that has example.two as well.
See `cljr-test-example-one' and `cljr-test-example-two'."
  (declare (indent 0) (debug t))
  `(cljr-test-with-project
       (:files `(("src/example/one.clj" . ,cljr-test-example-one)
                 ("src/example/two.clj" . ,cljr-test-example-two)))
     (cljr-test-visit "src/example/one.clj")
     ,@body))

(defun cljr-test-hash (&rest keys-and-values)
  "Return a hash table of KEYS-AND-VALUES, like the middleware's maps."
  (let ((table (make-hash-table :test #'equal)))
    (cl-loop for (key value) on keys-and-values by #'cddr
             do (puthash key value table))
    table))

(defun cljr-test-occurrence (path &rest keys-and-values)
  "Return an occurrence in PATH of the test project, as find-symbol reports it.
KEYS-AND-VALUES are the rest of it: `:line-beg', `:col-beg' and so on."
  (apply #'cljr-test-hash :file (cljr-test-file path) keys-and-values))

(defun cljr-test-param-keep (old-index new-index name &optional new-name)
  "Return the signature change that keeps the parameter NAME.
It moves from OLD-INDEX to NEW-INDEX, and is called NEW-NAME if given."
  (cljr-test-hash :op :keep :old-index old-index :new-index new-index
                  :old-name name :new-name (or new-name name)))

(defun cljr-test-param-add (new-index name &optional default)
  "Return the signature change that adds the parameter NAME at NEW-INDEX.
DEFAULT is what the call sites pass for it, \"nil\" if not given."
  (cljr-test-hash :op :add :new-index new-index :new-name name
                  :default (or default "nil")))

(defun cljr-test-param-remove (old-index name)
  "Return the signature change that removes the parameter NAME at OLD-INDEX."
  (cljr-test-hash :op :remove :old-index old-index :old-name name))

(defun cljr-test-middleware-answers (answer)
  "Have the middleware reply with ANSWER to what it's asked in this spec.
ANSWER is what the op puts under its key: as a rule a string of EDN."
  (spy-on 'cljr--call-middleware-sync :and-return-value answer))

(defun cljr-test-middleware-request (key)
  "Return what the first request sent to the middleware has under KEY.
KEY is a string."
  (cl-loop for (name value) on (car (spy-calls-args-for 'cljr--call-middleware-sync 0))
           by #'cddr
           when (equal name key) return value))

(defun cljr-test-without-middleware ()
  "Stand in for a REPL that has every op, for the current spec.
The specs drive the part of a refactoring that edits buffers, and
this keeps the checks that come before it out of the way."
  (spy-on 'cljr--ensure-op-supported :and-return-value t)
  (spy-on 'cljr--op-supported-p :and-return-value t)
  (spy-on 'cljr--maybe-eval-ns-form))

(provide 'cljr-test-helpers)

;;; cljr-test-helpers.el ends here
