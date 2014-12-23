(Given "^I open file \"\\(.+\\)\"$"
  (lambda (filename)
    (setq default-directory clj-refactor-root-path)
    (find-file filename)))

(Given "^I have a project \"\\([^\"]+\\)\" in \"\\([^\"]+\\)\"$"
  (lambda (project-name dir-name)
    (setq default-directory clj-refactor-root-path)

    ;; delete old directory
    (when (file-exists-p dir-name)
      (delete-directory dir-name t))

    ;; create directory structure
    (mkdir (expand-file-name project-name (expand-file-name "src" dir-name)) t)
    (mkdir (expand-file-name project-name (expand-file-name "test" dir-name)) t)

    ;; add project.clj
    (with-temp-file (expand-file-name "project.clj" dir-name)
      (insert "(defproject " project-name " \"0.1.0-SNAPSHOT\")"))))

(Given "^I have a clojure-file \"\\([^\"]+\\)\"$"
  (lambda (file-name)
    (setq default-directory clj-refactor-root-path)
    (find-file file-name)
    (save-buffer)
    (kill-buffer)))

(Given "^I switch project-clean-prompt off$"
  (lambda ()
    (setq cljr-project-clean-prompt nil)))

(Given "^I switch auto-sort off$"
  (lambda ()
    (setq cljr-auto-sort-ns nil)))

(Given "^I switch auto-sort on$"
  (lambda ()
    (setq cljr-auto-sort-ns t)))

(Given "^I set sort comparator to string length$"
  (lambda ()
    (setq cljr-sort-comparator 'cljr--string-length-comparator)))

(Given "^I set sort comparator to semantic$"
  (lambda ()
    (setq cljr-sort-comparator 'cljr--semantic-comparator)))

(Given "^I set sort comparator to string natural$"
  (lambda ()
     (setq cljr-sort-comparator 'cljr--string-natural-comparator)))

(Given "^I exit multiple-cursors-mode"
  (lambda ()
    (multiple-cursors-mode 0)))

(defun run-rename-symbol (ns occurrences new-name)
  (cljr--rename-symbol ns (cljr--read-symbol-metadata occurrences) new-name))

(Given "^I call the rename callback directly with mock data for foo->baz"
  (lambda ()
    (run-rename-symbol "example.two" '(3  4  1  9  "foo"  "tmp/src/example/two.clj"  "" 5  5  15  23  "foo"  "tmp/src/example/one.clj"  "") "baz")))

(Given "^I call the rename callback directly with mock data for star->asterisk"
  (lambda ()
    (run-rename-symbol "example.two" '(6  7  1  10  "star*"  "tmp/src/example/two.clj"  "" 8  8  17  27  "star*"  "tmp/src/example/one.clj"  "") "asterisk*")))

(Given "^I call the add-missing-libspec callback directly with mock data to import"
  (lambda ()
    (cljr--add-missing-libspec "Date" "java.util.Date" "import")))

(Given "^I call the add-missing-libspec callback directly with mock data to refer split"
  (lambda ()
    (cljr--add-missing-libspec "split" "clojure.string" "require")))

(Given "^I call the add-missing-libspec callback directly with mock data to alias clojure.string"
  (lambda ()
    (cljr--add-missing-libspec "str/split" "clojure.string" "require")))

(Then "^the file should be named \"\\([^\"]+\\)\"$"
  (lambda (file-name-postfix)
    (assert (s-ends-with? file-name-postfix (buffer-file-name)) nil "Expected %S to end with %S" (buffer-file-name) file-name-postfix)))

(And "^the cursor is inside the first defn form$"
  (lambda ()
    (goto-char (point-min))
    (re-search-forward "defn")))
