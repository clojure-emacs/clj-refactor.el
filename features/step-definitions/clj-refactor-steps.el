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

(Given "^I call the rename callback directly with mock data for foo->baz"
       (lambda ()
         (cljr--rename-occurrences "example.two"
                                   '((:line-beg 3 :line-end 4 :col-beg 1 :col-end 9
                                                :name "foo"
                                                :file "tmp/src/example/two.clj"
                                                :match "")
                                     (:line-beg 5 :line-end 5 :col-beg 15
                                                :col-end 23 :name "foo"
                                                :file "tmp/src/example/one.clj"
                                                :match ""))
                                   "baz")))

(Given "^I call the rename callback directly with mock data for star->asterisk"
       (lambda ()
         (cljr--rename-occurrences "example.two"
                                   '((:line-beg 6 :line-end 7 :col-beg 1
                                                :col-end 10 :name "star*"
                                                :file "tmp/src/example/two.clj"
                                                :match "")
                                     (:line-beg 8 :line-end 8 :col-beg 17
                                                :col-end 27 :name "star*"
                                                :file "tmp/src/example/one.clj"
                                                :match ""))
                                   "asterisk*")))

(Given "^I call the add-missing-libspec callback directly with mock data to import"
       (lambda ()
         (cljr--add-missing-libspec "Date" '((java.util.Date :class)))))

(Given "^I call the add-missing-libspec callback directly with mock data to refer split"
       (lambda ()
         (cljr--add-missing-libspec "split" '((clojure.string  :ns)))))

(Given "^I call the add-missing-libspec callback directly with mock data to alias clojure.string"
       (lambda ()
         (cljr--add-missing-libspec "str/split" '((clojure.string :ns)))))

(Given "^I call the add-missing-libspec callback directly with mock data to require WebrequestHandler"
       (lambda ()
         (cljr--add-missing-libspec "WebrequestHandler" '((modular.ring.WebrequestHandler :type)))))

(Then "^the file should be named \"\\([^\"]+\\)\"$"
      (lambda (file-name-postfix)
        (assert (s-ends-with? file-name-postfix (buffer-file-name)) nil "Expected %S to end with %S" (buffer-file-name) file-name-postfix)))

(And "^the cursor is inside the first defn form$"
  (lambda ()
    (goto-char (point-min))
    (re-search-forward "defn")))
