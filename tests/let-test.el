;;; let-test.el --- Specs for the let refactorings -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cljr-test-helpers)

(describe "cljr-expand-let"
  (it "pulls the let out of the form it's in"
    (expect (cljr-test-refactor "(defn handle-request
  {:status 200
   :body (let [body |(find-body abc)]
           body)})"
              (cljr-expand-let))
            :to-equal "(defn handle-request
  (let [body (find-body abc)]
    {:status 200
     :body body}))"))

  (it "goes up one more level each time"
    (expect (cljr-test-refactor "(defn handle-request
  (let [body |(find-body abc)]
    {:status 200
     :body body}))"
              (cljr-expand-let))
            :to-equal "(let [body (find-body abc)]
  (defn handle-request
    {:status 200
     :body body}))"))

  (it "replaces the other occurrences of the bound values"
    (expect (cljr-test-refactor "(defn handle-request
  (when (find-body abc)
    (println \"body: \" (find-body	abc) \", params: \" [\"param1\"    \"param2\"] \", status: \" 200)
    {:status 200
     :foobar 200000
     :baz 049403200
     :body (let |[body (find-body abc)
                 params [\"param1\" \"param2\"]
                 status 200]
             body)
     :params [\"param1\"
              \"param2\"]}))

(defn handle-request-other
   {:status 200})"
              (cljr-expand-let)
              (cljr-expand-let))
            :to-equal "(defn handle-request
  (let [body (find-body abc)
        params [\"param1\" \"param2\"]
        status 200]
    (when body
      (println \"body: \" body \", params: \" params \", status: \" status)
      {:status status
       :foobar 200000
       :baz 049403200
       :body body
       :params params})))

(defn handle-request-other
   {:status 200})"))

  (it "can be repeated with l"
    (expect (cljr-test-refactor "(defn handle-request
  {:status 200
   :body (let [body |(find-body abc)]
           body)})"
              (cljr-test-keys "C-! el" "l"))
            :to-equal "(let [body (find-body abc)]
  (defn handle-request
    {:status 200
     :body body}))")))

(describe "the let commands of clojure-mode"
  (it "are bound under the prefix: introduce let"
    (expect (cljr-test-refactor "{:status 200
 :body |(find-body abc)}"
              (cljr-test-keys "C-! il" "body" "RET"))
            :to-equal "{:status 200
 :body (let [body (find-body abc)]
         body)}"))

  (it "are bound under the prefix: move to let"
    (expect (cljr-test-refactor "(defn handle-request
  (let [body (find-body abc)]
    {:status |(or status 500)
     :body body}))"
              (cljr-test-keys "C-! ml" "status" "RET"))
            :to-equal "(defn handle-request
  (let [body (find-body abc)
        status (or status 500)]
    {:status status
     :body body}))"))

  (it "move a constant to a when-let"
    (expect (cljr-test-refactor "(defn handle-request
  (when-let[body (find-body abc)]
    {:status |42
     :body body}))"
              (cljr-test-call #'clojure-move-to-let "status"))
            :to-equal "(defn handle-request
  (when-let[body (find-body abc)
            status 42]
    {:status status
     :body body}))"))

  (it "move to an empty if-let"
    (expect (cljr-test-refactor "(defn handle-request
  (if-let []
    {:status |(or status 500)
     :body body}))"
              (cljr-test-call #'clojure-move-to-let "status"))
            :to-equal "(defn handle-request
  (if-let [status (or status 500)]
    {:status status
     :body body}))"))

  (it "introduce a let when there's none to move to"
    (expect (cljr-test-refactor "(defn handle-request
  {:status |(or status 500)
   :body body})"
              (cljr-test-call #'clojure-move-to-let "status"))
            :to-equal "(defn handle-request
  {:status (let [status (or status 500)]
             status)
   :body body})")))

;;; let-test.el ends here
