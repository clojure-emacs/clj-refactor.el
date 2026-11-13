;;; promote-function-test.el --- Specs for cljr-promote-function -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cljr-test-helpers)

(defvar cljr-test-used-locals ""
  "What the middleware says the locals the promoted function uses are.")

(defmacro cljr-test-promote (content &rest answers)
  "Promote the function at the `|' in CONTENT; return the buffer's text.
ANSWERS are what the command's prompts are answered with."
  (declare (indent 1))
  `(cljr-test-refactor ,content
     (cljr-test-call #'cljr-promote-function ,@answers)))

(describe "cljr-promote-function"
  (before-each
    (cljr-test-without-middleware)
    (setq cljr-test-used-locals "")
    (spy-on 'cljr--call-middleware-to-find-used-locals
            :and-call-fake (lambda (&rest _) cljr-test-used-locals)))

  (describe "on a fn"
    (it "makes it a private top-level function with the name asked for"
      (expect (cljr-test-promote "(map (fn [sym] (-> sym (str \"!\") |symbol)) '[aww yeah])"
                "shout-it!")
              :to-equal "(defn- shout-it!
  [sym]
  (-> sym (str \"!\") symbol))

(map shout-it! '[aww yeah])"))

    (it "uses the name of the fn, if it has one, instead of asking"
      (expect (cljr-test-promote "((fn foobar [x] (|str x)) 123)")
              :to-equal "(defn- foobar [x]
  (str x))

(foobar 123)"))

    (it "works with point before the parameters"
      (expect (cljr-test-promote "((fn |foobar [x] (str x)) 123)")
              :to-equal "(defn- foobar [x]
  (str x))

(foobar 123)"))

    (it "promotes the innermost fn around point, function literals in it included"
      (expect (cljr-test-promote "(fn [cells]
  (reduce (fn [{|:keys [key-1 key-2] :as acc} [idx cell]]
            (assoc acc (if (even? idx) key-1 key-2)
                   (map #(do-stuff %) (range idx))))
          {}
          (map-indexed vector cells)))"
                "step")
              :to-equal "(defn- step
  [{:keys [key-1 key-2] :as acc} [idx cell]]
  (assoc acc (if (even? idx) key-1 key-2)
         (map #(do-stuff %) (range idx))))

(fn [cells]
  (reduce step
          {}
          (map-indexed vector cells)))"))

    (it "passes the locals the fn closes over as leading arguments"
      (setq cljr-test-used-locals "foo bar")
      (expect (cljr-test-promote "(let [foo 1
      bar 2]
  (map (fn| [n] (+ foo bar n)) [1 2 3]))"
                "foobar-adder")
              :to-equal "(defn- foobar-adder
  [foo bar n]
  (+ foo bar n))

(let [foo 1
      bar 2]
  (map (partial foobar-adder foo bar) [1 2 3]))"))

    (it "makes the locals the only parameters of a fn that takes none"
      (setq cljr-test-used-locals "foo")
      (expect (cljr-test-promote "(let [foo 1]
  (repeatedly (fn [] (|inc foo))))"
                "next-foo")
              :to-equal "(defn- next-foo
  [foo]
  (inc foo))

(let [foo 1]
  (repeatedly (partial next-foo foo)))"))

    (it "asks the middleware about the position of the fn"
      (cljr-test-promote "(map inc
     (map (fn [n] (|inc n)) [1 2 3]))"
        "plus-one")
      (expect 'cljr--call-middleware-to-find-used-locals
              :to-have-been-called-with nil 2 12))

    (it "puts the new function after the ns form"
      (expect (cljr-test-promote "(ns cljr.core)

(map (fn [x] (|str x)) (range 10))"
                "stringify")
              :to-equal "(ns cljr.core)

(defn- stringify
  [x]
  (str x))

(map stringify (range 10))"))

    (it "makes the function public when private ones aren't favoured"
      (let ((cljr-favor-private-functions nil))
        (expect (cljr-test-promote "((fn foobar [x] (|str x)) 123)")
                :to-equal "(defn foobar [x]
  (str x))

(foobar 123)")))

    (it "marks the function private with metadata when that's preferred"
      (let ((clojure-use-metadata-for-privacy t))
        (expect (cljr-test-promote "((fn foobar [x] (|str x)) 123)")
                :to-equal "(defn ^:private foobar [x]
  (str x))

(foobar 123)")))

    (it "leaves point on the name that took the place of the fn"
      (expect (cljr-test-with-buffer "(map (fn [x] (st|r x)) (range 10))"
                (cljr-test-call #'cljr-promote-function "stringify")
                (cljr-test-buffer-string t))
              :to-equal "(defn- stringify
  [x]
  (str x))

(map |stringify (range 10))"))

    (it "is bound to pf under the prefix"
      (expect (cljr-test-refactor "((fn foobar [x] (|str x)) 123)"
                (cljr-test-keys "C-! pf"))
              :to-equal "(defn- foobar [x]
  (str x))

(foobar 123)")))

  (describe "on a function literal"
    (it "makes it a fn, naming its argument as asked"
      (expect (cljr-test-promote "(map #(|str %) (range 10))" "x")
              :to-equal "(map (fn [x] (str x)) (range 10))"))

    (it "leaves the fn around it alone"
      (expect (cljr-test-promote "(fn [cells]
  (reduce (fn [{:keys [key-1 key-2] :as acc} [idx cell]]
            (assoc acc (if (even? idx) key-1 key-2)
                   (map #(do-stuff |%) (range idx))))
          {}
          (map-indexed vector cells)))"
                "x")
              :to-equal "(fn [cells]
  (reduce (fn [{:keys [key-1 key-2] :as acc} [idx cell]]
            (assoc acc (if (even? idx) key-1 key-2)
                   (map (fn [x] (do-stuff x)) (range idx))))
          {}
          (map-indexed vector cells)))"))

    (it "works with point between the # and the paren"
      (expect (cljr-test-promote "(map #|(str %) (range 10))" "x")
              :to-equal "(map (fn [x] (str x)) (range 10))"))

    (it "names each of the arguments"
      (expect (cljr-test-promote "(map #(|+ %1 %2) xs ys)" "a" "b")
              :to-equal "(map (fn [a b] (+ a b)) xs ys)"))

    (it "goes all the way to a top-level function with a prefix argument"
      (expect (cljr-test-refactor "(map |#(str %) (range 10))"
                (cljr-test-keys "C-u C-! pf" "x RET" "stringify RET"))
              :to-equal "(defn- stringify
  [x]
  (str x))

(map stringify (range 10))"))

    ;; Pending: today it stops halfway with a scan-error.  Once the
    ;; literal is a fn, the command goes on from where point was instead
    ;; of from the start of the fn.
    (xit "goes all the way with a prefix argument when point is inside the literal"
      (expect (cljr-test-refactor "(map #(|str %) (range 10))"
                (cljr-test-keys "C-u C-! pf" "x RET" "stringify RET"))
              :to-equal "(defn- stringify
  [x]
  (str x))

(map stringify (range 10))")))

  (it "complains when point isn't in a function"
    (cljr-test-with-buffer "(ns cljr.core)

(map inc |(range 10))"
      (expect (cljr-promote-function nil) :to-throw 'error)
      (expect (cljr-test-buffer-string t)
              :to-equal "(ns cljr.core)

(map inc |(range 10))"))
    (cljr-test-with-buffer "(map inc (range 10))\n|"
      (expect (cljr-promote-function nil)
              :to-throw 'error '("Not inside a s-expression")))))

;;; promote-function-test.el ends here
