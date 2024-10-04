(ns destructive.form-reader
  (:import (clojure.lang LineNumberingPushbackReader)
           (java.io StringReader)))

(set! *warn-on-reflection* true)
(set! *default-data-reader-fn* tagged-literal)

(defmacro with-read-known
  "Evaluates body with *read-eval* set to a \"known\" value,
   i.e. substituting true for :unknown if necessary."
  [& body]
  `(binding [*read-eval* (if (= :unknown *read-eval*) true *read-eval*)]
     ~@body))

(defn message->forms
  "Produce a list of forms read from string `s`. Any read failure throws"
  [s]
  (let [EOF    (Object.)
        reader (LineNumberingPushbackReader. (StringReader. s))]
    (reduce (fn [forms [form _]]
              (if (identical? form EOF)
                (reduced forms)
                (conj forms form)))
            [] (repeatedly #(with-read-known (read+string reader false EOF))))))


