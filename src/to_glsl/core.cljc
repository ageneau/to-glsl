(ns to-glsl.core
  (:require
    fipp.visit
    clojure.walk
    [camel-snake-kebab.core :as csk]
    [clojure.string :as string]
    [fipp.engine :as fipp]))

(declare compile-form)
(declare compile-symbol)

(defrecord GLSLPrinter [state]
  fipp.visit/IVisitor
  (visit-meta [this meta x]
    (fipp.visit/visit* this x))
  (visit-string [this x]
    [:text "\"" x "\""])
  (visit-number [this x]
    [:text (str x)])
  (visit-boolean [this x]
    [:text (str x)])
  (visit-seq [this x]
    (compile-form {:visit (partial fipp.visit/visit this) :state state} x))
  (visit-symbol [this x]
    (compile-symbol {:visit (partial fipp.visit/visit this) :state state} x)))

(defn- compile-symbol
  [{:keys [state]} x]
  (let [x-str (str x)]
    [:text (if (string/starts-with? x-str "gl/")
             (str "gl_" (csk/->PascalCase (string/replace x-str "gl/" "")))
             (str x))]))

(defn- compile-typed
  [context definition]
  (let [arg (last definition)
        arg-type (drop-last definition)]
    [:span [:text (string/join " " (map #(csk/->camelCase (str %)) arg-type))] " " (compile-symbol context arg)]))

(defn- compile-def
  [{:keys [visit] :as context} [_ t n initial-value]]
  (into [:span] (concat (list (compile-typed context (flatten [t n])))
                        (when initial-value [[:span [:text " = "] (visit initial-value)]]))))

(defn- compile-def-varying
  [context x]
  (into [:span "varying "] (compile-def context x)))

(defn- compile-def-uniform
  [context x]
  (into [:span "uniform "] (compile-def context x)))

(defn- compile-def-attribute
  [context x]
  (into [:span "attribute "] (compile-def context x)))

(defn- compile-do
  [{:keys [visit]} [_ & forms]]
  (into [:group]
        (map (fn [x] (into [:group]
                           (concat [(visit x)]
                                   (when-not (contains? #{'if 'when 'defn 'pre.if 'pre.ifdef 'pre.cond 'pre.include} (first x)) [";"])
                                   [:break]))) forms)))

(defn- compile-defn
  [{:keys [visit] :as context} [_ n  [return-type args] & body]]
  [:span
   [:text return-type " " (csk/->camelCase n)]
   [:group "(" (interpose ", " (map (partial compile-typed context) args)) ") "]
   [:group "{" :break [:nest 2 (visit `(do ~@body))] "}"]])

(defn- compile-when
  [{:keys [visit]} [_ condition & body]]
  [:span "if (" (visit condition) ") " [:group "{" :break [:nest 2 (visit `(do ~@body))] "}"]])

(defn- compile-if
  [{:keys [visit]} [_ condition success fail]]
  [:span (visit condition) " ? " (visit success) " : " (visit fail)])

(defn- make-doable [form]
  (if (= 'do (first form)) form (list 'do form)))


(defn- compile-pre-include
  [_ [_ file]]
  [:span "#include<" (str file) ">"])

(defn- compile-pre-ifdef
  [{:keys [visit state] :as context} [_ condition success fail]]
  (into [:group]
        (concat [[:span "#ifdef "
                  (fipp.visit/visit (GLSLPrinter. (assoc state :preprocessor? true)) condition)]]
                [:break]
                (compile-do context (make-doable success))
                (when fail [[:line "#else" :line]
                            (compile-do context (make-doable fail))])

                ["#endif"])))

(defn- compile-pre-if
  [{:keys [visit state] :as context} [_ condition success fail]]
  (into [:group]
        (concat [:span "#if " (fipp.visit/visit (GLSLPrinter. (assoc state :preprocessor? true)) condition)]
                [:break]
                (compile-do context (make-doable success))
                (when fail [[:line "#else" :line]
                            (compile-do context (make-doable fail))])
                ["#endif"])))


(defn- compile-pre-cond
  [{:keys [visit state] :as context} [_ & clauses]]
  {:pre [even? (count clauses)]}
  (into [:group]
        (concat (map-indexed (fn [k [test expr]]
                               (let [expr-pp (compile-do context (make-doable expr))]
                                 (concat (into [:span]
                                               (if (= :else test)
                                                 ["#else"]
                                                 (let [test-pp (fipp.visit/visit (GLSLPrinter. (assoc state :preprocessor? true)) test)]
                                                   (if (zero? k)
                                                     ["#if " test-pp]
                                                     ["#elif " test-pp]))))
                                         [:break]
                                         expr-pp)))
                             (partition 2 clauses))
                ["#endif"])))

(defn- compile-set!
  [{:keys [visit] :as context} [_ dest source]]
  [:span (compile-symbol context dest) " = " (visit source) ])

(defn- compile-inc
  [context [_ n]]
  [:span (compile-symbol context n) "++"])

(defn- compile-dec
  [context [_ n]]
  [:span (compile-symbol context n) "--"])

(defn- compile-field
  [context [_ n access]]
  [:span (compile-symbol context n) "." (str access)])

(defn- compile-swizzle
  [context [_ n & elements]]
  [:span (compile-symbol context n) "." (string/join (map str elements))])

(defn- compile-default
  [{:keys [visit] :as context} [n & args]]
  (into [:group] (concat [(compile-symbol context n)]
                         ["("]
                         (interpose ", " (map visit args))
                         [")"])))

(defn- infix
  [op]
  (fn [{:keys [visit]} [_ & args]]
    (into [:span]
          (interpose (str " " op " ") (map visit args)))))

(defn- compile-form
  [context x]
  (let [compile-fn
        (condp = (first x)
          'defn compile-defn
          'def compile-def
          'def-varying compile-def-varying
          'def-uniform compile-def-uniform
          'def-attribute compile-def-attribute
          'when compile-when
          'if compile-if
          'pre.ifdef compile-pre-ifdef
          'pre.if compile-pre-if
          'pre.cond compile-pre-cond
          'pre.include compile-pre-include
          'and (infix "&&")
          'or (infix "||")
          '= (infix "==")
          '+ (infix "+")
          '- (infix "-")
          '/ (infix "/")
          '* (infix "*")
          '> (infix ">")
          '>= (infix ">=")
          '< (infix "<")
          '<= (infix "<=")
          'set! compile-set!
          'bit-and (infix "&")
          'bit-or (infix "|")
          'bit-xor (infix "^")
          'bit-not (infix "~")
          'bit-shift-right (infix ">>")
          'bit-shift-left (infix "<<")
          'inc compile-inc
          'dec compile-dec
          'field compile-field
          'swizzle compile-swizzle
          'do compile-do
          compile-default)]
    (compile-fn context x)))

(defn pretty
  [x]
  (fipp.visit/visit (GLSLPrinter. {}) x))

(defn locations
  [form]
  (let [locs (atom {:attributes []
                    :uniforms []})
        add! (fn [k [_ t loc]] (swap! locs update k conj 
                                      {:type (str t)
                                       :location (str loc)
                                       :gl-location (csk/->camelCase (str loc))}))]
    (clojure.walk/prewalk
      (fn [x] 
        (when (and (list? x) (contains? #{'def-attribute 'def-varying 'def-uniform} (first x)))
          (condp = (first x)
            'def-attribute (add! :attribute x)
            'def-uniform (add! :uniform x)))
        x)
      form)
    @locs))

(defn ->glsl
  "Converts form to GLSL"
  [form]
  (let [pretty-data (pretty form)]
    (with-out-str
      (fipp/pprint-document pretty-data {:width 40}))))
