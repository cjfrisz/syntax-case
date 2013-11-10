;; open problems:
;;   - writing the pattern matching algorithm
;;   - finishing out the expanders for core forms 
;;      (probably all of clojure.core)
;;   - making sure I understand how this works in user land, i.e. that
;;      syntax-case can be the body of a defmacro and it 'just works'
;;   - bootstrap the expander with an existing version of syntax-case (?)
;;   - come up with a definition for the syntax form
;;      * related problem: implement #' for syntax-quote as reader macro?
;;   - figuring out whether the expander needs the meta environment since
;;      there aren't locally-bound macros in Clojure's core language
;;   - which functions in the algorithm get exported vs. which are local

(ns syntax-case.core
  (:require [syntax-case.util :refer [defrecord+]]))

;;------------------------------
;; SECTION 3: Representations
;;------------------------------

(defrecord+ syntax-object [expr wrap])

(defrecord+ mark [])

(defrecord+ subst [sym mark* label])

(defrecord+ label [])

(defrecord+ binding [type value])

;;----------------------------------------
;; SECTION 4: Producing expander output
;;----------------------------------------

(def- n (atom 0))
(defn gen-var
  [id]
  (swap! n inc)
  (let [name (syntax-object-expr id)]
    (symbol (str name "." n))))

;;----------------------------------------
;; SECTION 5: Stripping syntax objects
;;----------------------------------------

;; Clojure's notion of equality is different from Scheme's,
;; e.g. (identical? 'a 'a) evaluates to false in Clojure, while 
;; (eq? 'a 'a) evaluates to #t in Scheme.
(defn eq?
  [a b]
  (if (and (symbol? a) (symbol? b))
      (= a b)
      (identical? a b)))

(defn strip
  [x]
  (cond
    (syntax-object? x) (if (top-marked? (syntax-object-wrap x))
                           (syntax-object-expr x)
                           (strip (syntax-object-expr x)))
    ;; NB: this test is pair? in the paper; don't have pairs in Clojure
    ;; NB: Clojure syntax built with more than lists a la Scheme
    ((some-fn list? vector? map? set?) x) (let [f (strip (first x))
                                                n (strip (next x))]
                                            (if (eq? f n)
                                                x
                                                (conj n f)))
    :else x))

(def top-mark (make-mark))

(defn top-marked?
  [wrap]
  (and (not (nil? (seq wrap)))
       (or (identical? (first wrap) top-mark)
           (top-marked? (next wrap)))))

;;------------------------------
;; SECTION 6: Syntax errors
;;------------------------------

(defn syntax-error
  [object message]
  (throw (Exception. (str message (strip object)))))

;;-----------------------------------
;; SECTION 7: Structural predicates
;;-----------------------------------

(defn identifier?
  [x]
  (and (syntax-object? x)
       (symbol? (syntax-object-expr x))))

;; Clojure also has self-evaluating keywords
(def self-evaluating? 
  (some-fn (partial instance? Boolean) string? char? keyword?))

;;------------------------------
;; SECTION 8: Creating wraps
;;------------------------------

(defn add-mark
  [mark x]
  (extend-wrap (list mark) x))

(defn add-subst
  [id label x]
  (extend-wrap
   (list (make-subst
           (syntax-object-expr id)
           (wrap-marks (syntax-object-wrap id))
           label))
   x))

(defn extend-wrap
  [wrap x]
  (if (syntax-object? x)
      (make-syntax-object
        (syntax-object-expr x)
        (join-wraps wrap (syntax-object-wrap x)))
      (make-syntax-object x wrap)))

(defn join-wraps
  [wrap1 wrap2]
  (cond
    (nil? (seq wrap1)) wrap2
    (nil? (seq wrap2)) wrap1
    :else (letfn [(f [w w*]
                    (if (nil? (seq w*))
                        (if (and (mark? w) (eq? (first wrap2) w))
                            (next wrap2)
                            (conj wrap2 w))
                      (conj (f (first w*) (next w*)) w)))]
            (f (first wrap1) (next wrap1)))))

;;----------------------------------------
;; SECTION 9: Manipulating environments
;;----------------------------------------

(defn extend-env [label binding env] (assoc env label binding))

;;-----------------------------------
;; SECTION 10: Identifier resolution
;;-----------------------------------

(defn id-binding
  [id r]
  (label-binding id (id-label id) r))

(defn id-label
  [id]
  (let [sym (syntax-object-expr id)
        wrap (syntax-object-wrap id)]
    ;; this is a named let called search in the paper
    (loop [wrap wrap
           mark* (wrap-marks wrap)]
      (if (nil? (seq wrap))
          (syntax-error id "undefined identifier")
          (let [w0 (first wrap)]
            (if (mark? w0)
                (recur (next wrap) (next mark*))
                (if (and (eq? (subst-sym w0) sym)
                         (same-marks? (subst-mark* w0) mark*))
                    (subst-label w0)
                    (recur (next wrap) mark*))))))))

(defn wrap-marks
  [wrap]
  (if (nil? (seq wrap))
      nil
      (let [w0 (first wrap)]
        (if (mark? w0)
            (conj (wrap-marks (next wrap)) w0)
            (recur (next wrap))))))

(defn same-marks?
  [m1* m2*)
  (if (nil? (seq m1*))
      (nil? (seq m2*))
      (and (not (nil? (seq m2*)))
           (eq? (first m1*) (first m2*))
           (recur (next m1*) (next m2*)))))

(defn label-binding
  [id label r]
  (or (get r label) 
      (syntax-error id "displaced lexical")))

;;------------------------------
;; SECTION 11: The expander
;;------------------------------

;; TODO: exp depends on syntax-case...figure that out
;; NB: might omit mr (meta-rho) since Clojure doesn't use locally-bound macros

;; the Scheme version from the paper:
;; (define exp
;;   (lambda (x r mr)
;;     (syntax-case x ()
;;       [id
;;        (identifier? #'id)
;;        (let ([b (id-binding #'id r)])
;;          (case (binding-type b)
;;            [(macro) (exp (exp-macro (binding-value b) x) r mr)]
;;            [(lexical) (binding-value b)]
;;            [else (syntax-error x "invalid syntax")]))]
;;       [(e0 e1 ...)
;;        (identifier? #'e0)
;;        (let ([b (id-binding #'e0 r)])
;;          (case (binding-type b)
;;            [(macro) (exp (exp-macro (binding-value b) x) r mr)]
;;            [(lexical)
;;             `(,(binding-value b) ,@(exp-exprs #'(e1 ...) r mr))]
;;            [(core) (exp-core (binding-value b) x r mr)]
;;            [else (syntax-error x "invalid syntax")]))]
;;       [(e0 e1 ...) `(,(exp #'e0 r mr) ,@(exp-exprs #'(e1 ...) r mr))]
;;       [_ (let ([d (strip x)])
;;            (if (self-evaluating? d)
;;                d
;;                (syntax-error x "invalid-syntax")))])))

;; sketch of what the Clojure version might look like
;; (defn exp
;;   [x r mr]
;;   (syntax-case x ()
;;     [id
;;      (identifier? #'id)
;;      (let [b (id-binding #'id r)]
;;        (case (binding-type b)
;;          :macro   (exp (exp-macro (binding-value b) x) mr)
;;          :lexical (binding-value b)
;;          (syntax-error x "invalid syntax")))]
;;     [(e0 e1 ...)
;;      (identifier? #'e0)
;;      (let [b (id-binding #'e0 r)]
;;        (case (binding-type b)
;;          :macro   (exp (exp-macro (binding-value b) x) mr)
;;          :lexical `(~(binding-value b) ~@(exp-exprs #'(e1 ...) r mr))
;;          :core (exp-core (binding-value b) x r mr)
;;          (syntax-error x "invalid syntax")))]
;;     [(e0 e1 ...) `(~(exp #'e0 r mr) ~@(exp-exprs #'(e1 ...) r mr))]
;;     [_ (let [d (strip x)]
;;          (if (slf-evaluating? d)
;;              d
;;              (syntax-error x "invalid-syntax")))]))

(defn exp-macro
  [p x]
  (let [m (make-mark)]
    (add-mark m (p (add-mark m x)))))

;; NB: similar to exp, might omit mr since no letrec-syntax
(defn exp-core [p x r mr] (p x r mr))

;; NB: similar to exp and exp-core, might omit mr since no letrec-syntax
(defn exp-exprs [x* r mr] (map (lambda (x) (exp x r mr)) x*))

;;------------------------------
;; SECTION 12: Core transformers
;;------------------------------

;; all the following require a definition of syntax-case
;; until that's figured out they'll remain commented-out

;; NB: maybe omit mr (meta-rho)
(defn exp-quote
  [x r mr]
  (syntax-case x ()
    [(_ d) `(quote ~(strip #'d))]))

(defn exp-if
  [x r mr]
  (syntax-case x ()
    [(_ e1 e2 e3) `(if ~(exp #'e1 r mr)
                       ~(exp #'e2 r mr)
                       ~(exp #'e3 r mr))]))

;; single var/body-expression fn per the paper
;; extend for multiple variables and body expressions
;; also debate on internal defs per semantic difference in Scheme
(defn exp-fn
  [x r mr]
  (syntax-case x ()
    [(_ [var] body)
     (let [label (make-label)
           new-var (gen-var #'var)]
       `(fn [~new-var]
          ~(exp (add-subst #'var label #'body)
                (extend-env label
                  (make-binding 'lexical new-var)
                  r)
                mr)))]))

;; single var/body-expression let per the paper
(defn exp-let
  [x r mr]
  (syntax-case x ()
    [(_ [var expr] body)
     (let [label (make-label)
           new-var (gen-var #'var)]
       `(let [~new-var ~(exp #'expr r mr)]
          ~(exp (add-subst #'var label #'body)
                (extend-env label
                  (make-binding 'lexical new-var)
                  r)
                mr)))]))

;; omitting exp-letrec-syntax since Clojure doesn't have it

;;-------------------------------------------------------
;; SECTION 13: Parsing and constructing syntax objects
;;-------------------------------------------------------

;; NB: Clojure doesn't have pairs; is this a proper translation?
(defn syntax-list? [x] (list? (syntax-object-expr x)))

(defn syntax-first
  [x]
  (extend-wrap
    (syntax-object-wrap x)
    (first (syntax-object-expr x))))

(defn syntax-next
  [x]
  (extend-wrap
    (syntax-object-wrap x)
    (next (syntax-object-expr x))))

;; handles constant input to syntax, but not pattern variables and
;; ellipses
(defn exp-syntax
  [x r mr]
  (syntax-case x ()
    [(_ t) `(quote ~#'t)]))

;;-----------------------------------
;; SECTION 14: Comparing identifiers
;;-----------------------------------

(defn free-identifier=?
  [x y]
  (eq? (id-label x) (id-label y)))

(defn bound-identifier=?
  [x y]
  (and (eq? (syntax-object-expr x) (syntax-object-expr y))
       (same-marks?
         (wrap-marks (syntax-object-wrap x))
         (wrap-marks (syntax-object-wrap y)))))

;;------------------------------
;; SECTION 15: Conversions
;;------------------------------

(defn datum->syntax
  [template-id x]
  (make-syntax-object x (syntax-object-wrap template-id)))

(def syntax->datum strip)

;;-----------------------------------
;; SECTION 16: Starting expansion
;;-----------------------------------

(defn expand
  [x]
  (let [[wrap env] (initial-wrap-and-env)]
    (exp (make-syntax-object x wrap) env env)))

;; the following will need serious reworking
;; omitting letrec-syntax
(def ^:private id-binding*
  ;; NB: somehow maybe use keywords instead?
  (reduce (fn [id-bind* [id type exp]]
            (assoc id-bind* id (make-binding type exp)))
    {} 
    [['quote              :core    exp-quote]
     ['if                 :core    exp-if]
     ['fn                 :core    exp-fn]
     ['let                :core    exp-let]
     ['identifier?        :lexical 'identifier?]
     ['free-identifier=?  :lexical 'free-identifier=?]
     ['bound-identifier=? :lexical 'bound-identifier=?]
     ['datum->syntax      :lexical 'datum->syntax]
     ['syntax->datum      :lexical 'syntax->datum]
     ['syntax-list?       :lexical 'syntax-pair?]
     ;; replace syntax-first, syntax-next, and syntax with syntax-case
     ;; when syntax-case is bootstrapped
     ['syntax-first       :lexical 'syntax-first]
     ['syntax-next        :lexical 'syntax-next]
     ['syntax             :core    exp-syntax]
     ['list               :core    'list]]))
   
(defn initial-wrap-and-env []
  (let [label* (map #(make-label) id-binding*)]
    [`(~@(map (fn [sym label]
                (make-subst sym (list top-mark) label))
           (keys id-binding*)
           label*)
       ~top-mark)
     (map first label* (vals id-binding*))]))
