(ns syntax-case.psyntax
  (:require [clojure.pprint :refer (cl-format)]))

;;-- procedures required for psyntax implementation --

;; TODO: decide whether nil is a sufficient unspecified value 
(defn void 
  "Returns the canonical unspecified value.
   For now, it returns nil, which may not be unspecified enough."
  []
  nil)

;; the original implementation requires the following procedures with
;; semantic equivalents in clojure:
;;    andmap -> every?
;;    ormap  -> some
;; we could alias them, but choose to use their clojure names

;;-- procedures required for psyntax expansion --

;; would be nice not to have to forward declare things
(declare sc-expand)
;; would be nice to have the power to change the evaluator
(defn sc-eval
  "expands x to core language forms using sc-expand and evaluates the
   resulting expression."
  [x]
  (eval (sc-expand x)))

;; NB: written according to psyntax.ss comments, but the cl-format call 
;; NB: seems volatile
(defn error
  "Throws an exception, identifying who threw it and printing a 
   formatted string with information for why the error occurred and what
   caused it."
  [who fmt why what]
  (throw (Exception. (str "error in " who ": " 
                       (cl-format fmt why what)))))

;; roll our own gensym because clojure's built-in doesn't have an 
;; associated predicate
(def ^:private sc-gensym-id (atom 0))
;; NB: maybe be smarter about sc-gensym's metadata tag
(def ^:private sc-gensym-tag :sc-gensym)

(defn sc-gensym
  "Creates a gensym. Optionally takes base identifier."
  ([] (sc-gensym "g"))
  ([base-id] 
   ;; scheme requires that base-id be a string, but we'll allow 
   (let [id @sc-gensym-id]
     (swap! sc-gensym-id inc)
     (with-meta (symbol (str base-id id)) {sc-gensym-tag true}))))

(defn sc-gensym? 
  "Returns true if genny is a gensym created by sc-gensym. Otherwise,
   returns false."
   [genny]
   (not (nil? (get (meta genny) sc-gensym-tag))))

(defn putprop 
  "Associates a property key with value val to the symbol sym that can
   be retrieved using getprop."
  [sym key val]
  (with-meta sym {key val}))

(defn getprop 
  "Retrieves the value for property key from symbol sym as set by 
   putprop. If no such property exists for sym, returns false."
  [sym key]
  (let [prop (get (meta sym) key)]
    (and (not (nil? prop)) prop)))

(defn remprop 
  "Removes property key from symbol sym."
  [sym key]
  (with-meta sym (dissoc (meta sym) key)))
