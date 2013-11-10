(ns syntax-case.util
  (:require [clojure.string :refer [lower-case]]))

(defn- make-name
  [& elt*]
  (symbol (apply str (interpose "-" elt*))))

(defmacro defrecord+
  [name field* & spec*]
  `(do
     (let [type-tag# (keyword (gensym ~name))]
       (defn ~(symbol (str "make-" name))
         ~(vec field*)
         ~(-> (reduce assoc {} (map keyword field*) field*)
              (assoc type-tag# nil)))
       (defn ~(symbol (str name "?")) 
         [v#]
         (some #{type-tag#} (keys v#))))
     ~@(for [field field*]
         `(def ~(make-name name field) ~(keyword field)))
     ~@(for [field field*]
         `(defn ~(make-name name field "set"))
            [record# new-val#]
            (assoc record# ~(keyword field) new-val#))))
