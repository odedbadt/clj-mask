(ns mask.util
	(:require [clojure.spec :as s]))

(s/fdef strictly-monotone-args? 
	:args (s/* integer?)
	:ret boolean?)

(defn strictly-monotone-args?
	([] true)
	([a] true)
	([a & more] (and (< a (first more)) (apply strictly-monotone-args? more))))

(s/fdef strictly-monotone? 
	:args (s/cat :coll coll?)
	:ret boolean?)

(defn strictly-monotone? [coll] (apply strictly-monotone-args? coll))

(defn empty-to-nil [coll]
	(if (empty? coll) nil coll))