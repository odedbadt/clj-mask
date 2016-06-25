(ns mask.point
	(:require [clojure.spec :as s]))

(s/def ::x integer?)

(s/def ::y integer?)

(s/def ::point (s/keys :req-un [::x ::y]))

(defrecord Point [x y])
