(ns mask.rect
	(:require [clojure.spec :as s] 
		      [mask.point :as point]))

(s/def ::topLeft :mask.point/point)

(s/def ::bottomRight :mask.point/point)

(s/def ::rectangle (s/keys :req-un [::topLeft ::bottomRight]))

(defrecord Rectangle [topLeft bottomRight])
