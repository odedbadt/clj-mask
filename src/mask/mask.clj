(ns mask.mask
  (:require [mask.point :as point]
            [mask.rect :as rect]          
            [mask.util :as util]
            [clojure.spec :as s]
            [clojure.spec.gen :as gen]))

(s/def ::bounding-box :mask.rect/rectangle)
(s/def ::short-scanline (fn [[x y & more]] (and (nil? more) (< x y) (integer? x) (integer? y))))
(def positive-integer (s/int-in 1 Integer/MAX_VALUE))
(def integer (s/int-in Integer/MIN_VALUE Integer/MAX_VALUE))
(def positive-integer-even-length-list-generator
  (s/gen (s/cat :t0 integer? :f0 positive-integer :rest (s/* (s/cat :f positive-integer :t positive-integer)))))

(def monotone-even-length-list-generator
  (gen/fmap (comp vec (partial reductions +)) positive-integer-even-length-list-generator))


(s/def ::scanline (s/with-gen (s/and util/strictly-monotone? (comp even? count))
                               (fn [] monotone-even-length-list-generator)))
(s/def ::heightline (s/with-gen
                      (s/and sorted? (s/coll-of (s/tuple integer? integer?) (sorted-map)))
                      (fn []
                        (gen/such-that (fn [candidate] (= 0 (reduce + (vals candidate))))
                          (gen/fmap (partial apply sorted-map) 
                            (s/gen (s/* (s/cat :f integer? :t (s/int-in -10 10)))))))))

(s/def ::alternations (s/nilable (s/and (s/map-of integer? ::scanline))))

(s/def ::mask (s/or :empty nil?
          :non-empty (s/keys :req [::alternations ::bounding-box])))

(s/fdef calculate-bounding-box
  :args (s/cat :alternations ::alternations)
  :ret :mask.rect/rectangle)
(defn calculate-bounding-box [alternations]
  (let [y-values (keys alternations)
        x-value-arrays (vals alternations) 
        top-most (first y-values)
        bottom-most (last y-values)
        l (apply min (map first x-value-arrays))
        r (apply max (map last x-value-arrays))]
        (rect/->Rectangle (point/->Point l top-most) (point/->Point r (inc bottom-most)))))

(s/fdef tosorted
  :args (s/cat :map map?)
  :ret (s/and map? sorted?))
(defn tosorted [m] (if (sorted? m) m (into sorted-map m)))

; (s/fdef create
;    :args (s/cat :alternations ::alternations)
;    :ret ::mask)
; (defn create [alternations] 
;   (if (empty? alternations)
;     nil
;     (let sorted-alternations (into (sorted-map) alternations)
;     {::alternations sorted-alternations
;      ::bounding-box (calculate-bounding-box sorted-alternations)})))
(defn in-scanline? [s x] 
  (odd? (count (filter (partial >= x) s))))

(s/fdef merge-scanlines
  :args (s/cat :s1 ::scanline :s2 :scanline :offset (s/? integer?))
  :ret ::scanline)
(defn merge-scanlines-trivial
  ([s1 s2 & [offset]]
    (let [offset (or offset 0)
        f (min (first s1) (+ (first s2) offset))
        t (max (last s1) (+ (last s2) offset))
        
        [last-x-in-merged output]
          (reduce
          (fn [[prev-x-in-merged output] x]
            (let [x-in-s1 (in-scanline? s1 x)
                x-in-s2 (in-scanline? s2 (- x offset))
                x-in-merged (or x-in-s1 x-in-s2)
                next-output (if (not= x-in-merged prev-x-in-merged) (conj output x)
                                                                     output)]
                ;next-output (if x-in-merged (conj output x) output)]
                [x-in-merged next-output]))
          [false []]
          (range f t))]
        (if last-x-in-merged (conj output t) output))))

(s/fdef eq-scanline
  :args (s/cat :a ::scanline :b ::scanline :offset (s/? integer?)))
(defn eq-scanline
  ([a b] (= a b))
  ([a b offset]
    (= a (map (partial + offset) b)
    )))

(s/fdef contains-scanline-trivial?
  :args (s/cat :a ::scanline :b ::scanline :offset (s/? integer?))
  :ret ::scanline)
(defn contains-scanline-trivial?
  ([a b] (contains-scanline-trivial? a b 0))
  ([a b offset]
    (let [l (count b)
          evens-of-b (map (partial nth b) (range 0 l 2))
          odds-of-b (map (partial nth b) (range 1 l 2))
          values-to-test (vec (concat evens-of-b (map dec odds-of-b)))
          ]
      (every? (partial in-scanline? a)
        (map (partial + offset) values-to-test))
    )))


(s/fdef merge-short-scanlines
  :args (s/cat :s1 ::short-scanline :s2 :short-scanline :offset (s/? integer?))
  :ret ::scanline)
(defn merge-short-scanlines [s1 s2] nil)


(defn merge-scanline-internal [s1 s2 offset l1 l2 i1 i2 lit output is-empty dbg]
  (cond (and (< i1 l1)
             (or (>= i2 l2)
                 (and (< i2 l2)
                      (< (nth s1 i1) (+ offset (nth s2 i2))))))
          (if (even? i1)
              (if (= lit 0)
                   (merge-scanline-internal s1 s2 offset l1 l2 (inc i1) i2 1 (conj output (nth s1 i1)) false :a)
                   (merge-scanline-internal s1 s2 offset l1 l2 (inc i1) i2 (inc lit) output is-empty :b))
              (if (= lit 1)
                   (merge-scanline-internal s1 s2 offset l1 l2 (inc i1) i2 0 (conj output (nth s1 i1)) false :c)
                   (merge-scanline-internal s1 s2 offset l1 l2 (inc i1) i2 (dec lit) output is-empty :d)))
        (and (< i1 l1)
             (< i2 l2)
             (= (nth s1 i1) (+ offset (nth s2 i2))))
          (if (= (even? i1) (even? i2))
              (merge-scanline-internal s1 s2 offset l1 l2 (inc i1) (inc i2) (+ (mod (inc i1) 2)
                                                                                (mod (inc i2) 2))
                                                                             (conj output (nth s1 i1))
                                                                             false :e)
              (merge-scanline-internal s1 s2 offset l1 l2 (inc i1) (inc i2) lit output is-empty :f))
        (< i2 l2)
          (if (even? i2)
              (if (= lit 0)
                  (merge-scanline-internal s1 s2 offset l1 l2 i1 (inc i2) 1 (conj output (+ offset (nth s2 i2))) false :g)
                  (merge-scanline-internal s1 s2 offset l1 l2 i1 (inc i2) (inc lit) output is-empty :h))
              (if (= lit 1)
                  (merge-scanline-internal s1 s2 offset l1 l2 i1 (inc i2) 0 (conj output (+ offset (nth s2 i2))) false :i)
                  (merge-scanline-internal s1 s2 offset l1 l2 i1 (inc i2) (dec lit) output is-empty :j))
              )
        true (if is-empty nil output)))



(s/fdef merge-scanlines
  :args (s/cat :s1 ::scanline :s2 ::scanline :offset (s/? integer?))
  :ret ::scanline
  :fn (fn [{{s1 :s1 s2 :s2 offset :offset} :args ret :ret}] 
    (eq-scanline ret (merge-scanlines-trivial s1 s2 offset))))
(defn merge-scanlines [s1 s2 & [offset]] 
  (let [offset (or offset 0)]
    (merge-scanline-internal s1 s2 offset (count s1) (count s2) 0 0 0 [] true 0)))
(defn bump [off-y a]
  (into (sorted-map) (map (fn [[y-a scanline]] [(+ y-a off-y) scanline]) a)))

(defn contains-trivial?
  ([a b] (contains-trivial? a b {:x 0 :y 0}))
  ([a b {off-x :x off-y :y}]
    (cond
      (nil? b) true
      (nil? a) (nil? b)
      true (every? true?
             (for [[y scanline-b] b]
               (contains-scanline-trivial? (get a (- y off-y)) scanline-b off-x))))))

(defn eq-alternations
  ([a b] (= a b))
  ([a b {off-x :x off-y :y}]
  (every?
    (fn [[y-b scanline-b]]
      (eq-scanline (a (+ y-b off-y)) scanline-b off-x))
    b)))

(s/fdef merge-alternations
  :args (s/cat :a ::alternations :b ::alternations :offset (s/? ::point/point))
  :ret ::alternations
  :fn (fn [{{s1 :s1 s2 :s2 offset :offset} :args ret :ret}] 
        (and (contains-trivial? ret s1)
             (contains-trivial? ret s1 offset))))
(defn merge-alternations 
  ([a b] (merge-with merge-scanlines a b))
  ([a b {off-x :x off-y :y}]
    (let [bumped-b (bump off-y b)]
      (util/empty-to-nil
        (merge-with (fn [s1 s2] (merge-scanlines s1 s2 off-x) a bumped-b))))))
(s/instrument-ns)


;   opt_offsetX = opt_offsetX || 0;
;   opt_offsetY = opt_offsetY || 0;
;   let alternations = {};
;   for (var y in alternationsA) {
;     let mergedLine = rowMergingFunction(alternationsA[Number(y)],
;         alternationsB[Number(y) - opt_offsetY] || null, opt_offsetX);
;     if (mergedLine) {
;       alternations[y] = mergedLine;
;     }
;   }
;   for (y in alternationsB) {
;     let mergedLine = rowMergingFunction(
;         alternationsA[Number(y) + opt_offsetY]  || null,
;         alternationsB[Number(y)], opt_offsetX);
;     if (mergedLine) {
;       alternations[Number(y) + opt_offsetY] = mergedLine;
;     }        
;   }  
;   return alternations;

; (s/fdef merge
;   :args (s/cat :maskA ::mask :maskB ::mask (s/? :offset (s/? ::point/point))
;   :ret (s/mask))
; (defn merge ([maskA maskB]
;   (cond
;      (and (nil? maskA) (nil? maskB))
;          nil
;      (nil? maskA)
;          (merge-alternations merge-mask-lines {} (:alternations maskB))

;  & offset] {
;   if (!maskA && !maskB) {
;     return null;
;   }
;   if (!maskA) {
;     return create(mergeAlternations(mergeMaskLines, {}, maskB.alternations, opt_offset && opt_offset.x, opt_offset && opt_offset.y));
;   }
;   if (!maskB) {
;     return maskA;
;   }
;   return create(mergeAlternations(mergeMaskLines, maskA.alternations, maskB.alternations, opt_offset && opt_offset.x, opt_offset && opt_offset.y));
; })



; export function offset(offset, mask) {
;   return merge(null, mask, offset);
; };
; /**
;  * @param {function(Array<number>, Array<number>, (number|undefined)): Array<number>} rowMergingFunction
;  * @param {Alternations} alternationsA
;  * @param {Alternations} alternationsB
;  * @param {number=} opt_offsetX
;  * @param {number=} opt_offsetY
;  * @return {Alternations}
;  * @private
;  */
; function mergeAlternations(rowMergingFunction, alternationsA, alternationsB, opt_offsetX, opt_offsetY) {
;   opt_offsetX = opt_offsetX || 0;
;   opt_offsetY = opt_offsetY || 0;
;   let alternations = {};
;   for (var y in alternationsA) {
;     let mergedLine = rowMergingFunction(alternationsA[Number(y)],
;         alternationsB[Number(y) - opt_offsetY] || null, opt_offsetX);
;     if (mergedLine) {
;       alternations[y] = mergedLine;
;     }
;   }
;   for (y in alternationsB) {
;     let mergedLine = rowMergingFunction(
;         alternationsA[Number(y) + opt_offsetY]  || null,
;         alternationsB[Number(y)], opt_offsetX);
;     if (mergedLine) {
;       alternations[Number(y) + opt_offsetY] = mergedLine;
;     }        
;   }  
;   return alternations;
; };


; /**
;  * @param {Alternations} alternations
;  * @param {Rectangle=} opt_boundingBox
;  * @param {boolean=} opt_isSimple
;  * @return {Mask}
;  */
; export function create(alternations, opt_boundingBox, opt_isSimple) {
;   if (Object.keys(alternations).length == 0) {
;     return null;
;   }
;   return {
;     alternations: alternations,
;     boundingBox : opt_boundingBox || calculateBoundingBox(alternations),
;     isSimple: opt_isSimple || calculateIsSimple(alternations) 
;   }
; };

; /**
;  * @return {Mask}
;  */
; function empty() {
;   return create({});
; };

; /** @return {Rectangle} */
; export function calculateBoundingBox(alternations) {
;   let fromX = Infinity;
;   let fromY = Infinity;
;   let toX = -Infinity;
;   let toY = -Infinity;
;   for (var y in alternations) {
;     if (Number(y) < fromY) {
;       fromY = Number(y);
;     }
;     if (Number(y) + 1 > toY) {
;       toY = Number(y) + 1; 
;     }
;     if (alternations[y].length == 0) {
;       continue;
;     }
;     if (alternations[y][0] < fromX) {
;       fromX = alternations[y][0];
;     }
;     if (alternations[y][alternations[y].length - 1] > toX) {
;       toX = alternations[y][alternations[y].length - 1];
;     }
;   }
;   return {fromX:fromX, fromY:fromY, toX:toX, toY:toY};
; };

; /** @return {boolean} */
; function calculateIsSimple(alternations) {
;   for (var y in alternations) {
;     if (alternations[y].length != 2) {
;       return false;
;     }
;   }
;   return true;
; };


; /** @return {Mask} */
; export function clip(mask, rectangle) {
;   if (!rectangle || !mask) {
;     return null;    
;   }
;   if (includesRect(rectangle, mask.boundingBox)) {
;     return mask;
;   }
;   let alternations = {};
;   for (var y in mask.alternations) {
;     if (y < rectangle.fromY || y >= rectangle.toY) {
;       continue;
;     }
;     let outputAlternations = clipMaskLine(
;         mask.alternations[y], rectangle.fromX, rectangle.toX);

;     if (outputAlternations) {
;       alternations[y] = outputAlternations;
;     }
;   }
;   return create(alternations);
; };


; /** @return {Mask} */
; export function line(cursor, fromX, fromY, toX, toY) {
;   //return merge(null, cursor, {x: fromX, y: fromY});
;   if (!cursor) {
;     return null;
;   }
;   if (arguments.length == 3 && fromX != null && fromY != null) {
;     return line(cursor, fromX.x, fromX.y, fromY.x, fromY.y)
;   }
;   return lineInternal(
;     cursor.isSimple ? mergeSimpleMaskLines : mergeMaskLines,
;     cursor, fromX, fromY, toX, toY);
; };


; /** @return {Mask} */
; export function line_1(cursor, fromX, fromY, toX, toY) {  
;   if (!cursor) {
;     return null;
;   }
;   if (arguments.length == 3 && fromX != null && fromY != null) {
;     return line(cursor, fromX.x, fromX.y, fromY.x, fromY.y)
;   }
;   //return line0(cursor, fromX, fromY, toX, toY);
;   let dy = toY - fromY;
;   let h = rectangle.height(cursor.boundingBox);
;   let top = cursor.boundingBox.fromY;
;   let bottom = cursor.boundingBox.toY;
;   if (fromX > toX || dy < h) {
;     return line0(cursor, fromX, fromY, toX, toY);
;   }
;   let grad = (toX - fromX) / (toY - fromY);
;   let sampler = line0(cursor, 0, 0, grad * h * 2, h * 2);
;   let alternations = {};
;   let shifter = function(s) { return x => x + s}
;   startShifter = shifter(fromX);
;   for (var t = 0; t < h; ++t) {
;     alternations[fromY + top + t] = 
;       sampler.alternations[top + t].map(startShifter);
;   }
;   for (var t = 0; t < toY - fromY - h; ++t) {
;     alternations[fromY + top + h + t] = 
;       sampler.alternations[top + h].map(shifter(Math.floor(fromX + t * grad)));
;   }
;   let endShifter = shifter(Math.floor(toX - grad * h * 2));
;   for (var t = 0; t < h; ++t) {
;     alternations[toY + bottom - h + t] =
;       sampler.alternations[bottom + h + t].map(endShifter);
;   }
;   /*
;   for (var t = 0; t < h; ++t) {
;     alternations[toY + bottom - h + t] = goog.array.map(
;       sampler.alternations[h + t], shifter(fromX + (toY - fromY) * grad));
;   }
;   for (var y = 0; y < toY - fromY - 2*h; ++y) {
;     alternations[y + fromY + h] = sampler.alternations[y - fromY].map(shifter(y));
;   }
;   for (var y = toY - h; y < toY; ++y) {
;     alternations[y] = sampler.alternations[y - (toY - h)].map(shifter(y));
;   }*/
;   return create(alternations);
; }

; /**
;  * @param {function(Array<number>, Array<number>, (number|undefined)): ?Array<number>} rowMergingFunction
;  * @param {Mask} cursor
;  * @param {number} fromX
;  * @param {number} fromY
;  * @param {number} toX
;  * @param {number} toY
;  * @return {Mask}
;  * @private
;  */
; function lineInternal(rowMergingFunction, cursor, fromX, fromY, toX, toY) {
;   let resultAlternations = {};
;   let x = fromX;
;   let y = fromY;
;   let cursorAlternations = cursor.alternations;
;   if (fromX == toX && fromY == toY) {
;     return merge(null, cursor, createPoint(fromX, fromY));
;   }
;   if (Math.abs(toX - fromX) > Math.abs(toY - fromY)) {
;     if (fromX < toX) {
;       let dx = 1;
;       let dy = (toY - fromY) / (toX - fromX);
;       while (x <= toX) {
;         resultAlternations = mergeAlternations(rowMergingFunction,
;             resultAlternations, cursorAlternations, Math.round(x), Math.round(y));
;         x += dx;
;         y += dy;
;       }
;     } else {
;       let dx = -1;
;       let dy = -(toY - fromY) / (toX - fromX);
;       while (x >= toX) {
;         resultAlternations = mergeAlternations(rowMergingFunction,
;             resultAlternations, cursorAlternations, Math.round(x), Math.round(y));
;         x += dx;
;         y += dy;
;       }    
;     }
;   } else {
;     if (fromY < toY) {
;       let dx = (toX - fromX) / (toY - fromY);    
;       let dy = 1;
;       while (y <= toY) {
;         resultAlternations = mergeAlternations(rowMergingFunction,
;             resultAlternations, cursorAlternations, Math.round(x), Math.round(y));
;         x += dx;
;         y += dy;
;       }
;     } else {
;       let dx = -(toX - fromX) / (toY - fromY);    
;       let dy = -1;
;       while (y >= toY) {
;         resultAlternations = mergeAlternations(rowMergingFunction,
;             resultAlternations, cursorAlternations, Math.round(x), Math.round(y));
;         x += dx;
;         y += dy;
;       }    
;     }    
;   }
;   return create(resultAlternations);
; };



; /**
;  * Merges the mask with another mask
;  * @param {Array<number>} maskA
;  * @param {Array<number>} maskB
;  * @param {number=} opt_offset
;  * @return {Array<number>}
;  */
; function mergeMaskLines(maskA, maskB, opt_offset) {
;   let offset = opt_offset || 0;
;   if (!maskA && !maskB) {
;     return null;
;   }
;   if (!maskA) {
;     return maskB.map(x =>Number(x) + offset);
;   }
;   if (!maskB) {
;     return maskA;
;   }
;   if (maskA.length == 2 && maskB.length == 2) {
;     return mergeSimpleMaskLines(maskA, maskB, offset);
;   }
;   let iA = 0;
;   let iB = 0;
;   let lit = 0;
;   let output = [];
;   let isEmpty = true;
;   while (true) {
;     if (iA < maskA.length && (iB >= maskB.length ||
;         iB < maskB.length && maskA[iA] < maskB[iB] + offset)) {
;       // A is strictly smaller or B is done, take A.
;       if (iA % 2 == 0) {
;         if (lit == 0) {
;           output.push(maskA[iA]);
;           isEmpty = false;
;         }
;         lit++;
;       } else {
;         lit--;
;         if (lit == 0) {
;           output.push(maskA[iA]);
;           isEmpty = false;
;         }
;       }
;       iA++;
;     } else if (iA < maskA.length && iB < maskB.length &&
;         maskA[iA] == (maskB[iB] + offset)) {
;         // A and B are equal, take none, skip both pointers.
;         if (iA % 2 == iB % 2) {
;           output.push(maskA[iA]);
;           isEmpty = false;          
;           lit = (iA + 1) % 2 + (iB + 1) % 2;
;         }
;         iA++;
;         iB++;
;     } else if (iB < maskB.length) {
;       // take B.
;       if (iB % 2 == 0) {
;         if (lit == 0) {
;           output.push(maskB[iB] + offset);
;           isEmpty = false;
;         }
;         lit++;
;       } else {
;         lit--;
;         if (lit == 0) {
;           output.push(maskB[iB] + offset);
;           isEmpty = false;
;         }
;       }
;       iB++;
;     } else {
;       // Done.
;       break;
;     }
;   }
;   return isEmpty ? null : output;
; };

; export function mergeSimpleMaskLines(maskA, maskB, offset) {
;   if (!maskA && !maskB) {
;     return null;
;   }
;   if (!maskB) {
;     return maskA;
;   }
;   if (!maskA) {
;     return [maskB[0] + offset, maskB[1] + offset];
;   }
;   if (maskA[1] < maskB[0] + offset) {
;     return [maskA[0], maskA[1], maskB[0] + offset, maskB[1] + offset];
;   }
;   if (maskB[1] + offset < maskA[0]) {
;     return [maskB[0] + offset, maskB[1] + offset, maskA[0], maskA[1]];
;   }
;   return [Math.min(maskA[0], maskB[0] + offset),
;       Math.max(maskA[1], maskB[1] + offset)];
; };

; export function clipMaskLine(alternations, fromX, toX) {
;   if (!alternations) {
;     return null;
;   }
;   let outputAlternations = [];
;   let on = false;
;   let prevInBounds = false; // x[-1] = -Inf
;   for (var i = 0; i < alternations.length; ++i) {
;     let x = alternations[i];
;     if (!prevInBounds && x >= fromX) {
;       if (on) {        
;         if (x < toX) {
;           if (x > fromX) {
;             outputAlternations.push(fromX);
;             outputAlternations.push(x);
;           }
;         } else {
;           outputAlternations.push(fromX);
;           outputAlternations.push(toX);
;         }
;       } else if (x < toX  || (x <= toX && on)) {
;         outputAlternations.push(x);
;       }
;     }    
;     if (prevInBounds) {
;       if (x > toX) {
;         if (on) {
;           outputAlternations.push(toX);
;         }
;       } else if (x < toX || (x <= toX && on)) {
;         outputAlternations.push(x);        
;       }
;     }
;     prevInBounds = x >= fromX && x < toX;
;     if (x >= toX) {
;       break;
;     }
;     on = !on;
;   }
;   return outputAlternations.length == 0 ? null : outputAlternations;
; };


; export function lineContains(line, x) {
;   // TODO: binary search?
;   for (var i = 0; i < line.length; ++i) {
;     if (line[i] > x) {
;       return (i % 2) == 1;
;     }
;   }
; };


; /**
;  * @param {Point} location
;  * @param {Mask} mask
;  * @param {number=} opt_opffsetX
;  * @param {number=} opt_opffsetY
;  * @return {boolean}
;  */
; export function contains(location, mask, opt_offset) {
;   if (!mask) {
;     return false;
;   }
;   let row = mask.alternations[location.y - (opt_offset ? opt_offset.y : 0)];
;   if (!row) {
;     return false;    
;   }
;   return lineContains(row, location.x - (opt_offset ? opt_offset.x : 0));
; };


; export function fromBoolean(boundingBox, f) {
;   if (!boundingBox) {
;     return null;
;   }
;   let m = null;
;   let alternations = {};
;   for (var y = boundingBox.fromY; y < boundingBox.toY; ++y) {
;     let on = false;
;     let row = [];
;     for (var x = boundingBox.fromX; x < boundingBox.toX; ++x) {
;       if (on && !f(x, y)) {
;         row.push(x);
;         on = !on;
;       }
;       if (!on && f(x, y)) {
;         row.push(x);
;         on = !on;
;       }
;     }
;     if (on) {
;       row.push(boundingBox.toX);
;     }
;     if (row.length > 0) {
;       alternations[y] = row;
;     }
;   }
;   return create(alternations);
; };


; export function fromRectangle(rectangle) {
;   let m = null;
;   let alternations = {};
;   for (var y = rectangle.fromY; y < rectangle.toY; ++y) {
;     alternations[y] = [rectangle.fromX, rectangle.toX];
;   }
;   return create(alternations, rectangle);
; };


; export function scale(factor, mask) {
;   if (!mask) {
;     return null;
;   }
;   let alternations = {};
;   let boundingBox = mask.boundingBox;
;   let outputBox = createRectangle(
;     Math.floor(boundingBox.fromX * factor.x),
;     Math.floor(boundingBox.fromY * factor.y),
;     Math.floor(boundingBox.toX * factor.x),
;     Math.floor(boundingBox.toY * factor.y));
;   for (var y = outputBox.fromY; y < outputBox.toY; ++y) {
;     let inputRow = mask.alternations[Math.round(y / factor.y)]
;     if (!inputRow) {
;       continue;
;     }
;     let outputRow = [];
;     let outputRowPtr = 0;
;     for (var i = 0; i < inputRow.length; ++i) {
;       let outputX = Math.floor(inputRow[i] * factor.x);
;       if (outputRowPtr == 0 || outputX != outputRow[outputRowPtr - 1]) {
;         outputRow[outputRowPtr] = outputX;  
;         outputRowPtr++;
;       } else if (outputX == outputRow[outputRowPtr - 1]) {
;         outputRowPtr--;
;       }      
;     }
;     outputRow.length = outputRowPtr;
;     if (outputRow.length == 0) {
;       continue;
;     }
;     alternations[y] = outputRow;
;   }
;   return create(alternations, outputBox);
; };

; export function volume(mask) {
;   if (!mask) {
;     return 0;
;   }
;   let volume = 0;
;   const alternations = mask.alternations;
;   for (var y in alternations) {
;     for (var i = 0; i < alternations[y].length; i += 2) {
;       volume += alternations[y][i+1] - alternations[y][i];
;     }
;   }
;   return volume
; }

