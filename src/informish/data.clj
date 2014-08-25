(ns informish.data
  (:use
        [clojure.core.logic.dcg]
        [clojure.core.logic]))

(def DATA
  {:1 {:type :room :n "Bathroom" :d "A small bathroom with black and white tiles on the wall." :prep "The"
       :contents [:2 :5 :4 :5] :color "yellow"}
   :2 {:type :container :n "cabinet" :d "It has two mirrored doors."
       :contents [:3 :4 :5 :3]}
   :3 {:type :thing :n "toothbrush" :d "A plain toothbrush."}
   :4 {:type :thing :n "soap" :d "Irish Spring soap." :prep "" :colord "green"}
   :5 {:type :thing :n "apple" :test :4 :d "Just a normal piece of fruit." :prep "an " :colord "red"}})


(def KINDS
  {:thing {:n "thing" :d ""}
   :container {:derive :thing :contents []}
   :wheel {:derive :thing :pressure 2}
   :vehicle {:derive :room :wheels []}
   :room {:derive :container}})

(defn uid? [d]
  (if
    (and (keyword? d) (get DATA d))
    true false))


(defn uid-> [k] (or (get DATA k) k))

(defn kind? [d]
  (if
    (and (keyword? d) (get KINDS d))
    true false))

(defn derived [k]
  (let [ik (get KINDS k)]
    (loop [m ik
           res #{k}]
      (let [der (get m :derive)
            nres (set (concat res (if der [der] #{})))]
      (if (or (not der) (= nres res) (res der))
        res
        (recur (get KINDS der) nres))))))


(defn kind-> [k]
  (let [dv (vec (derived k))
        vv (vals (select-keys KINDS dv))
        mv (conj (apply merge vv) {:type k})]
          (dissoc mv :derive)))

(defn kinds-in
  ([k] (kinds-in k (vals DATA)))
  ([k col] (filter #(= k (:type (or (uid-> %) %))) col))
  ([k col & more] (kinds-in k col)))



(def td (str
"A thing is a kind of map. A container is a kind of thing. A container has a vector called contents."
"A ship is a kind of container. A ship is usually opaque."
"A computer is a kind of thing."
"A computer has a thing called a target. "
"A vector is a kind of value. The form [100,100,100] specifies a vector with parts x, y (without leading zeros), z (without leading zeros)."
"Repeat for S running  through the shiplist:"
"Every ship contains a computer (called its computer);"
"A ship has a vector called position. The position of a ship is usually <0,0,0>;"
"A ship has a vector called velocity.  The description of a ship is usually 'a normal ship'."
"After printing the name of a ship (called V):"
"	say '[position of V]';"
" let B be a random position in the list of all positions;
    Repeat (for S) running through the shiplist:
         say '[name of S]'."
"A sun is a kind of light."
))



td

(defn split-statements [s] (map first (re-seq #"([\w\s0-9'\",\<\>=()\]\[]+)([.:;]*)" s)))

(defn parse-words [ss]
  (if-not (string? ss)
    (list ss)
  (let [s (.toLowerCase ss)
        punct (last s)
        wrapper (cond (= punct \.) vec
                      (= punct \:) vec
                      :else identity)
        f (comp #(with-meta % {:form punct}) wrapper )]

  (f (map first (re-seq #"([\w.:;]+)" s))))))

(defn parse-closures [s]
  (re-seq #"(?>\()[^\(\)]+(?>\))" s))





(defn jlist [& more]
  (let [m (filter #(if (string? %)
                     (if (re-seq #"([\w.:;]+)" %) true false) true) more)]
    (apply concat (map parse-words m))))

(defn jvector [& more]
  (let [m (filter #(if (string? %)
                     (if (re-seq #"([\w.:;]+)" %) true false) true) more)]
    (vec (apply concat (mapv parse-words m)))))

(defn parse-enclosures [s]
  (load-string (apply str
        (concat ["(jlist (str " ] (map-indexed (fn [i c] (case c
                         \(  ")(jlist (str "
                         \)  "))(str "
                         \[  ")(jvector (str "
                         \]  "))(str "
                         (str "\\" c))) s)
                    [") )"]))))



(defn pre-DCG [col]
  (let [mcol (map #(if (#{"is" "are"} %) :is %) col)
        pcol (partition-by #(#{:is} %) mcol)
        head (first pcol)
        tail (last pcol)]

    (cond
      (= 3 (count pcol)) ['def head tail]
      (= \: (last (last (flatten col)))) ['block mcol]
      :else mcol)))


(defn mega-parse [st]
  (let [res (mapv pre-DCG
         (mapv parse-enclosures (split-statements st)))
        ]
    (reduce (fn [col v]
              (let [terminator (last (last (flatten (flatten v))))
                    control (first (or (last col) []))
                    prev-term (last (last (flatten (last col))))]
                ;(print "{"terminator"|"prev-term"|"control"}")
                (cond (and
                       (contains? #{\: \;} prev-term)
                       (= control 'block)) (conj (pop col) (conj (peek col) v))

                :else (conj col v)))) [] res)))



(mapv #(print % "\n\n")
 (mega-parse td))


(mega-parse td)



(def-->e verbo [x]
    ([:is] ["is"])
    ([:has] ["has"])
    ([:contain] ["contains"])
    ([:contain] ["holds"]))


(def-->e article [x]
    ([:indef] ["a"])
    ([:indef] ["an"])
    ([:def] ["the"])
    ([:pos] ["his"])
    ([:pos] ["its"])
    ([:pos] ["her"]))

(def-->e stringo [x]
    ([_] [x]
       (!dcg
        (project [x]
          (== (string? x) true)))))


(def-->e noun [x]
  ([[{:noun ?d}]] (stringo ?d))
  ([[{:noun [?d ?d2]}]] (stringo ?d)(stringo ?d2))
  ([[{:noun [?d ?d2 ?d3]}]] (stringo ?d)(stringo ?d2)(stringo ?d3))
  ([[{:noun [?d ?d2 ?d3 ?d4]}]] (stringo ?d)(stringo ?d2)(stringo ?d3)(stringo ?d4)))

(def-->e art-noun [x]
  ([[?a ?d]] (article ?a)(noun ?d))
  ([[:def ?d]] (noun ?d))
  )


(def-->e subject-verb [x]
  ([[?a {:verb ?v}]] (art-noun ?a)(verbo ?v)))


(defn do-dcg [phrase]
  (let [DCG (run 10 [q]
    (subject-verb q phrase []))]
    DCG))





(do-dcg '["the" "boat" "is"])
