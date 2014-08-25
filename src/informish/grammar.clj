(ns informish.grammar
  (:refer-clojure :exclude [==])

  (:use
   [informish.cvar :refer [cvar solve]]
   [informish.data :refer [DATA uid? uid-> KINDS kind? kind-> kinds-in]]
        [clojure.core.logic.dcg]
        [clojure.core.logic]))


(declare adj-noun2 adj-noun3 group1)
(def D (atom {}))


(defn bind!
  ([s] s)
  ([s v]
  (swap! D #(conj % {s v}))
  (intern *ns* s v)))


(def -types #{'list 'set 'map 'number 'string})

(def -abstract-singular #{'thing 'item 'member})

(def -abstract-plural #{'things 'items 'members 'keys 'values})

(def -verbs #{})



(def-->e typo [x]
    ([_] [x]
       (!dcg
        (project [x]
          (== (contains? -types x) true)))))

(def-->e kindo [x]
    ([_] [x]
       (!dcg
        (project [x]
          (== (kind? x) true)))))

(def-->e abstracto [x]
    ([_] [x]
       (!dcg
        (project [x]
          (== (contains? -abstract-singular x) true)))))

(def-->e existo [x]
    ([_] [x]
       (!dcg
        (project [x]
          (== true (contains? (set (keys (ns-publics *ns*))) x))))))

(def-->e keyo [x]
    ([_] [x]
       (!dcg
        (project [x]
          (== (keyword? x) true)))))

(def-->e uido [x]
    ([_] [x]
       (!dcg
        (project [x]
          (== (uid? x) true)))))

(def-->e verbo [x]
    ([_] [x]
       (!dcg
        (project [x]
          (== (contains? -verbs x) true)))))

(def-->e symbolo [x]
    ([_] [x]
       (!dcg
        (project [x]
          (== (string? x) true)))))

(def-->e det [x]
    ([:coll] ['all])
    ([:type] ['a])
    ([:ref] ['the])
    ([:prop] ['his]))

(def-->e adj [x]
    ([first] [first])
    ([rest] [rest])
    ([count] [count])
    ([keys] [keys]))




(def-->e possesive [x]
    (['solve] ['of]))

(def-->e grouping [x]
    (['kinds-in] ['all])
    (['kinds-in] ['every]))

(def-->e instance [x]
    (['kind->] ['a])
    (['kind->] ['an]))

(def-->e define [x]
    (['bind!] ['is]))

(def-->e initial-noun [x]
  ;noun is a defined symbol
  ([?d] (existo ?d))
  ;([[?a ?d]] (det ?a)(existo ?d))
  ;uid keywords
  ([['uid-> ?d]] (uido ?d))
  ;keywords
  ([?d] (keyo ?d))
  ;noun is a class of things
  ([[?d]] (typo ?d))
  ;noun is abstract
  ([['cvar ?d]] (abstracto ?d) ))

(def-->e noun1 [x]
  ;noun is a defined symbol
  ([[?p ?d ?n]] (initial-noun ?d) (possesive ?p) (initial-noun ?n))
  ;([[?p ?n]] (grouping ?p) (kindo ?n))
  ;([[?p ?n ?n2 ?h]] (grouping ?p) (kindo ?n) (possesive ?h) (initial-noun ?n2))
  ([?d] (initial-noun ?d)))

(def-->e adj-noun1 [x]
  ;noun is a defined symbol
  ([[?p ?a ?d]] (adj ?a)(possesive ?p)(noun1 ?d))
  ([['solve ?a ?d]] (adj ?a)(noun1 ?d))
  ([?d] (noun1 ?d)))

(def-->e group1 [x]
  ([[?g ?k]] (instance ?g) (kindo ?k))
  ([[?g ?k]] (grouping ?g) (kindo ?k))
  ([[?g ?k ?n ?h]] (grouping ?g) (kindo ?k) (possesive ?h) (adj-noun1 ?n))
)

(def-->e noun2 [x]
  ;noun is a defined symbol
  ([[?p ?d ?n]] (adj-noun1 ?d) (possesive ?p) (adj-noun1 ?n))
  ([?d] (adj-noun1 ?d))
  ([?g] (group1 ?g)))

(def-->e adj-noun2 [x]
  ;noun is a defined symbol
  ([[?p ?a ?d]] (adj ?a)(possesive ?p)(noun1 ?d))
  ([['solve ?a ?d]] (adj ?a)(noun2 ?d))
  ([?d] (noun2 ?d))
  ([?g] (group1 ?g)))

(def-->e noun3 [x]
  ;noun is a defined symbol
  ([[?p ?d ?n]] (adj-noun2 ?d) (possesive ?p) (adj-noun2 ?n))
  ([?d] (adj-noun2 ?d))
  ([?g] (group1 ?g)))

(def-->e adj-noun3 [x]
  ;noun is a defined symbol
  ([[?p ?a ?d]] (adj ?a)(possesive ?p)(noun3 ?d))
  ([['solve ?a ?d]] (adj ?a)(noun3 ?d))
  ([?d] (noun3 ?d))
  ([?g] (group1 ?g)))

(def-->e subject [x]
  ;noun is a defined symbol
  ([?n] (adj-noun3 ?n))
  ([[?i ['symbol ?p] ?n]] (symbolo ?p) (define ?i) (adj-noun3 ?n)))

(def-->e verb [x]
    ([[?d]] (verbo ?d)))




;(run 10 [q]
;    (declaration q '[the key of map is a number] []))

;(run 1 [q]
;    (adj-noun2 q '[:name of :siblings of first player] []))
;(run 10 [q]
;    (adj-noun2 q '[:name of :name of player] []))
;; (run 1 [q]
;;     (adj-noun2 q '[:name of thing] []))

;; (run 10 [q]
;;     (adj-noun2 q '[:name of first :siblings of player] []))






(defn v->l [part]
  (let [p ;part]
        (if (sequential? part)
            (do ;(when (= 2 (count part))
                 ; (cons 'solve part))
            (cond (= 'cvar (first part))
                  '(cvar)

              :else (map v->l part)) )
            part)]
    (if (vector? p) (reverse (into '() p)) p)))


(defn ok [phrase]
  (let [DCG (run 10 [q]
    (subject q phrase []))

        code (first (v->l DCG))]
        {code (eval code)}
        ))




(ok '[:d first :contents of :1])


;(def dv [grinding tire])


(ok '["grinding" is a :wheel])

(ok '["tire" is a :wheel])

(ok '[first thing])

D

