(ns informish.core
  (:refer-clojure :exclude [==])

  (:use
   [informish.cvar :refer [cvar solve]]
        [clojure.core.logic.dcg]
        [clojure.core.logic]
        ;[clojure.core.logic.pldb]
   ))



(def -types #{'list 'set 'map 'number 'string})

(def -abstract-singular #{'thing 'item 'member})

(def -abstract-plural #{'things 'items 'members 'keys 'values})

(def -verbs #{'is 'has})


(def-->e typo [x]
    ([_] [x]
       (!dcg
        (project [x]
          (== (contains? -types x) true)))))

(def-->e abstracto [x]
    ([_] [x]
       (!dcg
        (project [x]
          (== (contains? -abstract-singular x) true)))))

(def-->e existo [x]
    ([_] [x]
       (!dcg
        (project [x]
          (== (contains? (set (keys (ns-publics *ns*))) x) true)))))

(def-->e keyo [x]
    ([_] [x]
       (!dcg
        (project [x]
          (== (keyword? x) true)))))

(def-->e verbo [x]
    ([_] [x]
       (!dcg
        (project [x]
          (== (contains? -verbs x) true)))))

(def-->e det [x]
    ([:coll] ['all])
    ([:type] ['a])
    ([:ref] ['the])
    ([:prop] ['his]))

(def-->e adj [x]
    ([first] [first])
     ([count] [count])
     ([keys] [keys]))

(def-->e possesive [x]
    (['solve] ['of]))


(def-->e initial-noun [x]
  ;noun is a defined symbol
  ([?d] (existo ?d))
  ;([[?a ?d]] (det ?a)(existo ?d))
  ;keywords
  ([?d] (keyo ?d))
  ;noun is a class of things
  ([[?d]] (typo ?d))
  ;noun is abstract
  ([['cvar ?d]] (abstracto ?d) ))

(def-->e noun1 [x]
  ;noun is a defined symbol
  ([[?p ?d ?n]] (initial-noun ?d) (possesive ?p) (initial-noun ?n))
  ([?d] (initial-noun ?d)))

(def-->e adj-noun1 [x]
  ;noun is a defined symbol
  ([['solve ?a ?d]] (adj ?a)(noun1 ?d))
  ([?d] (noun1 ?d)))


(def-->e noun2 [x]
  ;noun is a defined symbol
  ([[?p ?d ?n]] (adj-noun1 ?d) (possesive ?p) (adj-noun1 ?n))
  ([?d] (adj-noun1 ?d)))

(def-->e adj-noun2 [x]
  ;noun is a defined symbol
  ([['solve ?a ?d]] (adj ?a)(noun2 ?d))
  ([?d] (noun2 ?d)))



(def-->e verb [x]
    ([[?d]] (verbo ?d)))




;(run 10 [q]
;    (declaration q '[the key of map is a number] []))

(run 1 [q]
    (adj-noun2 q '[:name of :siblings of first player] []))
(run 10 [q]
    (adj-noun2 q '[:name of :name of player] []))
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
    (adj-noun2 q phrase []))

        code (first (v->l DCG))]
        {code (eval code)}
        ))

(def player {:name "john"
                      :age 29
                      :siblings [{:name "sue"}
                                 {:name "steve"}]})


(ok '[:name of player])

(ok '[player])

(ok '[:name of first :siblings of thing])

; the player (player)
; the thing (cvar #{})
; a list (cvar #{(fn [v] (list? v))})
; the :siblings of the player (:siblings player)
; the first item of the :siblings of the player (first (:siblings player))


;; (def-->e identifier [x]
;;    ([[?p ?n [?p2 ?d ?g]]] (noun ?g) (possesive ?p2) (noun ?d) (possesive ?p) (noun ?n))
;;    ([[?n ?p ?d]] (noun ?d) (possesive ?p) (noun ?n)))














