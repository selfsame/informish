(ns informish.core
  (:refer-clojure :exclude [==])

  (:use [clojure.core.match :only [match]]
        [clojure.string :as string :exclude [replace reverse]]
        [clojure.core.logic :exclude [get-dom]]
        [clojure.core.logic.dcg]
        [clojure.core.logic.pldb]))

(def META-TABLE
  (atom {}))

(def PLURALS
  {:is "\\bare\\b"
   :has "\\b(have)\\b|'s\\b"
   :the "\\b(those|these)\\b"
   :a "\\b(some|any|all)\\b"})

(def TYPES (atom
 {:list #(identity (list))
  :hash-map #(hash-map)
  :set #(set [])}))

(def PATTERNS (atom {}))

(def RELATIONS
  (atom {"\\b(contains|holds)\\b" :contains
         "\\bfirst (of|in)\\b" :first-of
         "\\brest (of|in)\\b" :rest-of
         "\\b(kind|type|class) of\\b" :kind-of
                      }))

(def ADJECTIVES
  (atom {"\\b(mutable|atomic|variable)\\b" :atomic?
         "\\bempty\\b" :empty?
         "\\bunique\\b" :unique?
         "\\bnumerical|numeric\\b" :number?
         "\\associative\\b" :map?
         "\\b(sequential|ordered)\\b" :sequential?
         "\\btrue\\b" :true?
         "\\bfalse\\b" :false?
         "\\bnil|undefined\\b" :nil?
                      }))

(def articles
  {"\\b(the|this|that|those|these)\\b" :the
   "\\b(a|some|any|all)\\b" :a
   })

(def VERB-FUNC
  {:cons cons
   :conj conj
   :set set
   :keys keys
   :vals vals
   })

(def verbs
  {"\\b(is|are)\\b" :is
   "\\b(has|have)\\b|'s\\b" :has
   "\\b(cons|push)(| to)\\b|'s\\b" :cons
   "\\b(conj|append)(| to)\\b|'s\\b" :conj
   })

(def primitives
  {"\\bfunction(|s)\\b" :function
   "\\b(list|collection|vector)(|s)\\b" :list
   "\\b(keyword|key)(|s)\\b" :keyword
   "\\bhash(?:-| |)map(|s)\\b" :hash-map
   "\\b(set)(|s)\\b" :set
   })



(defn make-noun [st]
  (let [t (string/trim st)
        c (keyword (gensym (string/replace t " " "-")))]
  {(str "\\b(" t ")\\b") c}))

(defn singular? [st variant-k]
  (let [pattern (if (variant-k PLURALS) (re-pattern (variant-k PLURALS)) (re-pattern "[s]\\b"))]
    (if (re-find pattern st) true false )))




(defn remove-ws [col]
  (filter #(not= (string/trim (str %)) "") col))

(defn insert-token [st [pattern sub]]
  (let [found (re-find (re-pattern pattern) (str " " (string/lower-case st) " "))
        splt  (split (str " " (string/trim (string/lower-case st) ) " ") (re-pattern pattern))
        replacers (mapv (fn [v] (if
                                  (singular? v sub)
                                    [(identity sub) (keyword "*")] (identity sub))) found)]

    (if found (remove-ws (flatten (pop (vec (interleave splt replacers))))) st)))



(defn tokenize [col scope]
  (reduce (fn [res term]
       (flatten (mapv
        (fn [part]
          (if (string? part)
                (insert-token part term)
                part)) res))
        ) col scope))





(defmacro inform [& body]
  (let [keystr [(apply str (interpose " " (mapv (fn [v] (str v)) body)))]
        scope [primitives verbs @RELATIONS articles @ADJECTIVES @PATTERNS ]]
    ;`(tokenize (tokenize (tokenize ~keystr primitives) verbs) articles)
    `(reduce #(tokenize %1 %2) ~keystr ~scope)
    ))



;=====================
(db-rel -type a)
(db-rel -kind a b)
(db-rel -instance a b)
(db-rel -arity a b)
(db-rel -meta-tag a b)

(def -LOGIC (atom
              (db )))

(defn precord [rel & args]
  (swap! -LOGIC (fn [a] (apply db-fact @-LOGIC rel args))))




(defn register-pattern [regex]
  (if (string? regex)
    (let [with-uid (make-noun regex)]
      (swap! PATTERNS conj with-uid)
      (first (vals with-uid)))
    (if (keyword? regex) regex)))


(defn clone-type [tk rk]
  (when-let [clone ((tk @TYPES))]
    (do (precord -type tk)
    (precord -instance rk tk)
    ;eventual declarative rule conformity
    (swap! META-TABLE conj {rk clone}))))

(defn break-to-be [tl]
  (partition-by #({:is :are} %) [:b :f :a :k :f]))

(defn route
  ([tokenized]
   (let [a (filter (fn [k] (not (#{:* :a} k))) tokenized)
         broke a];(break-to-be a)]
     (apply route broke)))
  ([a b]
   (match [a b]
          [:hash-map _] a
          [:list _] a
          [:set _] a
          [_ _] (get @META-TABLE a)))
  ([a b c]
   (if (a VERB-FUNC) ((a VERB-FUNC)
                      (get @META-TABLE b)
                      (get @META-TABLE c))
  (match [a b c]
         [_ :is :keyword] (clone-type c a)
         [_ :is :hash-map] (clone-type c a)
         [_ :is :list] (clone-type c a)
         [_ :is :set] (clone-type c a)
         [:conj _ _] (conj b c)
         [_ :has _] #()
         [_ _ _] #()))))

(defn play [sta]
  (let [nk (mapv register-pattern sta)]
    (route nk)))




(play (inform settings are a hash map))
(inform player is a hash map)
(play (inform cheese is a set))

(play (inform cat is a set))
(play (inform ladffgfa is a list))
(play (inform player is a hash map))

(play (inform bird is list))

(play (inform cons bird cheese))

(inform A point is a kind of list with two numbers called x and y)


@META-TABLE





(db-rel -function x)
(db-rel -adjective x)
(db-rel -noun x)
(db-rel -adverb x)
(db-rel -verb x)

;built in
(db-rel -pronoun x)
(db-rel -preposition x)
(db-rel -adposition x)
(db-rel -article x)
  (db-rel -definite x)
  (db-rel -indefinite x)



(def -standard
  (db [-article :a]
      [-noun :dog]
      [-article :the]
      [-noun :thing]
      [-noun :value]
      [-verb :is]
      [-verb :called]))


(defne righto [x y l]
  ([_ _ [x y . ?r]])
  ([_ _ [_ . ?r]] (righto x y ?r)))

(defn nexto [x y l]
  (conde
    ((righto x y l))
    ((righto y x l))))



(defmacro reflect [v]
  `(let [~'m (meta (var ~v))
        ~'arities (->> ~'m :arglists (map count))]
     (mapv #(precord -arity (quote ~v) %) ~'arities)
     (when (:tag ~'m) (precord -meta-tag (quote ~v) (:tag ~'m)))
     (precord -type (quote ~v) (type ~v))
     ~'m
  ))

;(map (fn [v] (reflect v)) [str cons]);(keys (ns-publics 'informish.core)))

(defn reflect [v]
  (let [m (meta  v)
        arities (->> m :arglists (map count))]
     (mapv #(precord -arity (quote v) %) arities)
     (when (:tag m) (precord -meta-tag (quote v) (:tag m)))
     (precord -type (quote v) (type v))
     m
  ))


(identity str)
;(reflect 'str)
;(reflect 'cons)



(meta (var str))




(db-rel -number x)
(db-rel -odd x)
(db-rel -even x)
(db-rel -pos x)
(db-rel -neg x)
(db-rel -mod7 x)
;(map nn (filter odd? (range -1000 1000)))

(defn nn [n]
  (if (odd? n) (precord -odd n)
               (precord -even n))
  (if (pos? n) (precord -pos n)
                 (precord -neg n))
  (when (mod n 7) (precord -mod7))

       )







 (into #{} (with-dbs
    [@-LOGIC]
    (run 30 [q]
         (fresh [k j s h]
                (membero s [[7 8][ 8 [[9 1] 2]] [5] [h]])
                (membero j s)
                (== k {s :b
                       j :b})

                (== q j)
                ))))

(type h)


