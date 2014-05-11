(ns informish.experiments
  (:refer-clojure :exclude [==])
  (:use [clojure.algo.monads :as m])
  (:use [informish.cvar :only [cvar solve fn-name]]

        [clojure.core.logic :exclude [get-dom]]

        [clojure.core.logic.pldb]))



(def -LOGIC (atom
              (db )))

(defn precord [rel & args]
  (swap! -LOGIC (fn [a] (apply db-fact @-LOGIC rel args))))

(def men (take 20 (repeatedly #(cvar))))
(db-rel -man a)
(db-rel -friend a b)
(map #(precord -man %) men)
(precord -friend (rand-nth men)(rand-nth men))

men




(db-rel -fn-res a b c)
(db-rel -pred a b c)





(for [
x [sequential? number? map? list? vector? set? empty? string?]

z [[] {}  "a" 7 ]]
  (let [res (try (x z)
    (catch Exception e :error))]
   (precord -pred x z res)


  res))


(for [
x [count str first conj cons vec first rest flatten type ]

z [[] {}  "a" 7 ]]
  (let [res (try (x z)
    (catch Exception e :error))]
   (precord -fn-res x z res)


  res))

(def g [])



(set? 7)
 (into #{} (with-dbs
    [@-LOGIC]
    (run 300 [q]
         (fresh [f d r p j b ]

                ;(== p empty?)
                (-fn-res f d r)
                (-pred p d b)
                (!= r :error)
                (== b true)
                (membero p [empty? list?])
                (== q f)
                ))))








 (str pos?)


 (into #{} (with-dbs
    [@-LOGIC]
    (run 300 [q]
         (fresh [f d r p j b ]


                (-fn-res count d r)
                (-pred p d true)
                ;(!= r :error)
                ;(== p map?)
                (== q [d r])
                ))))
