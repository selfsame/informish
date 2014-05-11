(ns informish.predicates
  (:refer-clojure :exclude [==])

  (:use [clojure.core.match :only [match]]
        [clojure.string :as string :exclude [replace reverse]]
))

" cvars track the constraints
  a symbol is used for primitives"


(defn cvar? [v] (if (:? (meta (cvar))) true false))

(defn cvar
  ([] (cvar '?))
  ([v]
  (let [mta (merge-with conj
               (meta v)
                {:? #{}})]
    (with-meta v (merge-with conj mta)))))

(defn bind [v c]
  )

(defmacro kind [sym pred f & body]
 `(let []
 (defn ~pred ~@f)
 (defn ~sym ~@body)))

(kind pair pair?
 ([p] (and (first p) (= 2 (count p))))
 ([] (list (cvar)(cvar)))
 ([p] (cond (number? p) (pair)
            (pair? p) p
            :else (pair)))
 ([x y] (list x y))
 ([x y & more] (list x y)))

(kind point point?
      ([[x y :as p]]
       (and
        (pair? p)
        (number? x)
        (number? y)))
      ([] [0 0])
      ([p] (pair p))
      ([x y] (pair [x y])))

(kind rect rect?
  ([[xy wh :as r]]
    (every?
      (pair? r)
      (point? xy)
      (point? wh)))
  ([] [(point) (point)])
  ([[x y :as r]] (pair (point x) (point y)))
  ([x y] [(point x) (point y)]))

(pair (pair) (pair))

(pair (point 8 8) (pair (rect) (point -1 -1)))
(point? (point 6 9))
(point? [0 0])
(rect [3 4] [5 6])

"

A pair is sequential and has two members.

A point is a pair where each member is numerical.

A rect is a pair of points.


"



