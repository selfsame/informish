(ns informish.basic
  (:refer-clojure :exclude [==])
  (:use
   [informish.cvar :refer [cvar solve]]
   [informish.data :refer [DATA uid? uid->]]
   [informish.grammar :refer [ok]]
   ))

(defn prep [v]
  (or (:prep v) "a "))

(defn list-interpose [sep tailsep col]
  (let [c (count col)]
    (if (<= c 2) (interpose tailsep col)
        (vec (concat (interpose sep (pop col)) [tailsep (last col)])))))

(declare do!)



(def ACTIONS
  {:print
   {:fn #(str %)
    :args
    {:description
     {:pre #(str "\n\n\n"(:n %) "\n --------\n")
      :fn #(str ""(:d %))}
     :name
     {:pre (fn [v] "")
      :fn #(str (prep %) (:n %))
      }}}
   :list
     {:fn (fn [col]
            (apply str (list-interpose
                        ", " " and "
                        (mapv #(do! :print :name %) col) )))}})

(def RULES
  {[:print :description]
   {#(= :room (:type %))
    {:pre #(str "\n\n\n" (do! :print :name %) "\n ~~~~~~~~~~~~\n")
     :fn #(str ">"(:d %) "\n\nYou see:")
     :post (fn [v] (do! :list :name (vals (select-keys DATA (:contents v)))))}}
   [:print :name]
   {#(:color %)
    {:pre #(str "<span class='" (:color %) "'>")
     :post (fn [v] "</span>")}

    #(and (:contents %) (not (:color %)))
    {:post (fn [v]
             (str "("
             (do! :list :name (vals (select-keys DATA (:contents v))))") ")
             )}}})



(defn do! [kf arg v]
  (let [action (kf ACTIONS)
        argmod (get (:args action) arg)
        f (or (:fn argmod) (:fn action) (fn [c] ""))
        pre (or (:pre argmod) (:pre action) (fn [c] ""))
        post (or (:post argmod) (:post action) (fn [c] ""))
        value v
        ka [kf arg]
        rules (get RULES ka)
        valid (filter (fn [[kk vv]] (kk v)) rules)
        stack (cons {:pre pre :post post :fn f} (vals valid))
        merged (apply merge stack)
        pre (:pre merged)
        f (:fn merged)
        post (:post merged)]

     (str (pre v)(f v)(post v))))

(defn --> [kf arg v]
  (print (do! kf arg v)))

(def bathroom (get DATA :1))

(--> :print :description (get DATA :1))

(ok '[:n of :1])
(ok '[first :contents of bathroom])
