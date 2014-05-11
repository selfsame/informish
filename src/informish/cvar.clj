(ns informish.cvar)

(defn fn-name [f]
  (if (keyword f) f
   (str
     (first (rest
       (re-find #".*\$([^_.*]+)(_QMARK_)*@" (str f))))
    (when (re-find #"_QMARK_" (str f)) "?")
    )) )




(defprotocol Solve
  (s- [this f]))
(defprotocol Constrain
  (c- [this f]))

(def ^:private -uid (atom 0))
(defn ^:private get-uid [] (swap! -uid inc))
(def ^:private CVARS (atom {}))

(defn  ^:private -type [cv]
  (let [fs (set (:c meta))]

  ))

(deftype CVar [data meta]
  clojure.lang.IObj
  (meta [this] meta)
  (withMeta [this new-meta] (CVar. data new-meta))
  Object
  (toString [this]
    (let [dep (:dep data)
          fs (set (:c meta))
          sug (str
                   "#"(get-uid)
                   (apply str (map fn-name fs))
                   (when dep (str "(" (apply str (interpose " " dep)) ")"))
                   (when (:s data) (str "=" (:s data) ")"))
               ""
               )] sug))
  clojure.lang.Counted
  (count [this]  0)
  Solve
  (s-
   [this f]
   (let [fs (set (:c meta))
         deps (:deps data)
         answer (if deps
                  (s-  (last deps) (first deps))
                  this)
         solution (if (:s data)(:s data) nil)]
     (cond
       solution (f solution)
       (keyword f) (CVar. {:dep [(fn-name f) answer]} {})
       (= count f) (cond (fs empty?) (CVar. {:s 0} {})
                         (fs number?) (count 6)
                         :else (CVar. {:dep [(fn-name f) answer]} {:c #{number?}}))
       (= first f) (cond (fs empty?) (CVar. {:s nil} {})
                         (fs number?) (first 6)
                         :else (CVar. {:dep ['first this]} {}))
       (= type f) "?type"
       :else (CVar. {:dep [(fn-name f) answer]}
                            {}))))

  Constrain
  (c-
   [this f]
   (with-meta
     this
     (merge-with
      concat
      (meta this)
      {:c  (set (if (sequential? f) f [f]))}
      ))))

(defn solve [f v]
  (if (= (type v) informish.cvar.CVar)
    (s- v f)
    (f v)))

(defn cvar
  ([] (CVar. {} {}))
  ([v] (cvar)))





(def v (c- (cvar ) empty?))
(solve identity (cvar))

(def cn (c- v [vector?]))
cn

(solve first (solve reverse cn))
(cvar)

(c- (cvar) empty?)
(solve count (cvar))
(solve str (solve count (c- (cvar) empty?)))

(solve count (c- (cvar) empty?))

(solve :meta (cvar) )

;this should solve as {}
(c- (cvar ) [empty? map?])

