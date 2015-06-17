(ns glens.core)

(def conjs (fnil conj #{}))

(defprotocol Attached
  (-graph [this])
  ;XXX -detatch
  )

(defprotocol HasEdges
  (-edges [this]))

(defprotocol HasNodes
  (-nodes [this]))

(defn graph [x] (-graph x))
(defn nodes [x] (-nodes x))
(defn edges [x] (-edges x))

(defn edge-id? [x]
  (and (vector? x) (= (count x) 2)))

(deftype Graph [nodes adjacent]

  Attached
  (-graph [this]
    this)

  HasEdges
  (-edges [this]
    (GraphEdges. this))

  HasNodes
  (-nodes [this]
    (GraphNodes. this))

  )

(deftype Node [^Graph g id]

  Attached
  (-graph [this] g)

  ;XXX HasEdges

  )

(deftype Edge [^Graph g id]

  Attached
  (-graph [this] g)

  ;XXX HasNodes

  )

(deftype GraphNodes [^Graph g]

  Attached
  (-graph [this] g)

  clojure.lang.Counted

  ;XXX clojure.lang.IEditableCollection
  ;XXX asTransient

  clojure.lang.IFn
  (invoke [this node]
    )
  (invoke [this node not-found]
    )

  ;XXX clojure.lang.IHashEq
  ;XXX (hasheq [this])

  clojure.lang.IPersistentCollection
  (cons [this obj]
    (GraphNodes. (Graph. (conj (.nodes g) obj) (.adjacent g))))
  (count [this]
    (count (.nodes g)))
  ;XXX (empty [this]
  ;XXX   )
  ;XXX (equiv [this x]
  ;XXX   )

  clojure.lang.IPersistentSet
  ;XXX (contains [this x]
  ;XXX   )
  ;XXX (disjoin [this x]
  ;XXX   )
  ;XXX (get [this x]
  ;XXX   )

  ;XXX clojure.lang.Seqable
  ;XXX (seq [this]
  ;XXX   )

  java.io.Serializable

  ;XXX java.lang.Iterable
  ;XXX (iterator [this]
  ;XXX   )

  ;XXX java.lang.Runnable
  ;XXX (run [this]
  ;XXX   )

  ;XXX java.util.Collection
  ;XXX (clojure.reflect/type-reflect java.util.Collection)

  ;XXX java.util.Set
  ;XXX (clojure.reflect/type-reflect java.util.Set)

  ;XXX java.util.concurrent.Callable
  ;XXX (call [this]
  ;XXX   )

  )

(deftype GraphEdges [g]

  Attached
  (-graph [this] g)

  clojure.lang.Counted

  ;XXX clojure.lang.IEditableCollection
  ;XXX asTransient

  clojure.lang.IFn
  (invoke [this node]
    )
  (invoke [this node not-found]
    )

  ;XXX clojure.lang.IHashEq
  ;XXX (hasheq [this])

  clojure.lang.IPersistentCollection
  (cons [this obj]
    (assert (edge-id? obj))
    (let [[x y] obj
          nodes (conj (.nodes g) x y)
          adj (-> (.adjacent g)
                  (update x conjs y)
                  (update y conjs x))]
      (GraphEdges. (Graph. nodes adj))))
  ;XXX (count [this]
  ;XXX   )
  ;XXX (empty [this]
  ;XXX   )
  ;XXX (equiv [this x]
  ;XXX   )

  clojure.lang.IPersistentSet
  ;XXX (contains [this x]
  ;XXX   )
  (disjoin [this obj]
    (assert (edge-id? obj))
    (let [[x y] obj
          adj (-> (.adjacent g)
                  (update x disj y)
                  (update y disj x))]
      (GraphEdges. (Graph. (.nodes g) adj))))
  ;XXX (get [this x]
  ;XXX   )

  ;XXX clojure.lang.Seqable
  ;XXX (seq [this]
  ;XXX   )

  java.io.Serializable

  ;XXX java.lang.Iterable
  ;XXX (iterator [this]
  ;XXX   )

  ;XXX java.lang.Runnable
  ;XXX (run [this]
  ;XXX   )

  ;XXX java.util.Collection
  ;XXX (clojure.reflect/type-reflect java.util.Collection)

  ;XXX java.util.Set
  ;XXX (clojure.reflect/type-reflect java.util.Set)

  ;XXX java.util.concurrent.Callable
  ;XXX (call [this]
  ;XXX   )

  )

;XXX (deftype NodeEdges )
;XXX (deftype EdgeNodes )

(defn- graph-rep [^Graph g]
  [(.nodes g) (.adjacent g)]) ;TODO edges instead of adjacent?

(defmethod print-method Graph [^Graph x writer]
  (print-method (tagged-literal 'graph (graph-rep x)) writer))

(defmethod print-method GraphNodes [^GraphNodes x writer]
  (print-method (tagged-literal 'nodes (graph-rep (.g x))) writer))

(defmethod print-method GraphEdges [^GraphEdges x writer]
  (print-method (tagged-literal 'edges (graph-rep (.g x))) writer))

(def null (Graph. #{} {}))

(comment

  (-> null edges (conj [:x :y] [:y :z]) graph)

  (-> null
      nodes (conj :foo :bar)
      graph edges (conj [:a :b] [:b :c] [:bar :baz])
      ;(disj [:b :c])
      )

)
