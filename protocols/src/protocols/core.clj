(ns protocols.core "Protocols, deftype, defrecord, reify"
    (:require [clojure.java.javadoc :refer [javadoc]]))

(comment
  (javadoc java.io.FileFilter)) ;; FileFilter is a Java interface

;; `reify` creates an anonymous Java instance
(comment
  (reify java.io.FileFilter
    (accept [_this _path]
      (#_(do something here)))))

;; `reify` gives us an implementation of a Java interface inline
(let [hidden? (memfn isHidden)
      visible? (complement hidden?)
      file? (memfn isFile)
      file-filter (reify java.io.FileFilter ;; `reify` creates an anonymous Java instance
                    (accept [_ path]
                      (and (file? path) (visible? path))))]
  (.listFiles (java.io.File. ".") file-filter))
; [#object[java.io.File 0x6439c6fc "./project.clj"],
;  #object[java.io.File 0x64702f9a "./LICENSE"],
;  #object[java.io.File 0x5becf58d "./README.md"]]

;; `reify` can be also be used to create one-off instances of Clojure protocols, and override methods on java.lang.Object.
;; The one restriction to `reify` is that it cannot be used to subclass a Java abstract or concrete class.
;; In order to do that we will need to use `proxy` or `gen-class`.
;; Both `proxy` and `gen-class` allow extending a base class in Java, with the difference being `proxy` creates
;; an anonymous instance (much like `reify`) while `gen-class` creates a named type.

;; `reify` represents pure Clojure semantics, versus `proxy` and `gen-class` expose Java semantics.

;; `defrecord` and `deftype`

;; Records and types are Clojure’s answer to Java types, in that they are Java classes.

(defrecord PersonRecord [name])

(deftype PersonType [name])

;; a record is a Java class
(let [record (PersonRecord. "Rich")]
  (println (str "Using map member syntax:  " (:name record)))
  (println (str "Using Java member syntax: " (.name record))))
; (out) Using map member syntax:  Rich
; (out) Using Java member syntax: Rich

(let [type (PersonType. "Stuart")]
  (println (str "Using map member syntax:       " (:name type)))
  (println (str "Only Java member syntax works: " (.name type))))
; (out) Using map member syntax:
; (out) Only Java member syntax works: Stuart

;; Records automatically implement clojure.lang.IPersistentMap interface, which allows them to act as regular Clojure maps.
;; Basically records are maps.
(let [record             (PersonRecord. "Rich")
      associated-record  (assoc  record :last-name "Hickey")
      dissociated-record (dissoc record :name)]
  (println (str "Last name is " (:last-name associated-record)))
  (println (str "record is:             " record))              ; is a PersonRecord
  (println (str "associated-record is:  " associated-record))   ; still a PersonRecord
  (println (str "dissociated-record is: " dissociated-record))) ; Clojure turns PersonRecord into map because one of it fields has been removed
; (out) Last name is Hickey
; (out) record is:             protocols.core.PersonRecord@496e27b5
; (out) associated-record is:  protocols.core.PersonRecord@e8606646
; (out) dissociated-record is: {}

;; Clojure provides two constructors for a record.
;; `->PersonRecord` and `map->PersonRecord`
(let [using->     (->PersonRecord "Rich")               ; uses postional args
      using-map-> (map->PersonRecord {:name "Stuart"})] ; uses a map for the record fields
  (println (str "using->     " (:name using->)))
  (println (str "using-map-> " (.name using-map->))))
; (out) using->     Rich
; (out) using-map-> Stuart

;; Types are exactly like Java types.
;; We cannot treat them as maps.
;; They allow for mutable fields!

;; Protocols are to Clojure what interfaces are to Java.
;; Protocols are mere contracts and offer no implementation.

;; first parameter of a method signature designates this (much like `reify`)
(defprotocol Show
  (pretty-print [_this]))

(defprotocol Identify
  (id [_this]))

;; Protocol usage
;; `extend-protocol` and `extend-type` macros.
;; Both macros work with types (defined by defrecord or deftype)

(comment
  (javadoc "")
  (.hashCode "Rich")) ; 2546940

#_{:clj-kondo/ignore [:redefined-var]}
(defrecord PersonRecord
           [name])

;; `extend-type` is useful when extending a particular type to multiple protocols
(extend-type PersonRecord
  Show
  (pretty-print [this] (str "My name is " (:name this)))
  Identify
  (id [this] (.hashCode (:name this))))

(let [rich (PersonRecord. "Rich")]
  (println (pretty-print rich))
  (println (id rich)))
; (out) My name is Rich
; (out) 2546940

(map #(.getName %) (.getMethods PersonRecord))
; ("remove"
;  "size"
;  "get"
;  "put"
;  "equals"
;  "values"
;  "hashCode"
;  "clear"
;  "isEmpty"
;  "iterator"
;  "count"
;  "entrySet"
;  "putAll"
;  "empty"
;  "cons"
;  "keySet"
;  "containsValue"
;  "containsKey"
;  "create"
;  "seq"
;  "hasheq"
;  "withMeta"
;  "meta"
;  "assoc"
;  "assoc"
;  "valAt"
;  "valAt"
;  "entryAt"
;  "without"
;  "equiv"
;  "getLookupThunk"
;  "getBasis"
;  "toString"
;  "getClass"
;  "notify"
;  "notifyAll"
;  "wait"
;  "wait"
;  "wait"
;  "assocEx"
;  "spliterator"
;  "forEach"
;  "remove"
;  "replace"
;  "replace"
;  "replaceAll"
;  "merge"
;  "putIfAbsent"
;  "compute"
;  "computeIfAbsent"
;  "forEach"
;  "getOrDefault"
;  "computeIfPresent")
;; functions are not added to PersonRecord

(filter
 #(re-find #"pretty-print|id" %)
 (map #(.getName %) (.getMethods PersonRecord)))
; ()

;; `extend-type`:     extend a type     with several protocols. Name type     then protocols.
;; `extend-protocol`: extend a protocol with several types.     Name protocol then types.

;; If an unimplemented function is invoked then Clojure will throw an exception.
(extend-protocol Show
  PersonRecord
  (pretty-print [this] (str "I am a record: " (:name this)))
  PersonType
  (pretty-print [this] (str "I am a type:   " (.name this))))

(let [rich   (PersonRecord. "Rich")
      stuart (PersonType.   "Stuart")]
  (println (pretty-print rich))
  (println (pretty-print stuart)))
; (out) I am a record: Rich
; (out) I am a type:   Stuart

;; extend an existing type ("expression problem" solved ;-)
(extend-type String
  Identify
  (id [this] (.hashCode this)))

(id "Rich") ; 2546940

;; extending a protocol using `reify`
(let [reified-show (reify Show
                     (pretty-print [_] "I am anonymous"))]
  (pretty-print reified-show))
; "I am anonymous"

;; `extend-protocol` and `extend-type` rely on a function named `extend`, which can be use directly.

(extend PersonRecord
  Show
  {:pretty-print #(str "My name is " (:name %))}
  Identify
  {:id #(.hashCode (:name %))})

(let [rich (PersonRecord. "Rich")]
  (println (pretty-print rich))
  (println (id rich)))
; (out) My name is Rich
; (out) 2546940

;; Protocol implementations are mere maps!
;; This implies that we can construct these maps using the powerful associative API available to us!

(def mixin
  {:pretty-print #(str "I am mixed in. Name is: " (:name %))})

;; We can change how a protocol gets extended for a type dynamically!
(extend PersonRecord
  Show
  (merge {:pretty-print #(str "My name is " (:name %))} mixin)
  Identify
  {:id #(.hashCode (:name %))})

(let [rich (PersonRecord. "Rich")]
  (println (pretty-print rich))
  (println (id rich)))
; (out) I am mixed in. Name is: Rich
; (out) 2546940

;; implementing a protocol by inlining it
#_{:clj-kondo/ignore [:redefined-var]}
(defrecord PersonRecord
           [name]
  Show
  (pretty-print [this] (str "My name is " (:name this)))
  Identify
  (id [this] (.hashCode (:name this))))

(let [rich (PersonRecord. "Rich")]
  (println (pretty-print rich))
  (println (id rich)))
; (out) My name is Rich
; (out) 2546940
