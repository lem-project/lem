(ns sample.core
  "Sample Clojure namespace for testing lem clojure-mode."
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;;; ============================================================
;;; Basic Definitions
;;; ============================================================

(def ^:const PI 3.14159265358979)

(def config
  {:host "localhost"
   :port 8080
   :debug? true})

;;; ============================================================
;;; Functions
;;; ============================================================

(defn greet
  "Greet a person by name."
  [name]
  (str "Hello, " name "!"))

(defn add
  "Add two numbers."
  [a b]
  (+ a b))

(defn factorial
  "Calculate factorial of n."
  [n]
  (if (<= n 1)
    1
    (* n (factorial (dec n)))))

(defn process-data
  "Process a collection of data with various transformations."
  [data]
  (let [filtered (filter even? data)
        doubled (map #(* 2 %) filtered)
        result (reduce + 0 doubled)]
    (when (pos? result)
      (println "Result:" result))
    result))

;;; ============================================================
;;; Control Flow Examples
;;; ============================================================

(defn classify-number
  "Classify a number as positive, negative, or zero."
  [n]
  (cond
    (pos? n) :positive
    (neg? n) :negative
    :else :zero))

(defn safe-divide
  "Safely divide two numbers, returning nil on division by zero."
  [a b]
  (when-not (zero? b)
    (/ a b)))

(defn process-items
  "Process a sequence of items."
  [items]
  (doseq [item items]
    (println "Processing:" item)))

;;; ============================================================
;;; Higher-Order Functions
;;; ============================================================

(defn compose-fns
  "Compose multiple functions."
  [& fns]
  (fn [x]
    (reduce (fn [acc f] (f acc)) x fns)))

(defn memoize-fn
  "Simple memoization wrapper."
  [f]
  (let [cache (atom {})]
    (fn [& args]
      (if-let [cached (get @cache args)]
        cached
        (let [result (apply f args)]
          (swap! cache assoc args result)
          result)))))

;;; ============================================================
;;; Data Structures
;;; ============================================================

(defrecord Person [name age email])

(defprotocol Printable
  (to-string [this] "Convert to string representation"))

(extend-protocol Printable
  Person
  (to-string [p]
    (format "Person{name=%s, age=%d}" (:name p) (:age p)))

  clojure.lang.PersistentVector
  (to-string [v]
    (str "[" (str/join ", " v) "]")))

;;; ============================================================
;;; Multimethods
;;; ============================================================

(defmulti area
  "Calculate area of a shape."
  :type)

(defmethod area :circle
  [{:keys [radius]}]
  (* PI radius radius))

(defmethod area :rectangle
  [{:keys [width height]}]
  (* width height))

(defmethod area :triangle
  [{:keys [base height]}]
  (/ (* base height) 2))

(defmethod area :default
  [_]
  (throw (ex-info "Unknown shape type" {})))

;;; ============================================================
;;; Threading Macros
;;; ============================================================

(defn transform-data
  "Transform data using threading macros."
  [data]
  (-> data
      (assoc :processed true)
      (update :count inc)
      (dissoc :temp)))

(defn process-collection
  "Process a collection using thread-last."
  [coll]
  (->> coll
       (filter some?)
       (map str/upper-case)
       (sort)
       (take 10)))

;;; ============================================================
;;; Error Handling
;;; ============================================================

(defn safe-parse-int
  "Safely parse an integer from a string."
  [s]
  (try
    (Integer/parseInt s)
    (catch NumberFormatException _
      nil)))

(defn with-retry
  "Execute f with retries on failure."
  [n f]
  (loop [attempts n]
    (let [result (try
                   {:success (f)}
                   (catch Exception e
                     {:error e}))]
      (if (:success result)
        (:success result)
        (if (pos? attempts)
          (recur (dec attempts))
          (throw (:error result)))))))

;;; ============================================================
;;; Regex and Strings
;;; ============================================================

(def email-pattern #"^[\w.+-]+@[\w.-]+\.\w{2,}$")

(defn valid-email?
  "Check if string is a valid email."
  [s]
  (boolean (re-matches email-pattern s)))

(defn parse-log-line
  "Parse a log line into components."
  [line]
  (when-let [[_ timestamp level message]
             (re-matches #"(\d{4}-\d{2}-\d{2}) \[(\w+)\] (.+)" line)]
    {:timestamp timestamp
     :level (keyword (str/lower-case level))
     :message message}))

;;; ============================================================
;;; Atoms and State
;;; ============================================================

(def counter (atom 0))

(defn increment-counter!
  "Increment the global counter."
  []
  (swap! counter inc))

(defn reset-counter!
  "Reset the global counter to zero."
  []
  (reset! counter 0))

(defn with-counter
  "Execute f and track how many times it's called."
  [f]
  (fn [& args]
    (increment-counter!)
    (apply f args)))

;;; ============================================================
;;; Main Entry Point
;;; ============================================================

(defn -main
  "Main entry point."
  [& args]
  (println "Sample Clojure Application")
  (println "Arguments:" args)
  (println (greet "World"))
  (println "Factorial of 5:" (factorial 5))
  (println "Process data:" (process-data (range 1 11)))
  (println "Circle area:" (area {:type :circle :radius 5}))
  (println "Rectangle area:" (area {:type :rectangle :width 4 :height 6})))

;; Rich comment block for REPL-driven development
(comment
  (-main)
  (greet "Lem")
  (factorial 10)
  (process-data [1 2 3 4 5 6 7 8 9 10])
  (area {:type :circle :radius 10})
  (valid-email? "test@example.com")
  (parse-log-line "2024-01-15 [INFO] Application started")

  ;; Threading examples
  (-> {:count 0 :temp "x"}
      (transform-data))

  (->> ["hello" nil "world" "foo"]
       (process-collection))

  ;; Atoms
  (reset-counter!)
  (increment-counter!)
  @counter

  :end)
