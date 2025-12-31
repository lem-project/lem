(ns sample.core-test
  "Tests for sample.core namespace."
  (:require [clojure.test :refer [deftest testing is are]]
            [sample.core :as core]))

;;; ============================================================
;;; Basic Function Tests
;;; ============================================================

(deftest greet-test
  (testing "greet returns proper greeting"
    (is (= "Hello, World!" (core/greet "World")))
    (is (= "Hello, Lem!" (core/greet "Lem")))
    (is (= "Hello, !" (core/greet "")))))

(deftest add-test
  (testing "add with positive numbers"
    (is (= 5 (core/add 2 3)))
    (is (= 0 (core/add 0 0))))
  (testing "add with negative numbers"
    (is (= -1 (core/add 2 -3)))
    (is (= -5 (core/add -2 -3)))))

(deftest factorial-test
  (testing "factorial of small numbers"
    (is (= 1 (core/factorial 0)))
    (is (= 1 (core/factorial 1)))
    (is (= 2 (core/factorial 2)))
    (is (= 6 (core/factorial 3)))
    (is (= 24 (core/factorial 4)))
    (is (= 120 (core/factorial 5)))))

;;; ============================================================
;;; Data Processing Tests
;;; ============================================================

(deftest process-data-test
  (testing "process-data filters evens, doubles, and sums"
    (is (= 0 (core/process-data [])))
    (is (= 4 (core/process-data [1 2])))
    (is (= 12 (core/process-data [1 2 3 4])))
    (is (= 60 (core/process-data (range 1 11))))))

(deftest classify-number-test
  (testing "number classification"
    (are [expected n] (= expected (core/classify-number n))
      :positive 1
      :positive 100
      :negative -1
      :negative -100
      :zero 0)))

(deftest safe-divide-test
  (testing "safe division"
    (is (= 2 (core/safe-divide 6 3)))
    (is (= 2.5 (core/safe-divide 5 2)))
    (is (nil? (core/safe-divide 5 0)))))

;;; ============================================================
;;; Higher-Order Function Tests
;;; ============================================================

(deftest compose-fns-test
  (testing "function composition"
    (let [add1 #(+ % 1)
          double #(* % 2)
          composed (core/compose-fns add1 double)]
      (is (= 6 (composed 2)))
      (is (= 10 (composed 4))))))

(deftest memoize-fn-test
  (testing "memoization caches results"
    (let [call-count (atom 0)
          slow-fn (fn [x]
                    (swap! call-count inc)
                    (* x x))
          memo-fn (core/memoize-fn slow-fn)]
      (is (= 4 (memo-fn 2)))
      (is (= 1 @call-count))
      (is (= 4 (memo-fn 2)))
      (is (= 1 @call-count))
      (is (= 9 (memo-fn 3)))
      (is (= 2 @call-count)))))

;;; ============================================================
;;; Protocol Tests
;;; ============================================================

(deftest printable-protocol-test
  (testing "Person to-string"
    (let [person (core/->Person "Alice" 30 "alice@example.com")]
      (is (= "Person{name=Alice, age=30}" (core/to-string person)))))
  (testing "Vector to-string"
    (is (= "[1, 2, 3]" (core/to-string [1 2 3])))))

;;; ============================================================
;;; Multimethod Tests
;;; ============================================================

(deftest area-test
  (testing "circle area"
    (is (< (Math/abs (- (* core/PI 25) (core/area {:type :circle :radius 5})))
           0.0001)))
  (testing "rectangle area"
    (is (= 24 (core/area {:type :rectangle :width 4 :height 6}))))
  (testing "triangle area"
    (is (= 15 (core/area {:type :triangle :base 6 :height 5}))))
  (testing "unknown shape throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (core/area {:type :hexagon})))))

;;; ============================================================
;;; Threading Macro Tests
;;; ============================================================

(deftest transform-data-test
  (testing "transform-data applies transformations"
    (let [input {:count 0 :temp "remove-me" :other "keep"}
          result (core/transform-data input)]
      (is (:processed result))
      (is (= 1 (:count result)))
      (is (nil? (:temp result)))
      (is (= "keep" (:other result))))))

(deftest process-collection-test
  (testing "process-collection filters and transforms"
    (is (= ["BAR" "FOO"] (core/process-collection ["foo" nil "bar"])))))

;;; ============================================================
;;; Utility Tests
;;; ============================================================

(deftest safe-parse-int-test
  (testing "valid integers"
    (is (= 42 (core/safe-parse-int "42")))
    (is (= -10 (core/safe-parse-int "-10")))
    (is (= 0 (core/safe-parse-int "0"))))
  (testing "invalid integers return nil"
    (is (nil? (core/safe-parse-int "abc")))
    (is (nil? (core/safe-parse-int "12.34")))
    (is (nil? (core/safe-parse-int "")))))

(deftest valid-email-test
  (testing "valid emails"
    (is (core/valid-email? "test@example.com"))
    (is (core/valid-email? "user.name+tag@domain.co.uk")))
  (testing "invalid emails"
    (is (not (core/valid-email? "invalid")))
    (is (not (core/valid-email? "@example.com")))
    (is (not (core/valid-email? "test@")))))

(deftest parse-log-line-test
  (testing "valid log lines"
    (is (= {:timestamp "2024-01-15"
            :level :info
            :message "Application started"}
           (core/parse-log-line "2024-01-15 [INFO] Application started"))))
  (testing "invalid log lines return nil"
    (is (nil? (core/parse-log-line "not a log line")))))

;;; ============================================================
;;; State Tests
;;; ============================================================

(deftest counter-test
  (testing "counter operations"
    (core/reset-counter!)
    (is (= 0 @core/counter))
    (core/increment-counter!)
    (is (= 1 @core/counter))
    (core/increment-counter!)
    (core/increment-counter!)
    (is (= 3 @core/counter))
    (core/reset-counter!)
    (is (= 0 @core/counter))))
