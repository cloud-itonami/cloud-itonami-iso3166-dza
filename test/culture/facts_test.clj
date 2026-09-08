(ns culture.facts-test
  (:require [clojure.edn :as edn]
            [kotoba.lang.text :as str]
            [clojure.test :refer [deftest is]]
            [culture.facts :as facts]))

(deftest dza-has-culture-basis
  (let [sb (facts/spec-basis "DZA")]
    (is (= 8 (count sb)))
    (is (= (count sb) (count (set (map :culture/id sb)))))
    (is (every? #(str/starts-with? (:culture/url %) "https://") sb))
    (is (every? #(= "DZA" (:culture/country %)) sb))
    (is (every? #(nil? (:culture/municipality %)) sb))
    (is (every? #(seq (:culture/summary %)) sb))
    (is (every? #(string? (:culture/retrieved-at %)) sb))))

(deftest unknown-jurisdiction-has-no-basis
  (is (nil? (facts/spec-basis "MAR")))
  (is (nil? (facts/spec-basis "zzz"))))

(deftest coverage-is-honest
  (let [c (facts/coverage ["DZA" "MAR"])]
    (is (= 2 (:requested c)))
    (is (= 1 (:covered c)))
    (is (= ["MAR"] (:missing-jurisdictions c)))))

(deftest by-kind-filters
  (is (= 4 (count (facts/by-kind "DZA" :dish))))
  (is (= ["dza.product.deglet-nour"]
         (mapv :culture/id (facts/by-kind "DZA" :product))))
  (is (empty? (facts/by-kind "DZA" :other)))
  (is (empty? (facts/by-kind "MAR" :dish))))

(deftest tx-file-matches-catalog
  (let [tx (edn/read-string (slurp "data/culture-tx.edn"))
        flat (mapcat val (sort-by key facts/catalog))]
    (is (= (vec flat) (vec tx)))))
