(ns bank-ocr.parse-test
  (:require [clojure.test :refer :all]
            [bank-ocr.parse :refer :all]))

(defn- ->printed [c] (if (even? (mod c 3)) \| \_))

(defn- ->row-chars [indices number]
  (let [print? (set number)]
    (for [i indices]
      (if (print? i) (->printed i) \space))))

(defn- ->row-strs [nums]
  (let [rows (->> (range 9) (partition 3))
        ->str #(apply str %)]
    (->> (for [row rows
               num nums]
           (->row-chars row num))
         (map ->str)
         (partition (count nums))
         (map ->str))))

(defn- print-nums [& nums]
  (let [rows (-> (->row-strs nums)
                 (interleave (repeat \newline)))]
    (print (apply str rows))))

(deftest test-number-format
  (testing "all zeroes"
    (is (= [" _  _  _  _  _  _  _  _  _ "
            "| || || || || || || || || |"
            "|_||_||_||_||_||_||_||_||_|"]
           (->row-strs [zero zero zero zero zero zero zero zero zero]))))
  (testing "all ones"
    (is (= ["                           "
            "  |  |  |  |  |  |  |  |  |"
            "  |  |  |  |  |  |  |  |  |"]
           (->row-strs [one one one one one one one one one]))))
  (testing "all twos"
    (is (= [" _  _  _  _  _  _  _  _  _ "
            " _| _| _| _| _| _| _| _| _|"
            "|_ |_ |_ |_ |_ |_ |_ |_ |_ "]
           (->row-strs [two two two two two two two two two]))))
  (testing "all three s"
    (is (= [" _  _  _  _  _  _  _  _  _ "
            " _| _| _| _| _| _| _| _| _|"
            " _| _| _| _| _| _| _| _| _|"]
           (->row-strs [three three three three three three three three three]))))
  (testing "all fours"
    (is (= ["                           "
            "|_||_||_||_||_||_||_||_||_|"
            "  |  |  |  |  |  |  |  |  |"]
           (->row-strs [four four four four four four four four four]))))
  (testing "all fives"
    (is (= [" _  _  _  _  _  _  _  _  _ "
            "|_ |_ |_ |_ |_ |_ |_ |_ |_ "
            " _| _| _| _| _| _| _| _| _|"]
           (->row-strs [five five five five five five five five five]))))
  (testing "all sixes"
    (is (= [" _  _  _  _  _  _  _  _  _ "
            "|_ |_ |_ |_ |_ |_ |_ |_ |_ "
            "|_||_||_||_||_||_||_||_||_|"]
           (->row-strs [six six six six six six six six six]))))
  (testing "all sevens"
    (is (= [" _  _  _  _  _  _  _  _  _ "
            "  |  |  |  |  |  |  |  |  |"
            "  |  |  |  |  |  |  |  |  |"]
           (->row-strs [seven seven seven seven seven seven seven seven seven]))))
  (testing "all eight s"
    (is (= [" _  _  _  _  _  _  _  _  _ "
            "|_||_||_||_||_||_||_||_||_|"
            "|_||_||_||_||_||_||_||_||_|"]
           (->row-strs [eight eight eight eight eight eight eight eight eight]))))
  (testing "all nines"
    (is (= [" _  _  _  _  _  _  _  _  _ "
            "|_||_||_||_||_||_||_||_||_|"
            " _| _| _| _| _| _| _| _| _|"]
           (->row-strs [nine nine nine nine nine nine nine nine nine]))))
  (testing "sequential numbers"
    (is (= ["    _  _     _  _  _  _  _ "
            "  | _| _||_||_ |_   ||_||_|"
            "  ||_  _|  | _||_|  ||_| _|"]
           (->row-strs [one two three four five six seven eight nine])))))
