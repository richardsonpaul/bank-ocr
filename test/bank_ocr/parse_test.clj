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

(deftest test-parse-tree
  (testing "zero"
    (is (= 0 (->numeral (nums 0)))))
  (testing "one"
    (is (= 1 (->numeral (nums 1)))))
  (testing "two"
    (is (= 2 (->numeral (nums 2)))))
  (testing "three"
    (is (= 3 (->numeral (nums 3)))))
  (testing "four"
    (is (= 4 (->numeral (nums 4)))))
  (testing "five"
    (is (= 5 (->numeral (nums 5)))))
  (testing "six"
    (is (= 6 (->numeral (nums 6)))))
  (testing "seven"
    (is (= 7 (->numeral (nums 7)))))
  (testing "eight"
    (is (= 8 (->numeral (nums 8)))))
  (testing "nine"
    (is (= 9 (->numeral (nums 9))))))
