(ns bank-ocr.parse-test
  (:require [clojure.test :refer :all]
            [bank-ocr.parse :refer :all]))

(deftest parse-account-numbers
  (testing "parse all zeroes"
    (is (= (parse (str " _  _  _  _  _  _  _  _  _ "
                       "| || || || || || || || || |"
                       "|_||_||_||_||_||_||_||_||_|"))
           000000000)))
  (testing "parse all ones"
    (is (= (parse (str "                           "
                       "  |  |  |  |  |  |  |  |  |"
                       "  |  |  |  |  |  |  |  |  |"))
           111111111)))
  (testing "parse all twos"
    (is (= (parse (str " _  _  _  _  _  _  _  _  _ "
                       " _| _| _| _| _| _| _| _| _|"
                       "|_ |_ |_ |_ |_ |_ |_ |_ |_ "))
           222222222)))
  (testing "parse all threes"
    (is (= (parse (str " _  _  _  _  _  _  _  _  _ "
                       " _| _| _| _| _| _| _| _| _|"
                       " _| _| _| _| _| _| _| _| _|"))
           333333333)))
  (testing "parse all fours"
    (is (= (parse (str "                           "
                       "|_||_||_||_||_||_||_||_||_|"
                       "  |  |  |  |  |  |  |  |  |"))
           444444444)))
  (testing "parse all fives"
    (is (= (parse (str " _  _  _  _  _  _  _  _  _ "
                       "|_ |_ |_ |_ |_ |_ |_ |_ |_ "
                       " _| _| _| _| _| _| _| _| _|"))
           555555555)))
  (testing "parse all sixes"
    (is (= (parse (str " _  _  _  _  _  _  _  _  _ "
                       "|_ |_ |_ |_ |_ |_ |_ |_ |_ "
                       "|_||_||_||_||_||_||_||_||_|"))
           666666666)))
  (testing "parse all sevens"
    (is (= (parse (str " _  _  _  _  _  _  _  _  _ "
                       "  |  |  |  |  |  |  |  |  |"
                       "  |  |  |  |  |  |  |  |  |"))
           777777777)))
  (testing "parse all eights"
    (is (= (parse (str " _  _  _  _  _  _  _  _  _ "
                       "|_||_||_||_||_||_||_||_||_|"
                       "|_||_||_||_||_||_||_||_||_|"))
           888888888)))
  (testing "parse all nines"
    (is (= (parse (str " _  _  _  _  _  _  _  _  _ "
                       "|_||_||_||_||_||_||_||_||_|"
                       " _| _| _| _| _| _| _| _| _|"))
           999999999)))
  (testing "parse sequential numbers"
    (is (= (parse (str "    _  _     _  _  _  _  _ "
                       "  | _| _||_||_ |_   ||_||_|"
                       "  ||_  _|  | _||_|  ||_| _|"))
           123456789))))
