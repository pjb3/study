(ns examples.test.memoized-male-female
  (:use clojure.contrib.test-is 
        examples.memoized-male-female))

(deftest test-hofstadter-m-f
  (are (= _1 _2)
       [0, 0, 1, 2, 2, 3, 4, 4, 5, 6, 6, 7, 7, 8, 9, 9, 10, 11, 11, 12, 12]
       (map m (range 21))
       [1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 8, 8, 9, 9, 10, 11, 11, 12, 13]
       (map f (range 21))))

