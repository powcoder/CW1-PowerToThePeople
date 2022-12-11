https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
module Power where
-- Code for the Power to the People Coursework.
-- Write all your code in this file.

---------------------------------------------
-- An example implementation of the power function.
-- *DO NOT EDIT THIS FUNCTION*

power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

-- Task 1 -------------------------

stepsPower :: Integer -> Integer -> Integer
stepsPower n k = error "Implement me!"

-- Task 2 -------------------------

power1 :: Integer -> Integer -> Integer
power1 n k = error "Implement me!"

-- Task 3 -------------------------

power2 :: Integer -> Integer -> Integer
power2 n k = error "Implement me!"

-- Task 4 -------------------------

comparePower1 :: Integer -> Integer -> Bool
comparePower1 n k = error "Implement me!"

comparePower2 :: Integer -> Integer -> Bool
comparePower2 n k = error "Implement me!"

-- Task 5 -------------------------

-- Each entry should be in this format: (n, k, result of comparePower1, result of comparePower2)
comparisonList :: [Integer] -> [Integer] -> [(Integer, Integer, Bool, Bool)]
comparisonList ns ks = error "Implement me!"
