https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
module Main (main) where

import Power

import Test.Tasty -- This is a testing framework which can nicely format our QuickCheck tests
import Test.Tasty.QuickCheck -- This is just Test.QuickCheck with a few extras for Tasty

import Control.Exception (evaluate)
import Data.Bifunctor ( Bifunctor(bimap) )
import Data.List (sortOn)
import Data.Time ( NominalDiffTime, diffUTCTime, getCurrentTime )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Power to the People Coursework"
          [task1, task2, task3, task4, task5]

------------------------------------------------------
-- Task 1                                           --
------------------------------------------------------

task1 :: TestTree
task1 = testGroup "Task 1 - stepsPower"
  [ testProperty "stepsPower returns the correct number of recursion steps"
      stepsPowerAccurate
  ]

stepsPowerAccurate :: Integer -> Integer -> Property
stepsPowerAccurate n k = k >= 0 ==> stepsPower n k === powerStepsRec n k
  where
    -- The power function, but it counts how many steps its done instead of calculating the power
    powerStepsRec :: Integer -> Integer -> Integer
    powerStepsRec n k
      | k < 0 = error "power: negative argument"
    powerStepsRec n 0  = 1
    powerStepsRec n k  = 1 + powerStepsRec n (k-1)

------------------------------------------------------
-- Power laws/rules/identities       --
------------------------------------------------------

powerLaws :: (Integer -> Integer -> Integer) -> TestTree
powerLaws powerFunc
  = testGroup "Power laws"
      [ testProperty "Power addition identity: n^(k+j) = n^k * n^j"
          (powerAdditionIdentity powerFunc)
      , testProperty "Power power identity: (n^k)^j = n^(k * j)"
          (powerPowerIdentity powerFunc)
      , testProperty "Base multiplication identity: (n * m)^k = n^k * m^k"
          (baseMultiplicationIdentity powerFunc)
      ]

-- n^(k+j) = n^k * n^j
powerAdditionIdentity ::  (Integer -> Integer -> Integer) -> Integer -> Integer -> Integer -> Property
powerAdditionIdentity powerFunc n k j = n /= 0 && k >= 0 && j >= 0
  ==> powerFunc n (k+j) === (powerFunc n k * powerFunc n j)

-- (n^k)^j = n^(k * j)
powerPowerIdentity ::  (Integer -> Integer -> Integer) -> Integer -> Integer -> Integer -> Property
powerPowerIdentity powerFunc n k j = n /= 0 && k >= 0 && j >= 0
  ==> powerFunc (powerFunc n k) j === powerFunc n (k * j)

-- (n * m)^k = n^k * m^k
baseMultiplicationIdentity :: (Integer -> Integer -> Integer) -> Integer -> Integer -> Integer -> Property
baseMultiplicationIdentity powerFunc n m k = n /= 0 && m /= 0 && k >= 0
  ==> powerFunc (n * m) k === (powerFunc n k * powerFunc m k)

------------------------------------------------------
-- Task 2                                           --
------------------------------------------------------

task2 :: TestTree
task2 = testGroup "Task 2 - power1"
      [ powerLaws power1 ]

------------------------------------------------------
-- Task 3                                           --
------------------------------------------------------

task3 :: TestTree
task3 = testGroup "Task 3 - power2"
  [ powerLaws power2
  , testProperty "power2 more efficient than power for large exponent?"
      power2EfficiencyTest
  ]

-- If this takes too long on your machine, reduce lowerBound and upperBound.
power2EfficiencyTest :: Integer -> Property
power2EfficiencyTest n = n /= 0 ==> forAll (chooseBoundedIntegral (lowerBound, upperBound) :: Gen Int) (\k -> ioProperty $ do
  kInteger <- evaluate (fromIntegral k)
  power1Time <- measureTime (power0 n) kInteger
  power2Time <- measureTime (power2 n) kInteger
  pure (power1Time > power2Time))
  where
    lowerBound = 10000
    upperBound = 50000

measureTime :: Show res => (arg -> res) -> arg -> IO NominalDiffTime
measureTime f arg = do
    startTime <- getCurrentTime
    evaluate (f arg) -- Force (shallow) evaluation before recording end time
    endTime   <- getCurrentTime
    pure (diffUTCTime endTime startTime)

------------------------------------------------------
-- Task 4                            --
------------------------------------------------------

task4 :: TestTree
task4 = testGroup "Task 4 - comparison functions"
  [ testProperty "comparePower1 performs accurate comparison"
      comparePower1Test
  , testProperty "comparePower2 performs accurate comparison"
      comparePower2Test
  ]

comparePower1Test :: Integer -> Integer -> Property
comparePower1Test n k = k >= 0 ==> comparePower1 n k === obscureCompare power1 n k

comparePower2Test :: Integer -> Integer -> Property
comparePower2Test n k = k >= 0 ==> comparePower2 n k === obscureCompare power2 n k

-- This effectively implements comparePower1/comparePower2,
-- but it's horrendously over-complicated on purpose so it doesn't just give away how to do it.
-- It's much, much easier to implement the comparePower functions yourself
-- than it is to reverse-engineer this function.
obscureCompare :: (Integer -> Integer -> Integer) -> Integer -> Integer -> Bool
obscureCompare foo bar baz
  = either not (const True)
  $ foldr (flimflam bar baz) kthulu
  $ [power0, foo]
  where
    flimflam :: Integer -> Integer -> (Integer -> Integer -> Integer) -> Either Bool Integer -> Either Bool Integer
    flimflam bim bam ping pang = case pang of
      Left False -> Right (ping bim bam)
      Left True  -> Left True
      Right x    -> if ping bim bam == x then Right x else Left True

    kthulu = Left False

------------------------------------------------------
-- Task 5 - Efficiency                              --
------------------------------------------------------

task5 :: TestTree
task5 = testGroup "Task 5 - Running comparisons"
  [ testProperty "comparisonList ns ks contains entry for every combination of ns and ks"
      (\ns ks -> length (comparisonList ns ks) === length ns * length ks)
  , testProperty "comparisonList accurately reports the results of comparePower1 and comparePower2"
      comparisonListAccurate
  ]
  where
    comparisonListAccurate :: [Integer] -> [NonNegative Integer] -> Property
    comparisonListAccurate ns ks
      =   sortComparisonList (comparisonList ns ks')
      === sortComparisonList (obscureComparisonList ns ks')
      where
        ks' = map getNonNegative ks
    
    sortComparisonList = sortOn (\(n,k,test1,test2) -> (n, k))

-- Again, this is horrendously overcomplicated on purpose
obscureComparisonList :: [Integer] -> [Integer] -> [(Integer, Integer, Bool, Bool)]
obscureComparisonList ns ks = fooMap (bimap (uncurry comparePower1) (uncurry comparePower2)) (do k <- ks; n <- ns; pure ((n, k), (n,k)))
  where
    fooMap f xs = map (\val@((x,y),_) -> case f val of (m,n) -> (x,y,m,n)) xs

------------------------------------------------------
-- Misc                                             --
------------------------------------------------------

power0 :: Integer -> Integer -> Integer
power0 n k
   | k < 0 = error "power0: negative argument"
power0 n 0  = 1
power0 n k  = n * power0 n (k-1) 
