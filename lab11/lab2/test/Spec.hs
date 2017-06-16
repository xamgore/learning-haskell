import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Data.List
import Data.Ord

import Task1
import Task2


main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]


properties = testGroup "(checked by QuickCheck)" [
        testProperty "removeNeg"   prop_removeNeg,
        testProperty "doubleEvens" prop_doubleEvens,
        testProperty "zip'"        prop_zip
    ]

prop_removeNeg :: [Int] -> Bool
prop_removeNeg = (Nothing ==) . find (< 0) . removeNeg

prop_doubleEvens :: [Int] -> Bool
prop_doubleEvens xs = map (\x -> if even x then 2*x else x) xs == doubleEvens xs

prop_zip :: [Int] -> [Int] -> Bool
prop_zip xs ys = zip xs ys == zip' xs ys



unitTests = testGroup "Unit tests" $ concat [
        test_nEven, test_sum_ab_rec, test_sum_ab_iter, test_doubleElems
    ]

test_nEven = testWithData nEven [
        ("nEven 1", [1, 2, 3, 4], 2),
        ("nEven 2", [1, 1, 1, 3], 0),
        ("nEven 3", [4, 4, 4, 4], 4) ]

test_sum_ab_rec = testWithData (uncurry sum_ab_rec) [
        ("sum_ab_rec 1", (1, 10), sum [1..10]),
        ("sum_ab_rec 2", (-10, 0), sum [-10..0]),
        ("sum_ab_rec 3", (1, 1), sum [1..1]) ]

test_sum_ab_iter = testWithData (uncurry sum_ab_iter) [
        ("sum_ab_rec 1", (1, 10), sum [1..10]),
        ("sum_ab_rec 2", (-10, 0), sum [-10..0]),
        ("sum_ab_rec 3", (1, 1), sum [1..1]) ]

test_doubleElems = testWithData doubleElems [
        ("doubleElems 1", [1..10], [2,4..20]),
        ("doubleElems 2", [-2, 0, 2], [-4, 0, 4]),
        ("doubleElems 3", [1.5], [3.0]) ]


testWithData f data' = map assert' data'
    where assert' (lab, input, output) = testCase lab $ f input @?= output
