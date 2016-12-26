import Test.HUnit
import Task1
import Task2
import Task3


main :: IO Counts
main = runTestTT $ TestList (concat [
    test_nEven, test_sum_ab_rec, test_sum_ab_iter, test_doubleElems])


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
    where assert' (lab, input, output) = TestCase $ assertEqual lab output (f input)
