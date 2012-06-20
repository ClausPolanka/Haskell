import HUnit

test1 = TestCase (assertEqual "for (foo 3)," (1,2) (foo 3))

tests = TestList [TestLabel "test1" test1]

foo :: Int -> (Int, Int)
foo x = (1, x)

