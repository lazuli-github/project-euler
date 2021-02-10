flipInt :: Int -> Int
flipInt = read . reverse . show

-- This function works out the largest palindrome resulting from the
-- multiplication of two numbers, each containing 3 digits.
largestPalindrome :: Int
largestPalindrome = let pairs = testEachCombination start start
                    in largestPalindromeInList (map (uncurry (*)) pairs)
  where start = 999
        testEachCombination x y
          | x < 100 = []
          | y < 100 = testEachCombination (x - 1) start
          | x * y == flipInt (x * y) = (x, y) : testEachCombination x (y - 1)
          | otherwise = testEachCombination x (y - 1)
        largestPalindromeInList = foldr1 (\x y -> if x >= y then x else y)

main :: IO ()
main = print largestPalindrome