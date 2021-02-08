-- Generates the Fibonacci sequence up to, but not including, n
getFibUpTo :: Integer -> [Integer]
getFibUpTo n = takeWhile (< n) fibList
  where fibList = 0 : 1 : zipWith (+) fibList (tail fibList)

sumEvenNumbers :: [Integer] -> Integer
sumEvenNumbers = sum . filter even

main :: IO ()
main = do
  print (sumEvenNumbers $ getFibUpTo 4000000)