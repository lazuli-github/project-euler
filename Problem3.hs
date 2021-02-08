calculatePrimeFactors :: Int -> [Int]
calculatePrimeFactors n = trialDivision n 2
  where trialDivision n f
          | n `mod` f == 0 = f : trialDivision (n `div` f) f
          | n > 1 = trialDivision n (f + 1)
          | otherwise = []

calculateLargestPrimeFactor :: Int -> Int
calculateLargestPrimeFactor = last . calculatePrimeFactors

main :: IO ()
main = do
  print (calculateLargestPrimeFactor 600851475143)