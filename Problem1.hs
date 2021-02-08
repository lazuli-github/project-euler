-- Sum the multiples of x and y below n
sumMultiples :: Int -> Int -> Int -> Int
sumMultiples x y n = sum (filter (\z -> z `mod` x == 0 || z `mod` y == 0) [1..n - 1])

main :: IO ()
main = print (sumMultiples 3 5 1000)
