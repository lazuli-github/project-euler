smallestMultiple :: Integer
smallestMultiple = foldr1 lcm [1..20]

main :: IO ()
main = do
  print smallestMultiple