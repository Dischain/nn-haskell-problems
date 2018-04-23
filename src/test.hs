myPack :: (Eq a) => [a] -> [[a]]
myPack = foldr myPack' []
  where myPack' x []     = [[x]]
        myPack' x (y : ys) =
             if x == (head y) then ((x : y) : ys) else ([x] : y : ys)

myEncode :: Eq a => [a] -> [(Int, a)]
myEncode = map (\x -> (length x, head x)) . myPack