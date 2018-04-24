-- Problem 21
-- Insert an element at a given position into a list.
-- Example:
--   myInsertAt 'x' "abcd" 2
myInsertAt :: a -> [a] -> Int -> [a]
myInsertAt x ys 1 = x : ys
myInsertAt x (y:ys) n = y : myInsertAt x ys (n-1)

-- Problem 26
-- Generate the combinations of K distinct objects chosen from the N 
-- elements of a list
-- Example:
--   myCombinations 3 "abcdef" = ["abc","abd","abe",...]
myCombinations :: Int -> [a] -> [[a]]
myCombinations 0 _ = [[]]
myCombinations n l = 
  do 
    x : xs <- tails l
    ys <- myCombinations (n - 1) xs
    return (x : ys)
  where
    tails :: [a] -> [[a]]
    tails [] = [[]]
    tails [x] = [[x]] 
    tails (x : xs) = (xs : (tails xs))

-- Problem 27
-- Group the elements of a set into disjoint subsets.
-- Example:
--   group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"] = 
--                 [[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...]
combination :: Int -> [a] -> [([a],[a])]
combination 0 xs     = [([],xs)]
combination n []     = []
combination n (x:xs) = ts ++ ds
  where
    ts = [ (x:ys,zs) | (ys,zs) <- combination (n-1) xs ]
    ds = [ (ys,x:zs) | (ys,zs) <- combination  n    xs ]
 
myGroup :: [Int] -> [a] -> [[[a]]]
myGroup [] _ = [[]]
myGroup (n:ns) xs =
    [ g:gs | (g,rs) <- combination n xs
           ,  gs    <- myGroup ns rs ]