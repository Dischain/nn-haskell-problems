-- Problem 1
-- Find the last element of a list
-- Example:
--   myLast [1, 2, 3, 4] = 4
myLast :: [a] -> a
myLast [] = error "Empty list!"
myLast [x] = x
myLast (_ : xs) = myLast xs

-- Problem 2
-- Find last but one element of a list
-- Example:
--   myButLust [1, 2, 3, 4] = 3
myButLast :: [a] -> a
myButLast [] = error "Empty list!"
myButLast [x, _] = x
myButLast (_ : xs) = myButLast xs

-- Problem 3
-- Find the k-th element of a list
-- The first element in the list is 1
-- Example
--   elementAt [1, 2, 3, 4] 3 = 3
elementAt :: [a] -> Int -> a
elementAt [] _ = error "Index out of bounds"
elementAt (x : _) 1 = x
elementAt (_ : xs) k
  | k < 1     = error "Index out of bounds" 
  | otherwise = elementAt xs (k - 1)

-- Problem 4
-- Find the number of elements of a list
-- Example:
--   myLength [123, 123, 323] = 3
myLength :: [a] -> Int
myLength list = myLength' list 0
  where
    myLength' [] n = n
    myLength' (_ : xs) n = myLength' xs (n + 1)

-- Problem 5
-- Reverse a list
-- Example:
--   myReverse [1, 2, 3] = [3, 2, 1]
myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse [x, y] = [y, x]
myReverse (x : xs) = myReverse xs ++ [x]

-- With tail call optimization
myReverseTailRec :: [a] -> [a]
myReverseTailRec (x : xs) = myReverseTailRec' xs [x]
  where
    myReverseTailRec' [] acc = acc
    myReverseTailRec' (x : xs) acc = myReverseTailRec' xs (x : acc)

-- Problem 6
-- Find out whether a list is a palindrome
-- Example:
--   myPalindrome [1, 2, 3] = False
myPalindrome :: (Eq a) => [a] -> Bool
myPalindrome [] = True
myPalindrome xs = xs == (reverse xs)

-- Problem 7
-- Flatten a nested list structure
-- Example:
--   myFlatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
-- Because of lists in Haskell are homogeneouse, we have to define a new data type
data NestedList a = Elem a | List [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten (Elem a)   = [a]
myFlatten (List [])     = []
myFlatten (List (x:xs)) = myFlatten x ++ myFlatten (List xs)

-- Problem 8
-- Eliminate consecutive duplicates of list elements
-- Example:
--   compress "aaaabccaadeeee"
myCompress :: (Eq a) => [a] -> [a]
myCompress [] = error "Empty list!"
myCompress [a] = [a]
myCompress (x : xs@(y : _))
  | x == y = myCompress xs
  | otherwise = x : myCompress xs

-- Problem 9
-- Pack consecutive duplicates of list elements into sublists. 
-- If a list contains repeated elements they should be placed in separate sublists
-- Example:
--   myPack [a a a a b c c a a d e e e e] = [[A A A A] [B] [C C] [A A] [D] [E E E E]]
myPack :: (Eq a) => [a] -> [[a]]
myPack = foldr myPack' []
  where myPack' x []     = [[x]]
        myPack' x (y : ys) =
             if x == (head y) then ((x : y) : ys) else ([x] : y : ys)

-- Problem 10
-- Implement run-length encoding of a list
-- Example:
--   myEncode [a a a a b c c a a d e e e e] = [(4 A) (1 B) (2 C) (2 A) (1 D)(4 E)]
myEncode :: Eq a => [a] -> [(Int, a)]
myEncode = map (\x -> (length x, head x)) . myPack