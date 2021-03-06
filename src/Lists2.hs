-- Problem 11
-- Modify run-length encoding such a way that if an element it is simply copied
-- un the result list. Only elements with duplicates are transferred as (N E) lists.
-- Example:
--   myEncodeModified "aabccaaddeee" = [(4 A) (1 B) (2 C) (2 A) (1 D)(4 E)]
data ListItem a = Single a | Multiple Int a

myEncodeModified :: (Eq a, Show a) => [a] -> [ListItem a]
myEncodeModified = map encodeHelper . myEncode
  where
    encodeHelper (1, x) = Single x
    encodeHelper (n, x) = Multiple n x
    myEncode = map (\x -> (length x, head x)) . myPack
    myPack = foldr myPack' []
      where myPack' x []     = [[x]]
            myPack' x (y : ys) 
              | x == (head y) = ((x : y) : ys) 
              | otherwise = ([x] : y : ys)

-- Problem 12
-- Decode a run-length encoded list
-- Example:
--   myDecode [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e'] =
--   "aaaabccaadeeee"
myDecode :: [ListItem a] -> [a]
myDecode = concatMap decodeHelper
  where 
    decodeHelper (Single x) = x
    decodeHelper (Multiple n x) = replicate n x

-- Problem 13
-- Implement run-lenght encoding of a list (direct solution)
-- Example:
--   myEncode [a a a a b c c a a d e e e e] = [(4 A) B (2 C) (2 A) D (4 E)]
myEncode' :: (Eq a) => [a] -> [(Int, a)]
myEncode' = foldr helper []
  where
    helper x [] = [(1, x)]
    helper x (y @ (n, b) : ys)
      | x == b = (n + 1, x) : ys
      | otherwise = (1, x) : y : ys

-- Problem 14
-- Duplicate the elements of a list
-- Example:
--   myDuplicate [a b c c d] = [a a b b c c c c d d]
myDuplicate :: [a] -> [a]
myDuplicate [] = []
myDuplicate (x : xs) = x : x : myDuplicate xs

-- Problem 15
-- Replicate the elements of a list a given number of times
-- Example:
--   myReplicate "abc" 3 = "aaabbbccc"
myReplicate :: [a] -> Int -> [a]
myReplicate []  _ = []
myReplicate x n = map (\i -> foldr (\_, acc -> acc : i) [] [1..n])

-- Problem 16
-- Drop every N'th element from a list
-- Example:
--   myDropEvery "abcdefghik" 3 = "abdeghk"
myDropEvery :: [a] -> Int -> [a]
myDropEvery (x : xs) n = myDropEvery' (x : xs) n 1 where
  myDropEvery' [] _ _ = []
  myDropEvery' (x : xs) count 1 = myDropEvery' xs count count
  myDropEvery' (x : xs) count n = x : myDropEvery' $ xs count (count - 1) 

-- Problem 17
-- Split a list into two parts; the length of the first part is given
-- Example:
--   mySplitAt [a, b, c, d, e] 2 = [[a, b], [c, d, e]]
mySplitAt :: [a] -> Int -> [a]
mySplitAt list n = mySplitAt' list n n where
  mySplitAt' [] _ _ = []
  mySplitAt'(x : xs) 1 = ([x] : xs])
  mySplitAt' (x : xs) n = ([x] : mySplitAt' xs n (n - 1))

-- Problem 18
-- Extract a slice from a list. 
-- Given two indices, i and k, the slice is the list containing the elements between 
-- the i'th and k'th element of the original list (both limits included). 
-- Start counting the elements with 1. 
-- Example:
-- mySlice ['a','b','c','d','e','f','g','h','i','k'] 3 7 = "cdefg"
mySlice :: [a] -> Int -> Int -> [a]
mySlice l 1 end = mySlice' l m []
  where
    mySlice' _ 0 acc = reverse acc
    mySlice' (x : xs) n acc = mySlice' xs (n - 1) (x : acc)
    mySlice' [] _ _ = []
mySlice (x : xs) start end = mySlice xs (n - 1) (m - 1)
mySlice [] _ _ = []

-- Problem 19
-- Rotate a list N places to the left.
-- Example:
-- myRotate ['a','b','c','d','e','f','g','h'] 3 = "defghabc"
myRotate:: [a] -> Int -> [a]
myRotate l @ (x : xs) n
  | n > length l = error "Number of elements to rotate exceeds the length of list"
  | n == length l = reverse l
  | otherwise = myRotate' l n [] where      
      myRotate' (x : xs) n acc = 
        | n > 1 = (myRotate' $ xs n - 1 (acc : x))
        | n == 1 = ((acc : x) : xs)

-- Problem 20
-- Remove the K'th element from a list.
-- Example:
--   myRemoveAt 2 "abcd" = ('b',"acd")
myRemoveAt :: Int -> [a] -> (a, [a])
myRemoveAt n l = myRemoveAt' n l []
  where
    myRemoveAt' 0 (x : xs) acc = (x, (acc : xs))
    myRemoveAt' n (x : xs) acc = myRemoveAt' $ n - 1 xs (acc : x)