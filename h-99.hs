import Data.List(group)
import System.Random(randomRIO)
import Control.Monad(replicateM)
-- Problem 1
myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs
    
myLast' :: [a] -> a
myLast' = head . reverse
    
myLast'' :: [a] -> a
myLast'' = foldl1 (flip const)    
    
myLast''' :: [a] -> a
myLast''' = foldr1 (flip const)
-- The following are confusing and amazing. 
    
myLast'''' :: [a] -> a
myLast'''' = foldr1 (const id)
    
myLast''''' :: [a] -> a
myLast''''' = foldl1 (const id)

-- Problem 2

myButLast :: [a] -> a
myButLast x = reverse x !! 1

myButLast' :: [a] -> a
myButLast' [x, _] = x
myButLast' (_ : xs) = myButLast' xs

myButLast'' :: [a] -> a
myButLast'' = last . init

myButLast''' :: [a] -> a
myButLast''' = head . tail . reverse

-- Problem 3

elementAt :: [a] -> Int -> a
elementAt xs i = xs !! (i - 1)

elementAt' :: [a] -> Int -> a
elementAt' (x : _) 1 = x 
elementAt' (_ : xs) i 
  | i < 1 = error "Index out of bounds"
  | otherwise = elementAt' xs (i-1)
elementAt' [] _ = error "Index out of bounds"

elementAt'' :: [a] -> Int -> a
elementAt'' xs i = last . take i $ xs 

-- Problem 4

myLength :: [a] -> Int
myLength [] = 0 
myLength (_ : xs) = 1 + myLength xs

myLength' :: [a] -> Int
myLength' = sum . map (\x -> 1)

myLength'' :: [a] -> Int
myLength'' = foldl (\n _ -> n+1) 0

myLength''' :: [a] -> Int
myLength''' = foldr (\_ n -> n+1) 0

myLength'''' :: [a] -> Int
myLength'''' = foldr (const (+1)) 0

-- Prbolem 5

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

reverse''' :: [a] -> [a]
reverse''' list = reverse'''' list []
  where reverse'''' [] reversed = reversed
        reverse'''' (x:xs) reversed = reverse'''' xs (x:reversed)

-- Problem 6

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list = list == reverse list

isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' [] = True
isPalindrome' [_] = True
isPalindrome' list = (head list) == (last list) && (isPalindrome' $ init $ tail list)

-- Problem 7

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

flatten' :: NestedList a -> [a]
flatten' (Elem x) = [x]
flatten' (List []) = []
flatten' (List (x:xs)) = flatten' x ++ flatten' (List xs)

flatten'' :: NestedList a -> [a]
flatten'' (Elem x) = [x]
flatten'' (List x) = foldr (++) [] $ map flatten'' x

-- Problem 8

compress :: Eq a => [a] -> [a]
compress = foldr compHelp []
  where compHelp x [] = [x]
        compHelp x (y:ys) 
          | x == y = y:ys
          | otherwise = x:y:ys
                        
compress' :: Eq a => [a] -> [a]
compress' [] = []
compress' (x:xs) = x : (compress $ dropWhile (== x) xs)

compress'' :: Eq a => [a] -> [a]
compress'' x = foldr (\a b -> if a == (head b) then b else a:b) [last x] x

compress''' :: Eq a => [a] -> [a]
compress''' = map head . Data.List.group

-- Problem 9

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = packHelper [] [] (x:xs)
  where packHelper result [] [] = result
        packHelper result current [] = result ++ [current]
        packHelper result [] (x:xs) = packHelper result [x] xs
        packHelper result current@(y:_) (x:xs) 
          | y == x = packHelper result (x:current) xs
          | otherwise = packHelper (result ++ [current]) [x] xs

pack' :: Eq a => [a] -> [[a]]
pack' = foldr packHelper []
  where packHelper item [] = [[item]]
        packHelper item result@(x:xs)
          | item == head x = (item:x) : xs
          | otherwise = [item] : result

pack'' :: Eq a => [a] -> [[a]]
pack'' [] = []
pack'' (x:xs) = let (first, rest) = span (==x) xs
                in (x:first) : pack'' rest

-- Problem 10

encode :: Eq a => [a] -> [(Int, a)]
encode list = let grouped = group list
              in zip (map length grouped) (map head grouped)

encode' :: Eq a => [a] -> [(Int, a)]
encode' = map (\x -> (length x, head x)) . group

encode'' :: Eq a => [a] -> [(Int, a)]
encode'' = foldr encodeHelper []
  where encodeHelper item [] = [(1, item)]
        encodeHelper item result@((count, x):xs)
          | item == x = (count + 1, x):xs
          | otherwise = (1, item) : result
                        
-- Problem 11

data Problem11 a = Single a | Multiple Int a deriving (Show)
encodeModified :: Eq a => [a] -> [Problem11 a]
encodeModified = map transformHelper . encode
  where transformHelper (1, x) = Single x
        transformHelper (n, x) = Multiple n x

-- Problem 12

decodeModified :: [Problem11 a] -> [a]
decodeModified = concatMap decodeHelper
  where decodeHelper (Single x) = [x]
        decodeHelper (Multiple n x) = replicate n x

-- Problem 13

encodeDirect :: Eq a => [a] -> [Problem11 a]
encodeDirect = foldr encodeHelper []
  where encodeHelper x result@(h:rest) 
          | (getContent h) == x = (Multiple (1 + getCount h) x) : rest
          | otherwise = (Single x) : result
        encodeHelper x [] = [(Single x)]
        getContent (Single x) = x
        getContent (Multiple _ x) = x
        getCount (Single _) = 1
        getCount (Multiple n _) = n

-- Problem 14

dupli :: [a] -> [a]
dupli = concatMap (\x->[x,x])

dupli' :: [a] -> [a]
dupli' = foldr (\x xs -> x:x:xs) []

-- Problem 15

repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

repli' :: [a] -> Int -> [a]
repli' = flip $ concatMap . replicate

-- Problem 16

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery _ 1 = []
dropEvery xs 0 = xs
dropEvery (x:xs) n = dropHelper [x] (n-1) xs
  where dropHelper result i [] = result
        dropHelper result 1 (_:ys) = dropHelper result n ys
        dropHelper result i (y:ys) = dropHelper (result ++ [y]) (i-1) ys

dropEvery' :: [a] -> Int -> [a]
dropEvery' xs 0 = xs
dropEvery' [] _ = []
dropEvery' xs n = map snd $ filter ((n /=) . fst) $ zip (cycle [1..n]) xs

-- Problem 17

split :: [a] -> Int -> ([a], [a])
split xs n = splitHelper [] n xs
  where splitHelper list 0 ys = (list, ys) 
        splitHelper list i [] = (list, [])
        splitHelper list i (y:ys) = splitHelper (list ++ [y]) (i-1) ys

split' :: [a] -> Int -> ([a], [a])
split' xs 0 = ([], xs)
split' [] i = ([], [])
split' (x:xs) n = (x:f, l) where (f,l) = split' xs (n-1)
                                 
-- Problem 18

slice :: [a] -> Int -> Int -> [a]
slice xs h t = sliceHelper [] xs 1
  where sliceHelper l [] _ = l
        sliceHelper l (y:ys) i
          | i > t = l
          | i < h = sliceHelper l ys (i+1) 
          | otherwise = sliceHelper (l ++ [y]) ys (i+1)

slice' :: [a] -> Int -> Int -> [a]
slice' xs h t = map snd $ filter ((>=h) . fst) $ zip [1..t] xs

slice'' :: [a] -> Int -> Int -> [a]
slice'' [] _ _ = []
slice'' (x:xs) h t
  | h > 1 = slice'' xs (h - 1) (t - 1)
  | t < 1 = []
  | otherwise = x:slice'' xs (h - 1) (t - 1)

-- Problem 19

rotate :: [a] -> Int -> [a]
rotate xs 0 = xs
rotate xs i 
  | i > len = rotate xs (mod i len)
  | i > 0 = swapConcat $ splitAt i xs
  | i < 0 = rotate xs $ length xs + i
  where swapConcat (h,t) = t ++ h
        len = length xs
        
rotate' :: [a] -> Int -> [a]
rotate' xs 0 = xs
rotate' [] _ = []
rotate' l@(x:xs) i
  | i > len = rotate l (mod i len)
  | i > 0 = rotate' (xs ++ [x]) (i - 1)
  | i < 0 = rotate' l $ length l + i
  where len = length l

-- Problem 20

removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt _ [] = (Nothing, [])
removeAt 0 (x:xs) = (Just x, xs)
removeAt k (x:xs) = let (a,r) = removeAt (k-1) xs in (a, x:r)

removeAt' :: Int -> [a] -> (a, [a])
removeAt' k xs = let (h, t) = splitAt (k + 1) xs in (last h, init h ++ t)

-- Problem 21

insertAt :: a -> [a] -> Int -> [a]
insertAt el xs 1 = el:xs
insertAt _ [] _ = []
insertAt el (x:xs) n = x:insertAt el xs (n-1)

insertAt' :: a -> [a] -> Int -> [a]
insertAt' el xs n = let (h,t)=splitAt n xs in h ++ el:t

-- Problem 22

range :: Int -> Int -> [Int]
range f l 
  | f <= l = [f..l]
  | otherwise = reverse $ range l f

range' :: Int -> Int -> [Int]
range' f l
  | f == l = [f]
  | f < l = f:range' (f+1) l
  | f > l = f:range' (f-1) l

-- Problem 23

rnd_select :: [a] -> Int -> IO [a]
rnd_select [] _ = return []
rnd_select xs n
  | n <= 0 = error "n must be greater than zero."
  | otherwise = replicateM n rand
  where rand = do r <- randomRIO (0, (length xs) - 1)
                  return (xs!!r)

-- Problem 24

diff_select :: Int -> Int -> IO [Int]
diff_select n m = diff_select' n [1..m]

diff_select' :: Int -> [a] -> IO [a]
diff_select' 0 _ = return []
diff_select' _ [] = error "too few elements to choose from"
diff_select' n xs = do r <- randomRIO (0,(length xs) - 1)
                       let remain = take r xs ++ drop (r+1) xs
                       rest <- diff_select' (n-1) remain
                       return ((xs!!r):rest)

-- Problem 25

rnd_permu :: [a] -> IO [a]
rnd_permu xs = diff_select' (length xs) xs

-- Problem 26

combinations :: Int -> [a] -> [[a]]
combinations _ [] = []
combinations 1 xs = map (\x -> [x]) xs
combinations n (x:xs) = combinations n xs ++ (map (x:) $ combinations (n-1) xs)

-- Problem 27

-- groupDisjoint :: [Int] -> [a] -> [[[a]]]
-- groupDisjoint d s 
--   | sum d /= length s = error "not valid parameter"