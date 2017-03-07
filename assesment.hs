import Data.List
--Write a function inlist which, given a list of Integers and an Integer n, returns 
--a boolean indicating whether n occurs in the list.

--1
inlist :: [Int] -> Int -> Bool
inlist [] x  = False
inlist (y:ys) x = y == x || inlist ys x


--2
exactlyonce :: [Int] -> Int -> Bool
exactlyonce list n
  | length list == 0 = error "List is empty"
  | n `notElem` list = False
  | otherwise = n `notElem` (delete n list)


--3
--count :: (Eq a) => [a] -> a -> Int
--count list input = length (filter (==input) list)


equalones :: [Int] -> [Int] -> Bool
equalones list list2
  | (length (filter (==1) list)) == (length (filter (==1) list2)) = True
  | otherwise = False

--4
replacenew :: Int -> [Int] -> [Int]
replacenew x [] = []
replacenew n (x:xs) =
  f x  : replacenew n xs
  where f x = (n-x)*(n-x)


--5
--numb :: Int -> [Int] -> Int
--numb n list = foldr (+) n list

--addthemup :: [[Int]] -> Int
--addthemup list = foldr (+) 0 list * foldr (+) 0 list1

addthemup :: [[Int]] -> Int
addthemup list = foldr (*) 1 (map sum list)

--6
square :: (Num a) => a -> a
square num = num * num

repeatnew :: (a -> a) -> Int -> a -> a
repeatnew _ 0 input   = input                      -- Base Cases (a bit silly)
repeatnew f 1 input   = f input                    -- Base Cases
repeatnew f num input = repeatnew f (num - 1) (f input)

--7
antepenultimate1 :: [Int] -> Bool
antepenultimate1 (num:num1:_:[]) = num == 1 && num1 == 1
antepenultimate1 (_:xs@(_:_:_)) = antepenultimate1 xs
antepenultimate1 _ = False

--8
sequenceones :: [Int] -> Bool
sequenceones (1:1:_) = True
sequenceones (x:xs) = sequenceones xs
sequenceones _ = False
--function (x:y:xs)
--function ([y] ++ xs)


