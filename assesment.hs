import Data.List
--Write a function inlist which, given a list of Integers and an Integer n, returns 
--a boolean indicating whether n occurs in the list.

--1
inlist :: [Int] -> Int -> Bool
inlist [] x  = False  -- base step, if epmty list return false
inlist (y:ys) x = y == x || inlist ys x -- if first element is equal to y then true, else call the function with the tail of the list and x again


--2
exactlyonce :: [Int] -> Int -> Bool
exactlyonce list n
  | length list == 0 = False -- if list empty then false
  | n `notElem` list = False -- if n not in the list then false
  | otherwise = n `notElem` (delete n list) -- else call check is n not an element of list without the previous n


--3
--count :: (Eq a) => [a] -> a -> Int
--count list input = length (filter (==input) list) count the number of input, in the list


equalones :: [Int] -> [Int] -> Bool
equalones list list2
  | (length (filter (==1) list)) == (length (filter (==1) list2)) = True --  if the number of ones is equal the true else false
  | otherwise = False

--4
replacenew :: Int -> [Int] -> [Int]
replacenew x [] = [] -- if empty list is a input return empty list
replacenew n (x:xs) =
  f x  : replacenew n xs  -- if actual input then applies the f(x) bellow to the list till the end
  where f x = (n-x)*(n-x)


--5
--numb :: Int -> [Int] -> Int
--numb n list = foldr (+) n list

--addthemup :: [[Int]] -> Int
--addthemup list = foldr (+) 0 list * foldr (+) 0 list1

addthemup :: [[Int]] -> Int
addthemup list = foldr (*) 1 (map sum list) -- using foldr, map and sum functions returns the end list out of the the inserted ones.

--6
square :: (Num a) => a -> a
square num = num * num

repeatnew :: (a -> a) -> Int -> a -> a
repeatnew _ 0 input   = input            -- if applied 0 times return the input           
repeatnew f 1 input   = f input          -- if applied 1 time the return the input after the function was applied once
repeatnew f num input = repeatnew f (num - 1) (f input) --  if more times the recursively keep applying n times

--7
antepenultimate1 :: [Int] -> Bool
antepenultimate1 (num:num1:_:[]) = num == 1 && num1 == 1 -- if 2nd and 3rd from last are 1 return true
antepenultimate1 (_:xs@(_:_:_)) = antepenultimate1 xs -- if not at the end return the tail of the list
antepenultimate1 _ = False -- if empty list then false

--8
sequenceones :: [Int] -> Bool
sequenceones (1:1:_) = True -- if two ones after each other then true
sequenceones (x:xs) = sequenceones xs -- otherwise apply the function to the tail of the list
sequenceones _ = False -- if empty list then false
--function (x:y:xs)
--function ([y] ++ xs)


