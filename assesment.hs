import Data.List
--Write a function inlist which, given a list of Integers and an Integer n, returns 
--a boolean indicating whether n occurs in the list.
inlist :: [Int] -> Int -> Bool
--member :: Eq [Int] => Int -> Bool
inlist [] x  = False
inlist (y:ys) x = y == x || inlist ys x

exactlyonce :: [Int] -> Int -> Bool
exactlyonce list n
  | length list == 0 = error "List is empty"
  | n `notElem` list = False
  | otherwise = n `notElem` (delete n list)

count :: (Eq a) => [a] -> a -> Int
count list input = length (filter (==input) list)


equalones :: [Int] -> [Int] -> Bool
equalones list list2
  | (count list 1) == (count list2 1) = True
  | otherwise = False

mydelete :: Int -> [Int] -> [Int]
mydelete x [] = []
mydelete n (x:xs) =
  f x  : mydelete n xs
  where f x = (n-x)*(n-x)

numb :: Int -> [Int] -> Int
numb n list = foldr (+) n list

addthemup :: [Int] -> [Int] -> Int
addthemup list list1 = numb 0 list * numb 0 list1

square :: (Num a) => a -> a
square num = num * num

iter :: (a -> a) -> Int -> a -> a
iter _ 0 input   = input                      -- Base Cases (a bit silly)
iter f 1 input   = f input                    -- Base Cases
iter f num input = iter f (num - 1) (f input)

antepenultimate1 :: [Int] -> Bool
antepenultimate1 (num:num1:_:[]) = num == 1 && num1 == 1
antepenultimate1 (_:xs@(_:_:_)) = antepenultimate1 xs
antepenultimate1 _ = False

sequenceones :: [Int] -> Bool
sequenceones (1:1:_) = True
sequenceones (x:xs) = sequenceones xs
sequenceones _ = False
--function (x:y:xs)
--function ([y] ++ xs)


