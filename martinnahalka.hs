
--Write a function inlist which, given a list of Integers and an Integer n, returns 
--a boolean indicating whether n occurs in the list.

--1
inlist :: [Int] -> Int -> Bool
inlist [] x  = False  -- base step, if epmty list return false
inlist (y:ys) x = y == x || inlist ys x -- if first element is equal to y then true, else call the function with the tail of the list and x again
--inlist [2,3,2,4,7] 7
--True
--inlist [1,2,3,4,5,6,7] 8
--False
--inlist [1..10] 4
--True

--2
exactlyonce :: [Int] -> Int -> Bool 
exactlyonce list n = 1 == length (filter (==n) list) --filters the list and gets all the 'n' and checks if there is exactly 1 of it. We cannot do the last one as it is infinite list, and we can never go through the whole list in this way, so we will just get an error. 
--exactlyonce [2,3,2,4,3] 3
--False
--exactlyonce [1..15] 10
--True
--exactlyonce [1..30] 31
--False
--exactlyonce [1..] 15 - cannot do 


--3
--count :: (Eq a) => [a] -> a -> Int
--count list input = length (filter (==input) list) count the number of input, in the list


equalones :: [Int] -> [Int] -> Bool
equalones list list2
  | (length (filter (==1) list)) == (length (filter (==1) list2)) = True --  if the number of ones is equal the true else false
  | otherwise = False
--equalones [1,2,0] [3,5,1,1]
--False
--equalones [1,1] [3,1,5,7,1]
--True
--equalones [2,5,7,8] [3,5,6,2]
--True

--4
replacenew :: Int -> [Int] -> [Int]
replacenew x [] = [] -- if empty list is a input return empty list
replacenew n (x:xs) =
  f x  : replacenew n xs  -- if actual input then applies the f(x) bellow to the list till the end
  where f x = (n-x)*(n-x)
--replacenew 2 [3,6,9]
--[1,16,81]
--replacenew 3 [2,1,7,10]
--[1,4,16,49]
--replacenew 2 [1..10]
--[1,0,1,4,9,16,25,36,49,56]

--5
--numb :: Int -> [Int] -> Int
--numb n list = foldr (+) n list

--addthemup :: [[Int]] -> Int
--addthemup list = foldr (+) 0 list * foldr (+) 0 list1

addthemup :: [[Int]] -> Int
addthemup list = foldr (*) 1 (map sum list) -- using foldr, map and sum functions returns the end list out of the the inserted ones.
--addthemup [[1,3],[3,7]]
--40
--addthemup [[1,2,3],[9]]
--54
--addthemup [[1,2],[]]
--0

--6
square :: (Num a) => a -> a
square num = num * num

repeatnew :: (a -> a) -> Int -> a -> a
repeatnew _ 0 input   = input            -- if applied 0 times return the input           
repeatnew f 1 input   = f input          -- if applied 1 time the return the input after the function was applied once
repeatnew f num input = repeatnew f (num - 1) (f input) --  if more times the recursively keep applying n times
--repeatnew square 1 2 
--4
--repeatnew square 2 2 
--16
--repeatnew (++ " bla") 7 "here"
--here bla bla bla bla bla bla bla
--repeatnew square 2 0.5
--6.25e-2


--7
antepenultimate1 :: [Int] -> Bool
antepenultimate1 (num:num1:_:[]) = num == 1 && num1 == 1 -- if 2nd and 3rd from last are 1 return true
antepenultimate1 (_:xs@(_:_:_)) = antepenultimate1 xs -- if not at the end return the tail of the list
antepenultimate1 _ = False -- if empty list then false
--antepenultimate1 [2,1,1,2]
--True
--antepenultimate1 [1,2,1,1]
--False
--antepenultimate1 [3,1,2,3,4,5,6,1,1,1]
--True

--8
sequenceones :: [Int] -> Bool
sequenceones (1:1:_) = True -- if two ones after each other then true
sequenceones (x:xs) = sequenceones xs -- otherwise apply the function to the tail of the list
sequenceones _ = False -- if empty list then false
--sequenceones [1,2,0,0,1,1]
--True
--sequenceones [0,1,2,3,4,7,8]
--False
--sequenceones [1,1,0,1,23,4]
--True


