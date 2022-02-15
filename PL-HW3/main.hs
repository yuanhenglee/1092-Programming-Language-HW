--1.
segments :: [a] -> [[a]]
segments [] = []
segments s@(x:xs) = [ x | x <- inits s ++ segments xs , not $null x ]

inits :: [Int] -> [[Int]]
inits [] = [[]]
inits xs = inits (init xs) ++ [xs]

--2.
insert :: ((Int,Int)->(Int,Int)->Bool) -> (Int,Int) -> [(Int,Int)] -> [(Int,Int)] 
insert _ x [] = [x]
insert f x (y:ys) | f x y  = x:y:ys
                  | otherwise = y:(insert f x ys)

isort2 :: ((Int,Int)->(Int,Int)->Bool)->[(Int,Int)]->[(Int,Int)] 
isort2 f [] = []
isort2 f (x:xs) =  insert f x (isort2 f xs)

comparePair :: (Int,Int)->(Int,Int)->Bool
comparePair (a,b) (c,d) = a < c || a == c && b < d

--3.a
maxl :: (Ord a) => [a] -> a
maxl (x:xs) = foldr max x xs

--3.b
member :: (Eq a) => [a] -> a -> Bool
member xs s = any (s == ) xs

--3.c
remdup :: [Char]->[Char]
remdup [] = []
remdup (x:xs) = x : remdup [a | a<-xs , a /= x]

delete :: Char -> [Char] -> [Char]
delete y xs = [ x | x<-xs , not (x == y)]

elemOcc :: Char-> [Char]->Int
elemOcc x = length . (filter (x==))

occurrences :: [Char] -> [(Char,Int)]
--occurrences xs = zip (remdup xs) [ elemOcc x xs | x <- xs ]
occurrences xs = zip setXS (map (\x->elemOcc x xs) setXS )
    where setXS = remdup xs

--4.a
myMap :: (a->b)->[a]->[b]
myMap f = foldr (\x xs -> f x : xs ) []                

--4.b
{-
pipeline=map.foldr(.)id
pipeline function 可以接受一組list of functions。
所有 functions 和 id 做(.)操作，會fold成類似
f1( f2( f3( ... fn(id())))) 形式
接著此複合function會再透過map apply到目標上
-}




main = 
    pretest "segments" $ segments [1,2,3,4,5]    
    pretest "segments" $ segments []::[Integer]
    pretest "isort2 comparePair [(5,4), (3,2), (1,7),(1,5), (2,6)]" $ isort2 comparePair [(5,4), (3,2), (1,7),(1,5), (2,6)]
    pretest "maxl [1,2,9,4,5,6]" $ maxl [1,2,9,4,5,6]
    pretest "member [1,2,3,4.0,5] 4" $ member [1,2,3,4,5] 4
    pretest "remdup ['a','b','a','b','c']" $ remdup ['a','b','a','b','c'] 
    pretest "delete 'a' ['a','b','c']" $ delete 'a' ['a','b','c']
    pretest "elemOcc 'a' ['a', 'c', 'd', 'a', 'c']" $ elemOcc 'a' ['a', 'c', 'd', 'a', 'c'] 
    pretest "occurrences ['a', 'c', 'd', 'a', 'c']" $ occurrences ['a', 'c', 'd', 'a', 'c']
    pretest "myMap even [1,2,3,4,5]" $ myMap even [1,2,3,4,5]

    where
    pretest p a = putStrLn $ p ++ " = " ++ show a

