--1.(a)
myButLast :: [a] -> a
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

--1.(b)
rev2 :: [a] -> [a]
rev2 s@(_:xs) | null (tail xs) = reverse s
              | otherwise = s

--2.(a)
countNeg :: [Int] -> Int
countNeg list = sum [1 | x <- list , x < 0 ]
                
--2.(b)
raise :: Int -> Int -> Int 
raise ba ex = product[ ba | _ <- [1..ex]] 

--2.(c)
pascal :: Int -> [Int]
pascal 1 = [1]
pascal n = [1] ++ [ fir + sec | (fir,sec) <- pairs ( pascal (n - 1) ) ] ++  [1]

pairs :: [Int] -> [(Int,Int)]
pairs [] = []
pairs [_] = []
pairs (x:xs) = [(x , head xs)] ++ pairs xs 


--3.(a)
q1f1a :: [Int] -> [Int]
q1f1a xs = map m1 (filter f1 xs )
    where m1 x = 3 * x
          f1 x = not (x < 3 || x > 10)
      
--3.(b)
q1f1b :: [Int] -> [Int]
q1f1b xs = [ x * 3 | x <- xs , not (x < 3 || x > 10)]

--3.(c)
compre :: [a]->(a->b)->(a->Bool)->[b]
compre xs f p = map f (filter p xs)

--4.
subsets :: [Int] -> [[Int]]
subsets [] = [[]]
subsets (x:xs) = prevSubset ++ [ x:sub | sub <- prevSubset ]
  where prevSubset = subsets xs


main = do
    pretest "myButLast [1, 2, 3, 4]" $ myButLast [1, 2, 3, 4]
    pretest "myButLast ['a'..'z']" $ myButLast ['a'..'z']
    pretest "rev2 [1, 2]" $ rev2 [1, 2]
    pretest "rev2 [1, 2, 3]" $ rev2 [1, 2, 3]
    pretest "countNeg [1, -1, 3, -5]" $ countNeg [1, -1, 3, -5]
    pretest "raise 2 4" $ raise 2 4
    pretest "pairs [1, 2, 3, 4, 5]" $ pairs [1, 2, 3, 4, 5]
    pretest "pascal 5" $ pascal 5
    pretest "q1f1a [1, 5, 15, -4, 7]" $ q1f1a [1, 5, 15, -4, 7]
    pretest "q1f1b [1, 5, 15, -4, 7]" $ q1f1b [1, 5, 15, -4, 7]
    pretest "compre [(-9)..7] (*3) odd" $ compre [(-9)..7] (*3) odd
    pretest "subsets [1, 2, 3]" $ subsets [1, 2, 3]
    where
    pretest p a = putStrLn $ p ++ " = " ++ show a
