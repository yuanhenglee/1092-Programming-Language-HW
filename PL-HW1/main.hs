
-- example
power :: Int -> Int -> Int
power n 0 = 1
power n k = n * power n (k-1)

-- 1. (a)
power1 :: Int -> Int -> Int
power1 n k | k < 0 = error "power: negative argument"
power1 n 0 = 1
power1 n k = product (take k (repeat n))

-- 1. (b)
power2 :: Int -> Int -> Int
power2 n k | k < 0 = error "power: negative argument"
power2 n 0 = 1
power2 n k | (even k) = (power2 (n*n) (div k 2))
           | otherwise = (n*power2 n (k - 1))

-- 2. (a)
tailUpto :: Int -> Int -> [Int] -> [Int]
tailUpto a b list | a > b = [] ++ list
tailUpto a b list = (tailUpto a (b - 1) ([b] ++ list))

-- 2. (b)
tailFib :: Int -> Int -> Int -> Int
tailFib 0 a b = a
tailFib n a b = tailFib (n - 1) b (a + b)


-- 3. (a)
palindrome :: [Int] -> Bool
palindrome text = text == reverse text

-- 3. (b)
isPermutation :: [Int] -> [Int] -> Bool
isPermutation list_a list_b = (removeElements list_a list_b) == []

removeElements :: [Int] -> [Int] -> [Int]
removeElements list_a list_b | list_b == [] = list_a
                             | otherwise = removeElements (removeOnce (head list_b) list_a) (tail list_b)
 
-- helper function for 4. (b)
removeOnce :: Int -> [Int] -> [Int]
removeOnce a list | list == [] = []
                  | a == head list = tail list
                  | otherwise = (head list) : removeOnce a (tail list)


main = do
    pretest "power 7 5" $ removeOnce 3 [1,3,5,3,4]--power 7 5
    pretest "power 3 7" $ power 3 7
    pretest "power1 7 5" $ power1 7 5
    pretest "power2 3 7" $ power2 3 7
    pretest "tailUpto 3 8 [1,2]" $ tailUpto 3 8 [1,2]
    pretest "tailUpto 8 3 [1]" $ tailUpto 8 3 [1]
    pretest "tailFib 5 0 1" $ tailFib 5 0 1
    pretest "palindrome [1, 2, 2, 3, 3]" $ palindrome [1, 2, 2, 3, 3]
    pretest "palindrome [1, 2, 3, 2, 1]" $ palindrome [1, 2, 3, 2, 1]
    pretest "palindrome [3]" $ palindrome [3]
    pretest "palindrome []" $ palindrome []
    pretest "isPermutation [] []" $ isPermutation [] []
    pretest "isPermutation [1,2,1] [2,1,1]" $ isPermutation [1,2,1] [2,1,1]
    pretest "isPermutation [1,2,1] [2,1,2]" $ isPermutation [1,2,1] [2,1,2]
    pretest "isPermutation [1,2,1] [2,1,1,2]" $ isPermutation [1,2,1] [2,1,1,2]
    pretest "removeOnce 3 [1,3,5,3,4]" $ removeOnce 3 [1,3,5,3,4]
    pretest "removeOnce 5 [1,2,3,3,4]" $ removeOnce 5 [1,2,3,3,4]
    pretest "removeOnce 3 []" $  removeOnce 3 []
    where
    pretest p a = putStrLn $ p ++ " = " ++ show a
    