-- 1.
type Name = String
data Expr = Var Name | Num Int  --constant
          | Expr :+: Expr
          | Expr :*: Expr
          | Let [String] Expr Expr
          deriving (Eq, Show)

global = [("x", 1), ("y", 2)] :: [(Name, Int)]

eval env (Var name) = undefined
eval env (Num int)  = undefined
eval env (exp0 :+: exp1) = undefined
eval env (exp0 :*: exp1) = undefined
eval env (Let [string] exp0 exp1) = undefined

-- 2.
data Tree a = Empty
            | Node a (Tree a) (Tree a) deriving (Show)

bfs :: Tree a -> [a]
bfs x = traverse' [x]
traverse' :: [Tree a] -> [a]
traverse' [] = []
-- traverse' ts = rootlabels ++ traverse' children
--   where  rootlabels = [x | Node x ...] -- <- stuff ...
--          children = [... | ...] -- <- stuff ...

-- both need list comprehension & pattern matching
--
-- 3.
data Proposition = Pvar String
                 | F
                 | T
                 | Not Proposition
                 | Proposition :|: Proposition
                 | Proposition :&: Proposition
                 deriving (Eq, Ord, Show)

isNorm :: Proposition -> Bool
isNorm expr = undefined

norm :: Proposition -> Proposition
norm expr = undefined

-- 4.
data Edit = Change Char
          | Copy
          | Delete
          | Insert Char
          deriving (Eq, Show)

transform:: String -> String -> [Edit]

transform [] [] = []
transform st [] = replicate (length st) Delete
transform [] st = map Insert st
-- transform (a:x) (b:y)
--   | a == b = Copy : transform x y
--   | otherwise =
--     best [Delete : ???, -- stuff ???
--           Insert b : ???,
--           Change b : ???]

best :: [[Edit]] -> [Edit]
best [x] = x
best (x:xs)
  | cost x <= cost b = x
  | otherwise = b where
    b = best xs

cost :: [Edit] -> Int
cost = length . filter (/= Copy)

test ex ur = if ex == ur then "Correct"
                     else "expect: " ++ show ex ++
                       ", your: " ++ show ur

tt = Node 1 (Node 10 Empty (Node 16 Empty Empty))
           (Node 17 (Node 14 Empty Empty)
                    (Node 20 Empty Empty))

main = do
  print "1."
  print . test 1 $ eval global (Var "x")
  print . test 13 $ eval global (Num 5 :+: Num 8)
  print . test 3 $ eval global (Num 1 :+: Var "y")
  print . test 15 $ eval global (Num 1 :+: Var "y" :*: Num 5)
  print "2."
  print . test [1, 10, 17, 16, 14, 20] $ bfs tt
  print "3."
  print . test True $ isNorm (Pvar "p" :&: Not (Pvar "q"))
  print . test False $ isNorm (Not (Pvar "p" :|: Pvar "q"))
  print . test False $ isNorm (Not (Not (Pvar "p")) :|: Not T)
  print . test False $ isNorm (Not (Pvar "p" :&: Not (Pvar "q")))
  print . test (Pvar "p" :&: Not (Pvar "q")) $
    norm (Pvar "p" :&: Not (Pvar "q"))
  print . test (Not (Pvar "p") :&: Not (Pvar "q")) $
    norm (Not (Pvar "p" :|: Pvar "q"))
  print . test (Pvar "p" :|: F) $
    norm (Not (Not (Pvar "p")) :|: Not T)
  print . test (Not (Pvar "p") :|: Pvar "q") $
    norm (Not (Pvar "p" :&: Not (Pvar "q")))
  print "4."
  print . test [Copy, Copy, Copy, Insert 'l', Change 'o'] $
    transform "help" "hello"
  print . test [Copy, Copy, Copy, Delete, Change 'p'] $
    transform "hello" "help"
  print . test [Change 'b', Copy, Copy, Delete, Delete] $
    transform "abcde" "bbc"
  print . test [Insert 'c', Change 'h', Copy, Insert 'p', Copy, Delete] $ transform "fish" "chips"
  print . test [Delete, Change '4', Copy, Insert '2', Change '1'] $ transform "1234" "4321"
  print . test [Delete, Change '6', Change '5', Copy, Insert '3', Change '2', Change '1'] $ transform "123456" "654321"
  print . test [Delete, Change '8', Change '7', Change '6', Copy, Insert '4', Change '3', Change '2', Change '1'] $ transform "12345678" "87654321"