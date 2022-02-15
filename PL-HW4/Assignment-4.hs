--1.
type Name = String
data Expr
  = Var Name
  | Num Int --constant
  | Expr :+: Expr
  | Expr :*: Expr
  | Let [String] Expr Expr
  deriving (Eq, Show)

global = [("x", 1), ("y", 2)] :: [(Name, Int)]

type Env a = [(Name, a)]

data Error a
  = S a 
  | Error String
  deriving (Eq,Show)

updEnv :: String -> Int -> Env Int -> Env Int
updEnv name val env = [(x, y) | (x, y) <- env, x /= name] ++ [(name, val)]

eval :: Env Int -> Expr -> Error Int
eval env (Var x) =
  case (lookup x env) of
    Just x' -> S x'
    Nothing -> Error ("unbound variable: " ++ x)
eval env (Num int) = S int
eval env (x :*: y) =
  case (first, second) of
    (S f, S s) -> S (f * s)
    (Error str, _) -> Error str
    (_, Error str) -> Error str
  where
    first = eval env x
    second = eval env y
eval env (x :+: y) =
  case (first, second) of
    (S f, S s) -> S (f + s)
    (Error str, _) -> Error str
    (_, Error str) -> Error str
  where
    first = eval env x
    second = eval env y
eval env (Let [x] exp1 exp2) =
  case eval env exp1 of
    S res1 -> 
      case eval env2 exp2 of
        S res2 -> S res2
        Error str -> Error str
        where env2 = updEnv x res1 env 
    Error str -> Error str

--2.
data Tree a
  = Empty
  | Node a (Tree a) (Tree a)
  deriving (Show)

bfs :: Tree a -> [a]
bfs x = traverse' [x]

traverse' :: [Tree a] -> [a]
traverse' [] = []
traverse' ts = rootlabels ++ traverse' children
  where
    rootlabels = [x | Node x _ _ <- ts]
    children = concat [[left, right] | Node _ left right <- ts]

--3
data Proposition
  = Pvar String
  | F
  | T
  | Not Proposition
  | Proposition :|: Proposition --or
  | Proposition :&: Proposition
  deriving (Eq, Ord, Show)

--3.a
isNorm :: Proposition -> Bool
isNorm (p1 :|: p2) = isNorm p1 && isNorm p2
isNorm (p1 :&: p2) = isNorm p1 && isNorm p2
isNorm (Not prop) =
  case (prop) of
    Pvar _ -> True
    p -> p == T || p == F
isNorm prop = True

--3.b
norm :: Proposition -> Proposition
norm (p1 :|: p2) = norm p1 :|: norm p2
norm (p1 :&: p2) = norm p1 :&: norm p2
norm (Not prop) =
  case (prop) of
    Not insideP -> norm insideP
    p1 :|: p2 -> norm ( (Not p1) :&: (Not p2) )
    p1 :&: p2 -> norm ( (Not p1) :|: (Not p2) )
    p -> if p == T then F else if p == F then T else Not p
norm prop = prop

--4.a
data Edit
  = Change Char
  | Copy
  | Delete
  | Insert Char
  deriving (Eq, Show)


cost :: [Edit] -> Int
cost = length . filter (/=Copy)

transform :: String -> String -> [Edit]
transform [] [] = []
transform st [] = replicate (length st) Delete
transform [] st = map Insert st
transform (a:x) (b:y)
    | a==b = Copy : transform x y
    | otherwise 
      = best [ 
        Delete   : transform x (b:y),
        Insert b : transform (a:x) y,
        Change b : transform x y 
      ]
best :: [[Edit]] -> [Edit]
best [x] = x
best (x:xs)
  | cost x <= cost b = x
  | otherwise = b
    where
        b = best xs



--4.b
--see minDistance.cpp

test ex ur = if (ex) == (ur) then "Correct"
                     else "expect: " ++ show ex ++
                       ", your: " ++ show ur

tt = Node 1 (Node 10 Empty (Node 16 Empty Empty))
           (Node 17 (Node 14 Empty Empty)
                    (Node 20 Empty Empty))
main = do
  print "1."
  print . test (S 1) $ eval global (Var "x")
  print . test (S 13) $ eval global (Num 5 :+: Num 8)
  print . test (S 3) $ eval global (Num 1 :+: Var "y")
  print . test (S 15) $ eval global (Num 1 :+: Var "y" :*: Num 5)
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