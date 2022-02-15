-- 5.
type Name = String

data Exp2
  = Var Name
  | Num Int
  | B Bool --constant
  | Exp2 :+: Exp2
  | Exp2 :*: Exp2
  | Exp2 :=: Exp2
  | If Exp2 Exp2 Exp2
  | Let [String] Exp2 Exp2
  deriving (Eq, Show)

type Env a = [(Name, a)]

data Val
  = VN Int
  | VB Bool
  deriving (Eq, Show)

data Error a
  = S a
  | Error String
  deriving (Eq, Show)

updEnv :: String -> Val -> Env Val -> Env Val
updEnv name val env = [(x, y) | (x, y) <- env, x /= name] ++ [(name, val)]

eval2 :: Env Val -> Exp2 -> Error Val
eval2 env (Var name) =
  case lookup name env of
    Just x -> S x
    Nothing -> Error ("unbound variable: " ++ name)
eval2 env (Num int) = S . VN $int
eval2 env (B bool) = S . VB $bool
eval2 env (exp0 :+: exp1) =
  case (first, second) of
    (S (VN f), S (VN s)) -> S . VN $ (f + s)
    -- ( S (VB f) , S (VB s) ) -> S . VB $ (f || s)
    (Error str, _) -> Error str
    (_, Error str) -> Error str
  where
    first = eval2 env exp0
    second = eval2 env exp1
eval2 env (exp0 :*: exp1) =
  case (first, second) of
    (S (VN f), S (VN s)) -> S . VN $ (f * s)
    (Error str, _) -> Error str
    (_, Error str) -> Error str
  where
    first = eval2 env exp0
    second = eval2 env exp1
eval2 env (exp0 :=: exp1) =
  case (first, second) of
    (S (VN f), S (VN s)) -> S . VB $ (f == s)
    (Error str, _) -> Error str
    (_, Error str) -> Error str
  where
    first = eval2 env exp0
    second = eval2 env exp1
eval2 env (If exp0 exp1 exp2) =
  case (first, second, third) of
    (S (VB f), S sVal, S tVal) -> if f then S sVal else S tVal
    (Error str, _, _) -> Error str
    (_, Error str, _) -> Error str
    (_, _, Error str) -> Error str
  where
    first = eval2 env exp0
    second = eval2 env exp1
    third = eval2 env exp2

eval2 env (Let [string] exp1 exp2) = 
  case eval2 env exp1 of
    S res1 -> 
      case eval2 env2 exp2 of
        S res2 -> S res2
        Error str -> Error str
        where env2 = updEnv string res1 env 
    Error str -> Error str


test ex ur = if (show ex) == (show ur) then "Correct"
                     else "expect: " ++ show ex ++
                       ", your: " ++ show ur
global = [("x", VN 1), ("y", VN 2)] :: [(Name, Val)]

main = do
  print "1."
  print . test (S (VN 1 )) $ eval2 global (Var "x")
  print . test (S (VN 13))  $ eval2 global (Num 5 :+: Num 8)
  print . test (S (VN 3 )) $ eval2 global (Num 1 :+: Var "y")
  print . test (S (VN 15))  $ eval2 global (Num 1 :+: Var "y" :*: Num 5)
  print . test (S (VN 5))  $ eval2 global (If ((Num 1):=:(Num 2)) (Num 3) (Num 5) )
  print . test (S (VN 0))  $ eval2 global (Let ["yy"] (Num 1) (If ((Var "yy"):=:(Num 2)) (Num 3) (Num 5) ) )