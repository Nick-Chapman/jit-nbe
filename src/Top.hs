
module Top where

import Data.Map (Map)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  putStrLn "*jit-nbe*"
  let prog = App (Lam x (Sum (Num 42) (Var x))) (Num 5) where x = Id"x"
  print ("prog:",prog)
  let p0 = Map.empty
  let value = eval p0 prog
  print ("res:",value)
  pure ()

newtype Id = Id { unId :: String } deriving (Eq,Ord,Show)

data Exp
  = Num Int
  | Sum Exp Exp
  | Var Id
  | Lam Id Exp
  | App Exp Exp
  | Let Id Exp Exp
  -- | Fix ...
  deriving Show

type Env = Map Id Value

eval :: Env -> Exp -> Value
eval p = \case
  Num i -> vnum i
  Sum e1 e2 -> vsum (eval p e1) (eval p e2)
  Var x -> maybe (error $ "eval,lookup:"++show x) id (Map.lookup x p)
  Lam x e -> vclose p x e
  App e1 e2 -> vapp (eval p e1) (eval p e2)
  Let i e1 e2 -> eval p (App (Lam i e2) e1)

vnum :: Int -> Value
vnum = VNum

vclose :: Env -> Id -> Exp -> Value
vclose = VClose

vsum :: Value -> Value -> Value
vsum = \case
  VClose{} -> error "vsum,arg1,VClose"
  VNum i1 -> \case
    VClose{} -> error "vsum,arg2,VClose"
    VNum i2 -> VNum (i1+i2)

vapp :: Value -> Value -> Value
vapp = \case
  VNum{} -> error "vapp,arg1,VNum"
  VClose p x e -> \v -> eval (Map.insert x v p) e

data Value = VNum Int | VClose Env Id Exp
  deriving Show
