
module Top (main) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map

import Control.Monad (ap,liftM,(>=>))

import qualified Bf

main :: IO ()
main = do
  Bf.main


_main :: IO ()
_main = do
  putStrLn "*jit-nbe*"
  let p0 = Map.empty

  let prog0 = with3 (\n3 -> App id (Lam x (Sum (App (App id id) (Sum n3 (Num 42))) (App dub (App inc (Var x))))))
        where
          with3 f = App (Lam y (f (Var y))) (Num 3) where y = mkId"y"
          x = mkId"x"
          id = Lam x (Var x)
          dub = Lam x (Sum (Var x) (Var x))
          inc = Lam x (Sum (Var x) (Num 1))

  print ("prog0:",prog0)
  value0 <- evalIO p0 (App prog0 (Num 5))
  print ("res0:",value0)

  let norm = normalize prog0
  print ("norm:",norm)

  let prog1 = deNorm norm

  value1 <- evalIO p0 (App prog1 (Num 5))
  print ("res1:",value1)
  pure ()

{-mkId :: String -> Id
newtype Id = Id { unId :: String } deriving (Eq,Ord,Show)
mkId = Id-}

type Id = String
mkId :: String -> Id
mkId s = s
unId :: Id -> String
unId s = s

data Exp
  = Num Int
  | Sum Exp Exp
  | Var Id
  | Lam Id Exp
  | App Exp Exp
  -- | Fix ...
  deriving Show

eLet :: Id -> Exp -> Exp -> Exp
eLet i e1 e2 = App (Lam i e2) e1

type Env = Map Id Value

{-eval :: Env -> Exp -> Value
eval p = \case
  Num i -> vnum i
  Sum e1 e2 -> vsum (eval p e1) (eval p e2)
  Var x -> maybe (error $ "eval,lookup:"++show x) id (Map.lookup x p)
  Lam x e -> vclose p x e
  App e1 e2 -> vapp (eval p e1) (eval p e2)-}

evalIO :: Env -> Exp -> IO Value
evalIO p = \case
  Num i -> pure $ vnum i
  Sum e1 e2 -> do
    v1 <- evalIO p e1
    v2 <- evalIO p e2
    let v = vsum v1 v2
    putStrLn "sum" -- print ("sum",v1,v2,"-->",v)
    pure v
  Var x -> pure $ maybe (error $ "eval,lookup:"++show x) id (Map.lookup x p)
  Lam x e -> pure $ vclose p x e
  App e1 e2 -> do
    v1 <- evalIO p e1
    v2 <- evalIO p e2
    --putStrLn "app..." -- ("app",v1,v2,"...")
    v <- vappIO v1 v2
    --putStrLn "app..done" -- print ("app",v1,v2,"-->",v)
    pure v

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

vappIO :: Value -> Value -> IO Value
vappIO = \case
  VNum{} -> error "vapp,arg1,VNum"
  VClose p x e -> \v -> evalIO (Map.insert x v p) e

data Value = VNum Int | VClose Env Id Exp
  deriving Show

----------------------------------------------------------------------
-- nbe

type SemEnv = Map Id SemVal

normalize :: Exp -> Norm
normalize = runM . norm

norm :: Exp -> M Norm
norm = reflect Map.empty >=> reify

data Norm
  = NNum Int
  | NSum Norm Norm
  | NVar Id
  | NLam Id Norm
  | NApp Norm Norm
  | NLet Id Norm Norm
  deriving Show

deNorm :: Norm -> Exp
deNorm = \case
  NNum i -> Num i
  NSum n1 n2 -> Sum (deNorm n1) (deNorm n2)
  NVar x -> Var x
  NLam x n -> Lam x (deNorm n)
  NApp n1 n2 -> App (deNorm n1) (deNorm n2)
  NLet x n1 n2 -> eLet x (deNorm n1) (deNorm n2)

reflect :: SemEnv -> Exp -> M SemVal
reflect q = \case
  Num i -> do
    pure $ Number i
  Sum e1 e2 -> do
    sv1 <- reflect q e1
    sv2 <- reflect q e2
    svSum sv1 sv2
  Var x -> do
    pure $ maybe (error $ "reflect,lookup:"++show x) id (Map.lookup x q)
  Lam x body -> do
    pure $ Macro (unId x) $ \arg -> do
      reflect (Map.insert x arg q) body
  App e1 e2 -> do
    sv1 <- reflect q e1
    sv2 <- reflect q e2
    svApply sv1 sv2

data SemVal
  = Syntax Norm
  | Name Id
  | Number Int
  | Macro String (SemVal -> M SemVal)

reify :: SemVal -> M Norm
reify = \case
  Syntax n -> pure n
  Name x -> pure $ NVar x
  Number i -> pure $ NNum i
  Macro tag f -> do
    x <- Fresh tag
    body <- Reset (f (Name x) >>= reify)
    return $ NLam x body

svApply :: SemVal -> SemVal -> M SemVal
svApply = \case
  Number{} -> error "svApply,arg1,Number"
  Name xf -> \arg -> do
    arg <- reify arg
    return $ Syntax (NApp (NVar xf) arg)
  Syntax func -> \arg -> do
    arg <- reify arg
    return $ Syntax (NApp func arg)
  Macro tag f -> \arg -> do
    if duplicatable arg then f arg else do -- OPTIMIZATION HERE: beta/inlining!
      x <- Fresh tag
      arg <- reify arg
      Wrap (NLet x arg) (f (Name x))

duplicatable :: SemVal -> Bool
duplicatable = \case
  Number{} -> True
  Name{} -> True
  Macro{} -> True
  Syntax{} -> False

svSum :: SemVal -> SemVal -> M SemVal
svSum sv1 sv2 =
  case (maybeConstNum sv1, maybeConstNum sv2) of
    (Just i1,Just i2) ->
      pure $ Number (i1+i2) -- OPTIMIZATION HERE: constant folding!
    _ ->
      pure $ Syntax (NSum (reifyNum sv1) (reifyNum sv2))

maybeConstNum :: SemVal -> Maybe Int
maybeConstNum = \case
  Number i -> Just i
  _ -> Nothing

reifyNum :: SemVal -> Norm
reifyNum = \case
  Macro{} -> error "maybeConstNum"
  Number i -> NNum i
  Name x -> NVar x
  Syntax n -> n

data M a where -- monad for normalizion: fresh-idents & continuation manipulation
  Ret :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  Fresh :: String -> M Id
  Wrap :: (Norm -> Norm) -> M a -> M a
  Reset :: M Norm -> M Norm

instance Functor M where fmap = liftM
instance Applicative M where pure = return; (<*>) = ap
instance Monad M where return = Ret; (>>=) = Bind

runM :: M Norm -> Norm
runM m = snd $ loop 1 m k0 where
  k0 s e = (s,e)
  loop :: State -> M a -> (State -> a -> Res) -> Res
  loop state m k = case m of
    Ret x -> k state x
    Bind m f -> loop state m $ \state a -> loop state (f a) k
    Fresh tag -> k (state+1) $ mkId (tag ++ show state)
    Wrap f m -> f' (loop state m k) where f' (s,e) = (s,f e)
    Reset m -> let (state',v) = loop state m k0 in k state' v

type Res = (State,Norm)
type State = Int
