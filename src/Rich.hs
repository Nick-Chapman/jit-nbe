
-- Think about evaluation (and later normalization) of a richer expression language
-- including: multi-app/lam, data-constructors + case-matching, mutual-fixpoints, and IO

module Rich (main,eval,Exp(..),eFix,eLet,eTrue,eFalse,eIf,eNil,eCons,eCaseList) where

import Data.Map (Map)
import Data.Word8 (Word8)
import System.IO (isEOF)
import qualified Data.Ascii as Ascii (fromChar,toChar)
import qualified Data.Map.Strict as Map

import Control.Monad (ap,liftM,(>=>))

main :: IO ()
main = do
  putStrLn "*rich*"
  let p0 = Map.empty

  let prog = Lam inc (with3 (\n3 -> App id (Lam x (Add (App (App id id) (Add n3 (Lit 42))) (App dub (App (Var inc) (Var x)))))))
        where
          --with3 f = App (Lam y (f (Var y))) (Lit 3) where y = mkName"y"
          with3 f = eLet y (Lit 3) (f (Var y)) where y = mkName"y"
          inc = mkName"inc"
          x = mkName"x"
          id = Lam x (Var x)
          dub = Lam x (Add (Var x) (Var x))
          --inc = Lam x (Add (Var x) (Lit 1))

  let _prog = Lam x (Con (Tag 7) [Add (Lit 12) (Lit 100), Var x, Add (Var x) (Lit 34)])
        where
          x = mkName"x"

  -- TODO: drive evaluation/normalization of remaining expression forms with tests

  let wrapContext e = App (App e inc) (Lit 5)
        where inc = Lam x (Add (Var x) (Lit 1))
              x = mkName"x"

  putStrLn ("prog:\n"++ show prog)
  value0 <- eval p0 (wrapContext prog)
  print ("res0:",value0)

  let norm = normalize prog
  putStrLn ("norm:\n"++ show norm)

  value1 <- eval p0 (wrapContext norm)
  print ("res1:",value1)

  pure ()


------------------------------------------------------------------------
-- derived constructors

eFix :: Name -> Abstraction -> Exp
eFix f abs = Letrec [(f,abs)] (Var f)

eLet :: Name -> Exp -> Exp -> Exp
eLet x rhs body = Case (Con (Tag 0) [rhs]) [Abstraction [x] body]

eTrue :: Exp
eTrue = Con (Tag 0) []

eFalse :: Exp
eFalse = Con (Tag 1) []

eIf :: Exp -> (Exp,Exp) -> Exp
eIf cond (thenBranch,elseBranch) =
  Case cond [Abstraction [] thenBranch, Abstraction [] elseBranch]

eNil :: Exp
eNil = Con (Tag 0) []

eCons :: Exp -> Exp -> Exp
eCons x xs = Con (Tag 1) [x,xs]

eCaseList :: Exp -> (Exp, (Name,Name,Exp)) -> Exp
eCaseList scrut (nilBranch,(x,xs,consBranch)) =
  Case scrut [Abstraction [] nilBranch, Abstraction [x,xs] consBranch]


----------------------------------------------------------------------
-- Exp, abstract syntax

mkName :: String -> Name
newtype Name = Name { unName :: String } deriving (Eq,Ord)
mkName = Name

newtype Tag = Tag Int

data Exp
  = Var Name
  | Lit Byte
  | Add Exp Exp
  | Lam Name Exp
  | App Exp Exp
  | Con Tag [Exp]
  | Case Exp [Abstraction]
  | Letrec [(Name,Abstraction)] Exp -- TODO: this abstraction always has 1 name ?!
  | Input Name Exp
  | Output Exp Exp

newtype Byte = Byte Word8
  deriving (Num)

data Abstraction = Abstraction [Name] Exp -- TODO: rename Alternative, and use only in Case
  deriving Show

----------------------------------------------------------------------
-- pretty

instance Show Exp where show = unlines . indent . pretty

type Lines = [String]

pretty :: Exp -> Lines
pretty = \case
  Lit byte -> [show byte]
  Add e1 e2 -> paren (beside " + " (pretty e1) (pretty e2))
  Var x -> [show x]
  App func arg -> paren (foldl jux [] (map pretty [func,arg]))
  Lam x body -> paren (hangIndented ("\\" ++ show x ++ ".") (pretty body))
  Con tag es -> do
    bracket (show tag++"{") "}" (seperated "," [ pretty e | e <- es ])
  Case scrut branches -> do
    bracket "case " " of" (pretty scrut)
    ++ indent (concat [ prettyAbstraction abs | abs  <- branches ])
  Letrec bindings body -> do
    undefined bindings body
  Input x body -> do
    undefined x body
  Output e1 e2 -> do
    undefined e1 e2

prettyAbstraction :: Abstraction -> Lines
prettyAbstraction (Abstraction xs body) =
  hangIndented ("\\" ++ show xs ++ " -> ") (pretty body)

instance Show Tag where show (Tag n) = "tag-" ++ show n
instance Show Name where show (Name s) = s
instance Show Byte where show (Byte w) = show w

paren :: Lines -> Lines
paren = bracket "(" ")"

bracket :: String -> String -> Lines -> Lines
bracket left right = onHead (left ++) . onTail (++ right)

seperated :: String -> [Lines] -> Lines
seperated sep = \case
  [] -> []
  [x] -> x
  x:xs -> beside sep x (seperated sep xs)

onHead,onTail :: (String -> String) -> Lines -> Lines
onHead _ [] = error "onHead"
onHead f (x:xs) = f x : xs
onTail f = reverse . onHead f . reverse

jux :: Lines -> Lines -> Lines
jux [x] [y] = [ x ++ " " ++ y ]
jux xs ys = xs ++ ys

beside :: String -> Lines -> Lines -> Lines
beside sep [x] [y] = [ x ++ sep ++ y ]
beside sep xs ys = onTail (++ sep) xs ++ ys

hangIndented :: String -> Lines -> Lines
hangIndented hang = \case
  [] -> error "indented"
  [oneLine] -> [hang ++ " " ++ oneLine]
  lines -> [hang] ++ indent lines

indent :: Lines -> Lines
indent lines = ["  " ++ line | line <- lines]


----------------------------------------------------------------------
-- evaluation

type Env = Map Name Value

eval :: Env -> Exp -> IO Value
eval p = \case
  Lit i -> pure (VByte i)
  Add e1 e2 -> do
    v1 <- eval p e1
    v2 <- eval p e2
    pure $ vAdd v1 v2
  Var x -> do
    pure $ maybe (error $ "eval,lookup:"++show x) id (Map.lookup x p)
  Lam x body -> do
    pure $ VClosure p (Abstraction [x] body)
  App e1 e2 -> do
    v2 <- eval p e2
    v1 <- eval p e1
    enterClosure v1 $ \p (Abstraction xs e) ->
      eval (extend xs [v2] p) e
  Con tag es -> do
    vs <- mapM (eval p) es
    pure (VData tag vs)
  Case e1 branches -> do
    v1 <- eval p e1
    unpackData v1 $ \(Tag n) vs -> do
      let Abstraction xs e2 = branches !! n
      eval (extend xs vs p) e2
  Letrec bindings body -> do -- TODO: not tested yet
    let names = [ x | (x,_) <- bindings ]
    let vs = [ VClosure p' abs | (_,abs) <- bindings ] -- cyclic values/env
        p' = extend names vs p
    eval p' body
  Input x body -> do -- TODO: not tested yet
    byte <- inputByte
    eval (extend [x] [VByte byte] p) body
  Output e1 e2 -> do -- TODO: not tested yet
    v1 <- eval p e1
    outputByte (getByte "output" v1)
    eval p e2


----------------------------------------------------------------------
-- values

vAdd :: Value -> Value -> Value
vAdd v1 v2 = VByte (getByte "add/1" v1 + getByte "add/2" v2)

getByte :: String -> Value -> Byte
getByte tag = \case VByte i -> i; v -> error (show (tag,v))

enterClosure :: Value -> (Env -> Abstraction -> r) -> r
enterClosure v k = case v of VClosure p abs -> k p abs; _ -> error (show ("enterClosure",v))

unpackData :: Value -> (Tag -> [Value] -> r) -> r
unpackData v k = case v of VData tag vs -> k tag vs; _ -> error (show ("unpackData",v))

data Value
  = VByte Byte
  | VClosure Env Abstraction -- TODO: should this be an Abstraction here
  | VData Tag [Value]
  deriving Show

----------------------------------------------------------------------
-- input/output

inputByte :: IO Byte
inputByte = do
  isEOF >>= \case
    True -> pure 0
    False -> do
      char <- getChar
      case Ascii.fromChar char of
        Just w8 -> pure (Byte w8)
        Nothing -> error ("inputByte, non ascii char: " ++ show char)

outputByte :: Byte -> IO ()
outputByte (Byte w8) = putChar (Ascii.toChar w8)

----------------------------------------------------------------------
-- normalization

normalize :: Exp -> Exp
normalize = runM . norm Map.empty

norm :: SemEnv -> Exp -> M Exp
norm p = reflect p >=> reify


type SemEnv = Map Name SemVal

reflect :: SemEnv -> Exp -> M SemVal
reflect q = \case
  Lit b -> do
    pure (Constant b)
  Add e1 e2 -> do
    sv1 <- reflect q e1
    sv2 <- reflect q e2
    pure $ svAdd sv1 sv2
  Var x -> do
    pure $ maybe (error $ "reflect,lookup:"++show x) id (Map.lookup x q)
  Lam x body -> do
    pure $ Macro (unName x) $ \arg -> do
      reflect (extend [x] [arg] q) body
  App eFunc eArg -> do
    svArg <- reflect q eArg
    svFunc <- reflect q eFunc
    svApply svFunc svArg
  Con tag es -> do
    vs <- mapM (reflect q) es
    ns <- mapM reify vs -- DONT reify
    pure (Syntax (Con tag ns)) -- TODO: need special semantic form here
  Case scrut branches -> do
    scrut <- norm q scrut
    branches <- mapM (normAbstraction q) branches -- dont norm (just reify)
    pure (Syntax (Case scrut branches)) -- TODO: need special semantic form here?
  Letrec bindings body -> do
    undefined bindings body -- TODO: explore unfold
  Input x body -> do
    undefined x body
  Output e1 e2 -> do
    undefined e1 e2


normAbstraction :: SemEnv -> Abstraction -> M Abstraction
normAbstraction q (Abstraction xs e) = do
  xs' <- mapM (Fresh . unName) xs
  let q' = extend xs (map Named xs') q
  e <- norm q' e
  pure $ Abstraction xs' e


data SemVal
  = Syntax Exp
  | Named Name
  | Constant Byte
  | Macro String (SemVal -> M SemVal)

reify :: SemVal -> M Exp
reify = \case
  Syntax n -> pure n
  Named x -> pure $ Var x
  Constant i -> pure $ Lit i
  Macro tag f -> do
    x <- Fresh tag
    body <- Reset (f (Named x) >>= reify)
    return $ Lam x body


svApply :: SemVal -> SemVal -> M SemVal
svApply = \case
  Constant{} -> error "svApply,arg1,Constant"
  Named xf -> \arg -> do
    arg <- reify arg
    return $ Syntax (App (Var xf) arg)
  Syntax func -> \arg -> do
    arg <- reify arg
    return $ Syntax (App func arg)
  Macro tag f -> \arg -> do
    if duplicatable arg then f arg else do -- OPTIMIZATION HERE: beta/inlining!
      x <- Fresh tag
      arg <- reify arg
      Wrap (eLet x arg) (f (Named x))

duplicatable :: SemVal -> Bool
duplicatable = \case
  Constant{} -> True
  Named{} -> True
  Macro{} -> True
  Syntax{} -> False


svAdd :: SemVal -> SemVal -> SemVal
svAdd sv1 sv2 =
  case (maybeConstantByte sv1, maybeConstantByte sv2) of
    (Just i1,Just i2) ->
      Constant (i1+i2) -- OPTIMIZATION HERE: constant folding!
    _ ->
      Syntax (Add (reifyByte sv1) (reifyByte sv2))

maybeConstantByte :: SemVal -> Maybe Byte
maybeConstantByte = \case
  Constant i -> Just i
  _ -> Nothing

reifyByte :: SemVal -> Exp
reifyByte = \case
  Macro{} -> error "reifyByte, Macro"
  Constant i -> Lit i
  Named x -> Var x
  Syntax n -> n


data M a where -- monad for normalizion: fresh-idents & continuation manipulation
  Ret :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  Fresh :: String -> M Name
  Wrap :: (Exp -> Exp) -> M a -> M a
  Reset :: M Exp -> M Exp

instance Functor M where fmap = liftM
instance Applicative M where pure = return; (<*>) = ap
instance Monad M where return = Ret; (>>=) = Bind

runM :: M Exp -> Exp
runM m = snd $ loop 1 m k0 where
  k0 s e = (s,e)
  loop :: State -> M a -> (State -> a -> Res) -> Res
  loop state m k = case m of
    Ret x -> k state x
    Bind m f -> loop state m $ \state a -> loop state (f a) k
    Fresh tag -> k (state+1) $ mkName (tag ++ show state)
    Wrap f m -> f' (loop state m k) where f' (s,e) = (s,f e)
    Reset m -> let (state',v) = loop state m k0 in k state' v

type Res = (State,Exp)
type State = Int


extend :: (Ord k, Show k) => [k] -> [v] -> Map k v -> Map k v
extend xs vs p =
  if length xs /= length vs then error (show ("extend",xs,length vs)) else do
    foldl (\p (x,v) -> Map.insert x v p) p (zip xs vs)

