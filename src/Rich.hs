
-- Think about evaluation (and later normalization) of a richer expression language
-- including: multi-app/lam, data-constructors + case-matching, mutual-fixpoints, and IO

module Rich (main,eval,Exp(..),eFix,eLet,eTrue,eFalse,eIf,eNil,eCons,eCaseList,eUnit) where

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

  let _prog = Lam inc (with3 (\n3 -> App id (Lam x (Add (App (App id id) (Add n3 (Lit 42))) (App dub (App (Var inc) (Var x)))))))
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

  let _prog = App (Lam x (Output (Add (Lit 60) (Var x)) eUnit)) (Lit 5)
        where --inc = Lam x (Add (Var x) (Lit 1))
              x = mkName"x"

  let _prog = Input x (Input y (Output (Var x) (Output (Var y) eUnit)))
             where x = mkName"x"
                   y = mkName"y"

  let _prog = eIf eTrue (Lit 12,Lit 34)

  let prog = Case (Con (Tag 0) [Lit 12, Lit 34]) [Alternative [x,y] (Add (Var x) (Var y))]
        where x = mkName"x"
              y = mkName"y"

  let _prog = App (App twice twice) inc
        where twice = Lam f (Lam x (App (Var f) (App (Var f) (Var x))))
              inc = Lam x (Add (Var x) (Lit 1))
              f = mkName"f"
              x = mkName"x"

  {-let wrapContext e = App (App e inc) (Lit 5)
        where inc = Lam x (Add (Var x) (Lit 1))
              x = mkName"x"-}

  let wrapContext e = e
  --let wrapContext e = App e (Lit 100)

  putStrLn ("prog:\n"++ show prog)
  res0 <- eval p0 (wrapContext prog)
  print ("res0:",res0)

  let norm = normalize prog
  putStrLn ("norm:\n"++ show norm)

  res1 <- eval p0 (wrapContext norm)
  print ("res1:",res1)

  pure ()


------------------------------------------------------------------------
-- derived constructors

eFix :: Name -> Name -> Exp -> Exp
eFix f x body = Letrec [(f,(x,body))] (Var f)

eLet :: Name -> Exp -> Exp -> Exp
eLet x rhs body = Case (Con (Tag 0) [rhs]) [Alternative [x] body]

eTrue :: Exp
eTrue = Con (Tag 0) []

eFalse :: Exp
eFalse = Con (Tag 1) []

eIf :: Exp -> (Exp,Exp) -> Exp
eIf cond (thenBranch,elseBranch) =
  Case cond [Alternative [] thenBranch, Alternative [] elseBranch]

eNil :: Exp
eNil = Con (Tag 0) []

eCons :: Exp -> Exp -> Exp
eCons x xs = Con (Tag 1) [x,xs]

eCaseList :: Exp -> (Exp, (Name,Name,Exp)) -> Exp
eCaseList scrut (nilBranch,(x,xs,consBranch)) =
  Case scrut [Alternative [] nilBranch, Alternative [x,xs] consBranch]

eUnit :: Exp
eUnit = Con (Tag 0) []


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
  | Case Exp [Alternative]
  | Letrec [(Name,(Name,Exp))] Exp
  | Input Name Exp
  | Output Exp Exp

newtype Byte = Byte Word8
  deriving (Num)

data Alternative = Alternative [Name] Exp
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
    bracket (show tag++"[") "]" (seperated "," [ pretty e | e <- es ])
  Case scrut branches -> do
    bracket "case " " of" (pretty scrut)
    ++ indent (concat [ prettyAlternative (Tag n) abs | (n,abs) <- zip [0..] branches ])
  Letrec bindings body -> do
    undefined bindings body
  Input x body -> do
    ["let " ++ show x ++ " = input() in"] ++ pretty body
  Output e1 e2 -> do
    bracket "output(" ");" (pretty e1)
    ++ pretty e2

prettyAlternative :: Tag -> Alternative -> Lines
prettyAlternative tag (Alternative xs body) = do
  let lhs = show tag ++ show xs
  hangIndented (lhs ++ " ->") (pretty body)

instance Show Tag where show (Tag n) = "Tag-" ++ show n
instance Show Name where show (Name s) = s
instance Show Byte where show (Byte w) = show w

paren :: Lines -> Lines
paren = bracket "(" ")"

bracket :: String -> String -> Lines -> Lines
bracket left right = onHead (left ++) . onTail (++ right)

seperated :: String -> [Lines] -> Lines
seperated sep = \case
  [] -> [""]
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
    pure $ VClosure p x body
  App e1 e2 -> do
    v2 <- eval p e2
    v1 <- eval p e1
    enterClosure v1 $ \p x body ->
      eval (extend [x] [v2] p) body
  Con tag es -> do
    vs <- mapM (eval p) es
    pure (VData tag vs)
  Case e1 branches -> do
    v1 <- eval p e1
    unpackData v1 $ \(Tag n) vs -> do
      let Alternative xs e2 = branches !! n
      eval (extend xs vs p) e2
  Letrec bindings body -> do -- TODO: not tested yet
    let names = [ f | (f,_) <- bindings ]
    let vs = [ VClosure p' x e | (_,(x,e)) <- bindings ] -- cyclic values/env
        p' = extend names vs p
    eval p' body
  Input x body -> do
    byte <- inputByte
    eval (extend [x] [VByte byte] p) body
  Output e1 e2 -> do
    v1 <- eval p e1
    outputByte (getByte "output" v1)
    eval p e2


----------------------------------------------------------------------
-- values

vAdd :: Value -> Value -> Value
vAdd v1 v2 = VByte (getByte "add/1" v1 + getByte "add/2" v2)

getByte :: String -> Value -> Byte
getByte tag = \case VByte i -> i; v -> error (show (tag,v))

enterClosure :: Value -> (Env -> Name -> Exp -> r) -> r
enterClosure v k = case v of VClosure p x e -> k p x e; _ -> error (show ("enterClosure",v))

unpackData :: Value -> (Tag -> [Value] -> r) -> r
unpackData v k = case v of VData tag vs -> k tag vs; _ -> error (show ("unpackData",v))

data Value
  = VByte Byte
  | VClosure Env Name Exp
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
outputByte (Byte w8) = do
  --putChar (Ascii.toChar w8)
  print ("outputByte",Ascii.toChar w8)

----------------------------------------------------------------------
-- normalization

normalize :: Exp -> Exp
normalize = runM . norm Map.empty

norm :: SemEnv -> Exp -> M Exp
norm q = reflect q >=> reify


type SemEnv = Map Name SemVal

reflect :: SemEnv -> Exp -> M Reflected
reflect q = \case
  Lit b -> do
    pure $ Semantic (Constant b)
  Add e1 e2 -> do
    sv1 <- reflect q e1
    sv2 <- reflect q e2
    pure $ svAdd sv1 sv2
  Var x -> do
    pure $ Semantic (maybe (error $ "reflect,lookup:"++show x) id (Map.lookup x q))
  Lam x body -> do
    pure $ Semantic $ Macro (unName x) $ \arg -> do
      reflect (extend [x] [arg] q) body
  App eFunc eArg -> do
    svArg <- reflect q eArg
    svFunc <- reflect q eFunc
    svApply svFunc svArg

  Con tag es -> do
    vs <- mapM (reflect q >=> shareReflected) es
    pure $ Semantic (Data tag vs)

  Case scrut branches -> do
    scrutR <- reflect q scrut
    case maybeConstantData scrutR of
      Just (Tag n,vs) -> do
        -- OPTIMIZATION HERE: branch selection!
        let Alternative xs rhs = branches !! n
        reflect (extend xs vs q) rhs
      Nothing -> do
        scrutN <- reify scrutR
        -- TODO: duplicate continuation across branches
        -- possible code explosion; but always good when just one branch (i.e. tuples)
        branches <- mapM (normAlternative q) branches
        pure (Syntactic (Case scrutN branches))

  Letrec bindings body -> do
    undefined bindings body -- TODO: explore unfold
  Input x e -> do
    withFresh x q $ \x q -> do
      n <- norm q e
      pure (Syntactic (Input x n))
  Output e1 e2 -> do
    n1 <- norm q e1
    n2 <- norm q e2
    pure (Syntactic (Output n1 n2))


shareReflected :: Reflected -> M SemVal
shareReflected = \case
  Semantic sv -> pure sv
  Syntactic exp -> do
    undefined exp
    --x <- Fresh "data"
    --Wrap (eLet x exp) (pure (Named x))


normAlternative :: SemEnv -> Alternative -> M Alternative
normAlternative q (Alternative xs e) = do
  xs' <- mapM (Fresh . unName) xs -- TODO: use withFresh
  let q' = extend xs (map Named xs') q
  e <- norm q' e
  pure $ Alternative xs' e


withFresh :: Name -> SemEnv -> (Name -> SemEnv -> M r) -> M r
withFresh x q k = do
  x' <- (Fresh . unName) x
  let q' = extend [x] [Named x'] q
  k x' q'


data Reflected
  = Syntactic Exp
  | Semantic SemVal

data SemVal -- sharable semantic values
  = Named Name
  | Constant Byte
  | Macro String (SemVal -> M Reflected)
  | Data Tag [SemVal]


reify :: Reflected -> M Exp
reify = \case
  Syntactic e -> pure e
  Semantic s -> reifyS s

reifyS :: SemVal -> M Exp
reifyS = \case
  Named x -> pure $ Var x
  Constant i -> pure $ Lit i
  Macro tag f -> do
    x <- Fresh tag
    body <- Reset (f (Named x) >>= reify)
    return $ Lam x body
  Data tag vs -> do
    es <- mapM reifyS vs
    pure $ Con tag es

svApply :: Reflected -> Reflected -> M Reflected -- TODO: inline at caller?
svApply = \case
  Syntactic func -> \arg -> do
    arg <- reify arg
    return $ Syntactic (App func arg)
  Semantic s ->
    svApplyS s

svApplyS :: SemVal -> Reflected -> M Reflected
svApplyS = \case
  Constant{} -> error "svApply,arg1,Constant"
  Data{} -> error "svApply,arg1,Data"
  Named xf -> \arg -> do
    arg <- reify arg
    return $ Syntactic (App (Var xf) arg)
  Macro tag func -> \case
    Semantic arg -> func arg -- OPTIMIZATION HERE: beta/inlining!
    Syntactic arg -> do
      x <- Fresh tag
      Wrap (eLet x arg) (func (Named x))


svAdd :: Reflected -> Reflected -> Reflected
svAdd sv1 sv2 =
  case (maybeConstantByte sv1, maybeConstantByte sv2) of
    (Just i1,Just i2) ->
      Semantic (Constant (i1+i2)) -- OPTIMIZATION HERE: constant folding!
    _ ->
      Syntactic (Add (reifyByte sv1) (reifyByte sv2))

maybeConstantByte :: Reflected -> Maybe Byte
maybeConstantByte = \case
  Semantic (Constant i) -> Just i
  _ -> Nothing

reifyByte :: Reflected -> Exp
reifyByte = \case
  Syntactic e -> e
  Semantic s -> reifyByteS s

reifyByteS :: SemVal -> Exp
reifyByteS = \case
  Macro{} -> error "reifyByte, Macro"
  Data{} -> error "reifyByte, Data"
  Constant i -> Lit i
  Named x -> Var x


maybeConstantData :: Reflected -> Maybe (Tag,[SemVal])
maybeConstantData = \case
  Semantic (Data tag svs) -> Just (tag,svs)
  _ -> Nothing



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

