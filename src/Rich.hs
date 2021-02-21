
-- Think about evaluation (and later normalization) of a richer expression language
-- including: multi-app/lam, data-constructors + case-matching, mutual-fixpoints, and IO

module Rich (eval,Exp(..),eFix,eLet,eTrue,eFalse,eIf,eNil,eCons,eCaseList) where


import Data.Map (Map)
import Data.Word8 (Word8)
import System.IO (isEOF)
import qualified Data.Ascii as Ascii (fromChar,toChar)
import qualified Data.Map.Strict as Map


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
eCaseList scrutinee (nilBranch,(x,xs,consBranch)) =
  Case scrutinee [Abstraction [] nilBranch, Abstraction [x,xs] consBranch]


--type Name = String
newtype Name = Name { unName :: String } deriving (Eq,Ord,Show)

--type Tag = Int
newtype Tag = Tag { unTag :: Int } deriving (Show)

data Exp
  = Var Name
  | Lit Byte
  | Add Exp Exp
  | Lam Abstraction
  | App Exp [Exp]
  | Con Tag [Exp]
  | Case Exp [Abstraction]
  | Letrec [(Name,Abstraction)] Exp
  | Input Name Exp
  | Output Exp Exp
  deriving Show


newtype Byte = Byte Word8
  deriving (Num,Show)

data Abstraction = Abstraction [Name] Exp
  deriving Show


type Env = Map Name Value

extend :: [Name] -> [Value] -> Env -> Env
extend xs vs p =
  if length xs /= length vs then error (show ("extend",xs,vs)) else do
    foldl (\p (x,v) -> Map.insert x v p) p (zip xs vs)


eval :: Env -> Exp -> IO Value
eval p = \case
  Lit i -> pure (VByte i)
  Add e1 e2 -> do
    v1 <- eval p e1
    v2 <- eval p e2
    pure $ vAdd v1 v2
  Var x -> do
    pure $ maybe (error $ "eval,lookup:"++show x) id (Map.lookup x p)
  Lam abs -> do
    pure $ VClosure p abs
  App eFunc eArgs -> do
    vArgs <- mapM (eval p) eArgs
    vFunc <- eval p eFunc
    enterClosure vFunc $ \p (Abstraction xs e) ->
      eval (extend xs vArgs p) e
  Con tag es -> do
    vs <- mapM (eval p) es
    pure (VData tag vs)
  Case e1 branches -> do
    v1 <- eval p e1
    unpackData v1 $ \(Tag n) vs -> do
      let Abstraction xs e2 = branches !! n
      eval (extend xs vs p) e2
  Letrec bindings body -> do
    let names = [ x | (x,_) <- bindings ]
    let vs = [ VClosure p' abs | (_,abs) <- bindings ] -- cyclic values/env
        p' = extend names vs p
    eval p' body
  Input x body -> do
    byte <- inputByte
    eval (extend [x] [VByte byte] p) body
  Output e1 e2 -> do
    v1 <- eval p e1
    outputByte (getByte "output" v1)
    eval p e2


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
  | VClosure Env Abstraction
  | VData Tag [Value]
  deriving Show


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
