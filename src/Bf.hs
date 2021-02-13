
module Bf(main) where

import qualified Data.Ascii as Ascii (fromChar,toChar)
import Data.Word8 (Word8)
import System.IO (isEOF)

main :: IO ()
main = do
  putStrLn "*bf*"
  s <- readFile "../bf/b/mandelbrot.b"
  --s <- readFile "../bf/b/factor.b"
  top s

top :: String -> Eff
top = run . parse

parse :: String -> [Op]
parse = loop [] []
  where
    loop :: [[Op]] -> [Op] -> String -> [Op]
    loop stack acc = \case
      [] -> case stack of [] -> reverse acc; _:_ -> error "["
      c:s -> case c of
        '.' -> loop stack (D : acc) s
        ',' -> loop stack (C : acc) s
        '<' -> loop stack (L : acc) s
        '>' -> loop stack (R : acc) s
        '+' -> loop stack (P : acc) s
        '-' -> loop stack (M : acc) s
        '[' -> loop (acc:stack) [] s
        ']' -> case stack of [] -> error "]"; acc':stack' -> loop stack' (Block (reverse acc) : acc') s
        _ -> loop stack acc s


-- brainfuck interpreter in haskell. code in very vanilla style
-- plan is to translate this to AST for a small func interpreter


data Op = P | M | L | R | D | C | Block [Op]

run :: [Op] -> IO ()
run is = steps is initT $ \_ -> pure ()

steps :: [Op] -> Tape -> (Tape -> Eff) -> Eff
steps is t k = case is of
  [] -> k t
  i:is -> step i t $ \t -> steps is t k

step :: Op -> Tape -> (Tape -> Eff) -> Eff
step = \case
  P -> plus
  M -> minus
  L -> left
  R -> right
  D -> dot
  C -> com
  Block xs -> block xs


type Tape = ([Byte],Byte,[Byte])

block :: [Op] -> Tape -> (Tape -> Eff) -> Eff
block is t k =
  if isZeroB (readT t)
  then k t
  else steps is t $ \t -> block is t k


dot :: Tape -> (Tape -> Eff) -> Eff
dot t k = putE (readT t) $ \() -> k t

com :: Tape -> (Tape -> Eff) -> Eff
com t k = getE $ \c -> k (writeT t c)

plus :: Tape -> (Tape -> Eff) -> Eff
plus t k = k (plusT t)

minus :: Tape -> (Tape -> Eff) -> Eff
minus t k = k (minusT t)

left :: Tape -> (Tape -> Eff) -> Eff
left t k = k (leftT t)

right :: Tape -> (Tape -> Eff) -> Eff
right t k = k (rightT t)


initT :: Tape
initT = ([],zeroB,[])

readT :: Tape -> Byte
readT (_,x,_) = x

writeT :: Tape -> Byte -> Tape
writeT (l,_,r) c = (l,c,r)

plusT :: Tape -> Tape
plusT (l,c,r) = (l,plusB c,r)

minusT :: Tape -> Tape
minusT (l,c,r) = (l,minusB c,r)

leftT :: Tape -> Tape
leftT (l,c,r) = caseL l (\() -> ([],zeroB,c:r)) (\x l -> (l,x,c:r))

rightT :: Tape -> Tape
rightT (l,c,r) = caseL r (\() -> (c:l,zeroB,[])) (\x r -> (c:l,x,r))

caseL :: [a] -> (() -> k) -> (a -> [a] -> k) -> k
caseL xs n c = case xs of
  [] -> n ()
  x:xs -> c x xs


-- from here we have what will be builtin in the small func language

type Byte = Word8
type Eff = IO ()

plusB :: Byte -> Byte
plusB x = x+1

minusB :: Byte -> Byte
minusB x = x-1

zeroB :: Byte
zeroB = 0

isZeroB :: Byte -> Bool
isZeroB = (== 0)

getE :: (Byte -> Eff) -> Eff
getE k = getW8 >>= k

putE :: Byte -> (() -> Eff) -> Eff
putE c k = do putW8 c; k ()


putW8 :: Byte -> IO ()
putW8 w = putChar (Ascii.toChar w)

getW8 :: IO Byte
getW8 = do
  isEOF >>= \case
    True -> pure 0
    False -> do
      char <- getChar
      case Ascii.fromChar char of
        Just w -> pure w
        Nothing -> error ("get, non ascii char: " ++ show char)
