
-- Clarify the semantics of bash execution, including pipes & redirection,
-- by writing a small simulation...

module BashSim(main) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  putStrLn "*bash-sim*"

  let r1to2 = RedirectO 1 (FdiReference 2) -- 1>&2
  let r2to1 = RedirectO 2 (FdiReference 1) -- 2>&1

  let swap1and2 =
        [ RedirectO 3 (FdiReference 1) -- 3>&1
        , RedirectO 1 (FdiReference 2) -- 1>&2
        , RedirectO 2 (FdiReference 3) -- 1>&3
        ]

  let _ = (r1to2,r2to1,swap1and2)

  let b1 = bseq
        [ B_Invoke [] (BinEcho "message1")
        , B_Invoke [r1to2] (BinEcho "ERROR MESSAGE")
        , B_Invoke [] (BinEcho "message2")
        ]

  let b2 = bseq
        [ B_Empty
        , B_Invoke [] (BinEcho "normal...")
        , B_Invoke [] (BinBash b1)
        , B_Invoke [] (BinEcho "swapped...")
        , B_Invoke swap1and2 (BinBash b1)
        ]

  let p :: OsProg = bash b2
  osim p


bseq :: [BashProg] -> BashProg
bseq = foldr B_Seq B_Empty


data BashProg
  = B_Empty
  | B_Invoke [Redirect] Exe
  | B_Seq BashProg BashProg
 -- TODO: support: backgrounding, pipe & input redirects

data Redirect
  = RedirectO FDI RedirectTarget

newtype FDI = FDI Int deriving (Eq,Ord,Num,Show) -- File descriptor index

data RedirectTarget
  = FdiReference FDI
  -- TODO: redirect to file

data Exe
  = BinEcho String -- String is command line arg
  | BinBash BashProg


runX :: Exe -> OsProg
runX = \case
  BinEcho text -> echo text
  BinBash bprog -> bash bprog


echo :: String -> OsProg
echo text = do
  OS_GetStdout $ \stdout ->
    OS_Write stdout text OS_Stop


bash :: BashProg -> OsProg
bash = loop OS_Stop
  where
    loop :: OsProg -> BashProg -> OsProg
    loop k = \case

      B_Empty -> k

      B_Invoke redirects exe -> do
        myInitialFdiMap $ \fm -> do
          let env = evalRedirects fm redirects
          OS_Invoke env (runX exe) k

      B_Seq p1 p2 ->
        loop (loop k p2) p1


type FdiMap = Map FDI Ofd

myInitialFdiMap :: (FdiMap -> OsProg) -> OsProg
myInitialFdiMap k = do
  OS_GetStdout $ \stdout -> do
    OS_GetStderr $ \stderr -> do
      k (Map.fromList [ (1,stdout), (2,stderr) ])

evalRedirects :: FdiMap -> [Redirect] -> OsProgEnv
evalRedirects fm = \case
  [] ->
    OsProgEnv
    { stdout = look "evalRedirects" 1 fm
    , stderr = look "evalRedirects" 2 fm
    }
  RedirectO left right : moreRedirects -> do
    let fm' = Map.insert left (evalRedirectTarget fm right) fm
    evalRedirects fm' moreRedirects

evalRedirectTarget :: FdiMap -> RedirectTarget -> Ofd
evalRedirectTarget fm = \case
  FdiReference fdi -> look "evalRedirectTarget" fdi fm


data OsProg
  = OS_Stop
--  | OS_Exec / OS_Fork
  | OS_Invoke OsProgEnv OsProg OsProg
  | OS_GetStdout (Ofd -> OsProg)
  | OS_GetStderr (Ofd -> OsProg)
  | OS_Write Ofd String OsProg
  -- TODO: reading from input file descriptors

data OsProgEnv = OsProgEnv
  { stdout :: Ofd
  , stderr :: Ofd
  }

type OEnv = Map Ofd OTarget
data OTarget = OutConsole | ErrConsole -- TODO: | FileAppend | Pipe ...

newtype Ofd = -- output file descriptor
  Ofd Int deriving (Eq,Ord,Num,Show)

-- TODO: input file descriptor


data OsState = OsState
  { oenv :: OEnv
  , callers :: [(OsProgEnv,OsProg)]
  -- TODO: file system!
  }

osim :: OsProg -> IO ()
osim = loop initProgEnv initOsState
  where

    initProgEnv :: OsProgEnv
    initProgEnv = OsProgEnv { stdout = 100, stderr = 101 }

    initOsState :: OsState
    initOsState = OsState
      { oenv = Map.fromList [ (100,OutConsole), (101,ErrConsole) ]
      , callers = []
      }

    loop :: OsProgEnv -> OsState -> OsProg -> IO ()
    loop env state = \case
      OS_Stop -> do
        let OsState{callers} = state
        case callers of
          [] -> pure ()
          (env',prog):callers -> do
            let state' = state { callers }
            loop env' state' prog

      OS_Invoke env' prog suspended -> do
        let state' = state { callers = (env,suspended) : callers state }
        loop env' state' prog

      OS_GetStdout f -> do
        let OsProgEnv{stdout} = env
        loop env state (f stdout)

      OS_GetStderr f -> do
        let OsProgEnv{stderr} = env
        loop env state (f stderr)

      OS_Write ofd text prog -> do
        let OsState{oenv} = state
        let otarget = look "OS_Write" ofd oenv
        writeTarget text otarget
        loop env state prog


writeTarget :: String -> OTarget -> IO ()
writeTarget str = \case
  OutConsole ->
    putStrLn $ "*out* : " ++ str
  ErrConsole ->
    putStrLn $ "*err* : " ++ str


-- helper for map lookup
look :: (Show k, Ord k) => String -> k -> Map k b -> b
look tag k env = maybe (error (show (tag,k))) id (Map.lookup k env)
