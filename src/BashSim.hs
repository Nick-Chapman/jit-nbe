
-- Clarify the semantics of bash execution, including pipes & redirection,
-- by writing a small simulation...

module BashSim(main) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map

{-
Still todo:  (in this rough order)
- DONE: input file descriptors, primary stdin from Console
- redirects to/from file: need file system in OS sim
- create/remove file (link/unlink into file-system); append to file
- spawned invocation (i.e. bash `&`): needs processes in OS sim (currently the OS sim is single threaded; bit of a useless OS)
- pipes as a kind of ephemeral file, (and so `|` in bash)

- bash parser
- real interaction
- persistance to real file-system
- find executables as files in the file system
- more basic executables: cat (can write that in bash), ls, ps, grep, rev, sed
- command line args
- process exit status
- bash command substitution
-}

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

  let b3 = bseq
        [ B_Invoke [] BinRev
        ]

  let _ = (b1,b2,b3)
  let p :: OsProg = bash b3
  osim p


bseq :: [BashProg] -> BashProg
bseq = foldr B_Seq B_Empty


data BashProg
  = B_Empty
  | B_Invoke [Redirect] Exe
  | B_Seq BashProg BashProg

data Redirect
  = RedirectO FDI RedirectTarget

newtype FDI = FDI Int deriving (Eq,Ord,Num,Show) -- File descriptor index

data RedirectTarget
  = FdiReference FDI

data Exe
  = BinEcho String -- String is command line arg
  | BinBash BashProg
  | BinRev


runX :: Exe -> OsProg
runX = \case
  BinEcho text -> echo text
  BinBash bprog -> bash bprog
  BinRev -> revLines


echo :: String -> OsProg
echo text = do
  OS_GetEnv $ \OsProgEnv{stdout} ->
    OS_Write stdout text OS_Stop


revLines :: OsProg
revLines = do
  OS_GetEnv $ \OsProgEnv{stdin,stdout} -> do
    let
      loop = do
        OS_Read stdin $ \case
          Nothing -> OS_Stop
          Just line -> do
            OS_Write stdout (reverse line) loop
    loop


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


type FdiMap = Map FDI FD

myInitialFdiMap :: (FdiMap -> OsProg) -> OsProg
myInitialFdiMap k = do
  OS_GetEnv $ \OsProgEnv{stdin,stdout,stderr} -> do
    k (Map.fromList [ (0,stdin), (1,stdout), (2,stderr) ])

evalRedirects :: FdiMap -> [Redirect] -> OsProgEnv
evalRedirects fm = \case
  [] ->
    OsProgEnv
    { stdin = look "evalRedirects" 0 fm
    , stdout = look "evalRedirects" 1 fm
    , stderr = look "evalRedirects" 2 fm
    }
  RedirectO left right : moreRedirects -> do
    let fm' = Map.insert left (evalRedirectTarget fm right) fm
    evalRedirects fm' moreRedirects

evalRedirectTarget :: FdiMap -> RedirectTarget -> FD
evalRedirectTarget fm = \case
  FdiReference fdi -> look "evalRedirectTarget" fdi fm


data OsProg
  = OS_Stop
--  | OS_Exec / OS_Fork
  | OS_Invoke OsProgEnv OsProg OsProg
  | OS_GetEnv (OsProgEnv -> OsProg)
  | OS_Read FD (Maybe String -> OsProg)
  | OS_Write FD String OsProg
  -- TODO: reading from input file descriptors

data OsProgEnv = OsProgEnv
  { stdin :: FD
  , stdout :: FD
  , stderr :: FD
  }

newtype FD = FD Int deriving (Eq,Ord,Num,Show) -- file descriptor

-- TODO: input file descriptor


data OsState = OsState
  { fdMap :: FdMap
  , callers :: [(OsProgEnv,OsProg)]
  -- TODO: file system!
  -- TODO: multiple processes!
  , consoleInputRemaining :: [String]
  }


type FdMap = Map FD EndPoint
data EndPoint = InConsole | OutConsole | ErrConsole -- TODO: files and pipes


osim :: OsProg -> IO ()
osim = loop initProgEnv initOsState
  where

    initProgEnv :: OsProgEnv
    initProgEnv = OsProgEnv { stdin = 99, stdout = 100, stderr = 101 }

    initOsState :: OsState
    initOsState = OsState
      { fdMap = Map.fromList [ (99,InConsole), (100,OutConsole), (101,ErrConsole) ]
      , callers = []
      , consoleInputRemaining = ["one","two","three"]
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

      OS_GetEnv f -> do
        loop env state (f env)

      OS_Read fd f -> do
        let OsState{fdMap} = state
        let endPoint = look "OS_Read" fd fdMap
        let (state',textMaybe) = readTarget state endPoint
        loop env state' (f textMaybe)

      OS_Write fd text prog -> do
        let OsState{fdMap} = state
        let endPoint = look "OS_Write" fd fdMap
        writeTarget text endPoint
        loop env state prog


readTarget :: OsState -> EndPoint -> (OsState, Maybe String)
readTarget state = \case
  OutConsole{} -> undefined -- TODO: report as error, or just return Nothing ?
  ErrConsole{} -> undefined
  InConsole -> do
    let OsState{consoleInputRemaining=xs} = state
    case xs of
      [] -> (state,Nothing)
      line:xs' -> (state { consoleInputRemaining = xs' }, Just line)


writeTarget :: String -> EndPoint -> IO ()
writeTarget str = \case
  InConsole ->
    undefined -- TODO: report as error, orjust do nothing
  OutConsole ->
    putStrLn $ "*out* : " ++ str
  ErrConsole ->
    putStrLn $ "*err* : " ++ str


-- helper for map lookup
look :: (Show k, Ord k) => String -> k -> Map k b -> b
look tag k env = maybe (error (show (tag,k))) id (Map.lookup k env)
