
-- Clarify the semantics of bash execution, including pipes & redirection,
-- by writing a small simulation...

module BashSim(main) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map

{-
Still todo:  (in this rough order)
- DONE: input file descriptors, primary stdin from Console
- DONE: redirects to/from file: need file system in OS sim
- DONE create/remove file (link/unlink into file-system); append to file
- spawned invocation (i.e. bash `&`): needs processes in OS sim (currently the OS sim is single threaded; bit of a useless OS)
- pipes as a kind of ephemeral file, (and so `|` in bash)

- bash parser
- real interaction
- persistance to real file-system
- find executables as files in the file system
- more basic executables: ls, ps, grep, sed
- command line args
- process exit status
- bash command substitution
-}

main :: IO ()
main = do
  putStrLn "*bash-sim*"

  let r1to2 = Redirect W 1 (FdiReference 2) -- 1>&2
  let r2to1 = Redirect W 2 (FdiReference 1) -- 2>&1

  let swap1and2 =
        [ Redirect W 3 (FdiReference 1) -- 3>&1
        , Redirect W 1 (FdiReference 2) -- 1>&2
        , Redirect W 2 (FdiReference 3) -- 1>&3
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

  let b4 = bseq
        [ B_Invoke [ Redirect R 0 (RedirectFile (Path "words")) ] BinRev
        ]

  let b5 = bseq
        [ B_Invoke [ Redirect W 1 (RedirectFile (Path "words")) ] (BinEcho "first")
        , B_Invoke [ Redirect W 1 (RedirectFile (Path "words")) ] (BinEcho "second")
        , B_Invoke [ Redirect R 0 (RedirectFile (Path "words"))
                   , Redirect W 1 (RedirectFile (Path "words.rev")) ] BinRev
        , B_Invoke [ Redirect R 0 (RedirectFile (Path "words.rev")) ] BinCat
        ]

  let b6 = bseq
        [ B_Empty
        , B_Background
          (B_Invoke [] (BinBash b1))
        , B_Invoke swap1and2 (BinBash b1)
        ]

  let _ = (b1,b2,b3,b4,b5,b6)

  let p :: OsProg = bash b6
  let fs0 = initFS
  _fs1 <- osim fs0 p
  --print _fs1
  pure ()


initFS :: FileSystem
--initFS = linkFS emptyFS (Path "words") $ Contents ["one","two","three","four"]
initFS = emptyFS


bseq :: [BashProg] -> BashProg
bseq = foldr B_Seq B_Empty


data BashProg
  = B_Empty
  | B_Invoke [Redirect] Exe
  | B_Seq BashProg BashProg
  | B_Background BashProg

data Redirect
  = Redirect Mode FDI RedirectTarget

newtype FDI = FDI Int deriving (Eq,Ord,Num,Show) -- File descriptor index

data RedirectTarget
  = FdiReference FDI
  | RedirectFile Path

data Exe
  = BinEcho String -- String is a command line arg
  | BinBash BashProg -- program is AST (i.e after parsing)
  | BinRev
  | BinCat


runX :: Exe -> OsProg
runX = \case
  BinEcho text -> echo text
  BinBash bprog -> bash bprog
  BinRev -> rev
  BinCat -> cat

-- a few standard programs...

echo :: String -> OsProg
echo text = do
  OS_GetEnv $ \OsProgEnv{stdout} ->
    OS_Write stdout text OS_Stop

rev :: OsProg
rev = do
  OS_GetEnv $ \OsProgEnv{stdin,stdout} -> do
    let
      loop = do
        OS_Read stdin $ \case
          Nothing -> OS_Stop
          Just line -> OS_Write stdout (reverse line) loop
    loop

cat :: OsProg
cat = do
  OS_GetEnv $ \OsProgEnv{stdin,stdout} -> do
    let
      loop = do
        OS_Read stdin $ \case
          Nothing -> OS_Stop
          Just line -> OS_Write stdout line loop
    loop


-- bash interpretation...

bash :: BashProg -> OsProg
bash = loop OS_Stop
  where
    loop :: OsProg -> BashProg -> OsProg
    loop k = \case

      B_Empty -> k

      B_Invoke redirects exe -> do
        myInitialFdiMap $ \fm -> do
          evalRedirects fm redirects $ \env ->
            OS_InvokeAndWait env (runX exe) k

      B_Seq p1 p2 ->
        loop (loop k p2) p1

      B_Background prog ->
        OS_Spawn (bash prog) k


-- redirection in bash...

-- TODO: kill FdiMap when OsProgEnv is richer
type FdiMap = Map FDI FD -- track which FDi/FD are opened for reading/writing

myInitialFdiMap :: (FdiMap -> OsProg) -> OsProg
myInitialFdiMap k = do
  OS_GetEnv $ \OsProgEnv{stdin,stdout,stderr} -> do
    k (Map.fromList [ (0,stdin), (1,stdout), (2,stderr) ])

evalRedirects :: FdiMap -> [Redirect] -> (OsProgEnv -> OsProg) -> OsProg
evalRedirects fm redirects k = case redirects of
  [] ->
    k $ OsProgEnv
    { stdin = look "evalRedirects" 0 fm
    , stdout = look "evalRedirects" 1 fm
    , stderr = look "evalRedirects" 2 fm
    }

  Redirect mode left right : moreRedirects -> do
    evalRedirectTarget fm mode right $ \fd -> do
      let fm' = Map.insert left fd fm
      evalRedirects fm' moreRedirects k

evalRedirectTarget :: FdiMap -> Mode -> RedirectTarget -> (FD -> OsProg) -> OsProg
evalRedirectTarget fm mode target k = case target of
  FdiReference fdi ->
    k (look "evalRedirectTarget" fdi fm)
  RedirectFile path -> do
    OS_OpenFile mode path $ \fd -> k fd


-- OS interface...

data OsProg
  = OS_Stop
  | OS_InvokeAndWait OsProgEnv OsProg OsProg -- TODO: remove
  | OS_Spawn OsProg OsProg
  | OS_GetEnv (OsProgEnv -> OsProg)
  | OS_OpenFile Mode Path (FD -> OsProg)
  | OS_Read FD (Maybe String -> OsProg)
  | OS_Write FD String OsProg

data Mode = R | W

data OsProgEnv = OsProgEnv -- TODO: generalise to be like FdiMap
  { stdin :: FD
  , stdout :: FD
  , stderr :: FD
  }

newtype FD = FD Int deriving (Eq,Ord,Num,Show) -- file descriptor


-- OS implementation...

data OsState = OsState
  { fdMap :: FdMap
  , nextFD :: FD
  , callers :: [(OsProgEnv,OsProg)] -- TODO: multiple processes!
  , consoleInputRemaining :: [String]
  , fs :: FileSystem
  , procs :: [Process]
  }

data Process = Process { env :: OsProgEnv, prog :: OsProg }


type FdMap = Map FD EndPoint
data EndPoint -- TODO: pipes
  = InConsole
  | OutConsole
  | ErrConsole
  | ReadingFile Contents
  | AppendingFile Path

osim :: FileSystem -> OsProg -> IO FileSystem
osim fs0 = loop initProgEnv initState
  where

    initProgEnv :: OsProgEnv
    initProgEnv = OsProgEnv { stdin = 99, stdout = 100, stderr = 101 }

    initState :: OsState
    initState = OsState
      { fdMap = Map.fromList [ (99,InConsole), (100,OutConsole), (101,ErrConsole) ]
      , nextFD = 102
      , callers = []
      , consoleInputRemaining = ["the","user","types","something"]
      , fs = fs0
      , procs = []
      }


    loop :: OsProgEnv -> OsState -> OsProg -> IO FileSystem
    loop env state prog = do
      let OsState{procs} = state
      case procs of
        [] -> loop' env state prog
        Process{env=env',prog=prog'}:procs -> do
          let state' = state { procs = procs ++ [Process {env,prog}] } -- round robin
          loop' env' state' prog'

    loop' :: OsProgEnv -> OsState -> OsProg -> IO FileSystem
    loop' env state = \case
      OS_Stop -> do
        let OsState{callers} = state
        case callers of
          [] -> pure (fs state)
          (env',prog):callers -> do
            let state' = state { callers }
            loop env' state' prog

      OS_InvokeAndWait env' prog suspended -> do
        let state' = state { callers = (env,suspended) : callers state }
        loop env' state' prog

      OS_Spawn spawned continue -> do
        let OsState{procs} = state
        let state' = state { procs = procs ++ [Process {env, prog = spawned}] }
        loop env state' continue

      OS_GetEnv f -> do
        loop env state (f env)

      OS_OpenFile R path f -> do
        let OsState{fs,nextFD=fd,fdMap} = state
        let contents = readFileContents fs path
        let state' = state
              { fdMap = Map.insert fd (ReadingFile contents) fdMap
              , nextFD = fd + 1
              }
        loop env state' (f fd)

      OS_OpenFile W path f -> do
        let OsState{fs,nextFD=fd,fdMap} = state
        let fs' = if existsFS fs path then fs else linkFS fs path (Contents [])
        let state' = state
              { fdMap = Map.insert fd (AppendingFile path) fdMap
              , nextFD = fd + 1
              , fs = fs'
              }
        loop env state' (f fd)

      OS_Read fd f -> do
        let OsState{fdMap} = state
        let endPoint = look "OS_Read" fd fdMap
        let (state',textMaybe) = readTarget state fd endPoint
        loop env state' (f textMaybe)

      OS_Write fd text prog -> do
        let OsState{fdMap} = state
        let endPoint = look "OS_Write" fd fdMap
        let OsState{fs=fs0} = state
        fs <- writeTarget fs0 text endPoint
        let state' = state { fs }
        loop env state' prog


readTarget :: OsState -> FD -> EndPoint -> (OsState, Maybe String)
readTarget state fd = \case
  -- TODO: error, or just return Nothing ?
  AppendingFile{} -> undefined
  OutConsole{} -> undefined
  ErrConsole{} -> undefined
  InConsole -> do
    let OsState{consoleInputRemaining=xs} = state
    case xs of
      [] -> (state,Nothing)
      line:xs' -> (state { consoleInputRemaining = xs' }, Just line)
  ReadingFile (Contents xs) -> do
    case xs of
      [] -> do
        -- TODO: remove FD from fdMap?
        (state,Nothing)
      line:xs' -> do
        let OsState{fdMap} = state
        let contents' = Contents xs'
        let fdMap' = Map.insert fd (ReadingFile contents') fdMap
        let state' = state { fdMap = fdMap' }
        (state', Just line)


writeTarget :: FileSystem -> String -> EndPoint -> IO FileSystem
writeTarget fs text = \case
  -- TODO: error, or just do nothing
  ReadingFile{} -> undefined
  InConsole{} -> undefined
  OutConsole -> do
    putStrLn $ "*out* : " ++ text
    return fs
  ErrConsole -> do
    putStrLn $ "*err* : " ++ text
    return fs
  AppendingFile path -> do
    let Contents xs = readFileContents fs path
    let contents' = Contents (xs ++ [text])
    return $ linkFS fs path contents'


-- file-system...

data FileSystem = FS (Map Path Contents)
newtype Path = Path String deriving (Eq,Ord,Show)
newtype Contents = Contents [String] deriving Show

instance Show FileSystem where
  show (FS m) =
    unlines [ unlines ((p ++ ":") : [ "  " ++ x | x <- xs ]) | (Path p,Contents xs) <- Map.toList m ]

emptyFS :: FileSystem
emptyFS = FS Map.empty

existsFS :: FileSystem -> Path -> Bool
existsFS (FS m) path = Map.member path m

linkFS :: FileSystem -> Path -> Contents -> FileSystem
linkFS (FS m) path contents = FS (Map.insert path contents m)

readFileContents :: FileSystem -> Path -> Contents
readFileContents (FS m) path = look "readFileContents" path m


-- helper for map lookup
look :: (Show k, Ord k) => String -> k -> Map k b -> b
look tag k env = maybe (error (show (tag,k))) id (Map.lookup k env)
