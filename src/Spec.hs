
-- Understanding the types of self-applicable partial evaluation...
module Spec where

-- type for programs. Prog in a small functional langauge, equipped with imperative get/put
data Prog a

-- parsing a Prog program from a string
parseP :: String -> Prog Io
parseP = undefined -- already written

-- some specific Prog program
fprogY :: Prog Io
fprogY = parseP "get \\x -> put x; put (x+1); done"

-- quote an arbitary value into a Prog program which represents it
--quote :: a -> Prog a -- THIS IS NOT POS IN GENERAL

-- quote a Prog program into Prog
quoteP :: Prog a -> Prog (Prog a)
quoteP = undefined -- easy

-- compose Prog programs in a type safe way
appP :: Prog (a -> b) -> Prog a -> Prog b
appP = undefined -- easy

-- interpreter for Prog programs; written in Haskell
h_pint :: Prog a -> a
h_pint = undefined -- already written

-- (self) interpreter for Prog programs; written in Prog
p_pint :: Prog (Prog a -> a)
p_pint = undefined -- transliterate Haskell version to Prog

-- derive (H-coded) Prog interpreter from (P-coded) Prog interpreter; via the original (H-coded) Prog interpreter
-- this will surely be a much slower interpreter!
--h_pint1 :: Prog a -> a
--h_pint1 f = h_pint (appP p_pint (quoteP f))


data B -- type of Brainfuck programs
data Io -- type of interactions from running Brainfuck programs

-- parsing a Brainfuck program from a string
parseB :: String -> B
parseB = undefined -- already written

-- quote a brainfuck program into Prog
quoteB :: B -> Prog B
quoteB = undefined -- easy

-- interpreter for Brainfuck programs; written in Haskell
h_bint0 :: B -> Io
h_bint0 = undefined -- already written; but dont need

-- interpreter for Brainfuck programs; written in Prog
p_bint :: Prog (B -> Io)
p_bint = undefined -- transliterate Haskell version to Prog

-- run (P-coded) Brainfuck interpreter; using the Haskell Prog interpreter
h_bint :: B -> Io
--h_bint b = h_pint (appP p_bint (quoteB b))
h_bint = h_pint p_bint

-- some specific Brainfuck program
bprogX :: B
bprogX = parseB ",.+."

-- Interaction from running a specific Brainfuck program
interactionX :: Io
interactionX = h_bint bprogX



-- Machines: i.e. executables. target for compilation
data Machine a
runM :: Machine a -> a
runM = undefined

-- specializer for Prog programs; written in Haskell
h_mix0 :: Prog (a -> b) -> Prog a -> Machine b
h_mix0 = undefined

-- specializer for Prog programs; written in Prog
p_mix :: Prog (Prog (a -> b) -> Prog a -> Machine b)
p_mix = undefined -- transliterate Haskell version to Prog

-- run (P-coded) specializer; using Haskell Prog interpreter
h_mix :: Prog (a -> b) -> Prog a -> Machine b
h_mix = h_pint p_mix



-- 1st Futamura projection...

-- Specialize the Brainfuck interpreter to a specific brainfuck program; in effect compiling that program
p_progX :: Machine Io
p_progX = h_mix p_bint (quoteB bprogX)

-- Generalize above compilation to get a B-compiler
compileB :: B -> Machine Io
compileB bprog = h_mix p_bint (quoteB bprog)

-- Compile a specific P-program by specializing the Prog self-interpreter to that program
fprogY1 :: Machine Io
fprogY1 = h_mix p_pint (quoteP fprogY)

-- Generalize above compilation to get a P-program compiler
compileP :: Prog a -> Machine a
compileP f = h_mix p_pint (quoteP f)


-- 2nd Futamura projection...

-- Specialize the (P-coded) Specializer to the (P-coded) Brainfuck interpreter; creating a Brainfuck compiler
m_compileB :: Machine (Prog B -> Machine Io)
m_compileB = h_mix p_mix (quoteP p_bint)

-- Specialize the (P-coded) Specializer to the (P-coded) Prog interpreter; creating a P-Program compiler
m_compileP :: Machine (Prog (Prog a) -> Machine a)
m_compileP = h_mix p_mix (quoteP p_pint)


-- 3rd Futamura projection...

-- Specialize the (P-coded) Specializer to the (P-coded) Specializer; creating a compiler-compiler
m_cogen :: Machine (Prog (Prog (a -> b)) -> Machine (Prog a -> Machine b))
m_cogen = h_mix p_mix (quoteP p_mix)

-- Use cogen to generate a Brainfuck compiler from an (P-coded) Brainfuck interpreter
m_compileB2 :: Machine (Prog B -> Machine Io)
m_compileB2 = runM m_cogen (quoteP p_bint)

-- Use cogen to generate a Prog compiler from an (P-coded) Self-interpreter
m_compileP2 :: Machine (Prog (Prog a) -> Machine a)
m_compileP2 = runM m_cogen (quoteP p_pint)
