
module Spec3 where -- simpler exploration of Futamura projections

(mix,sint,bint,runMachine,runProg,quoteP,quoteB) = undefined

data Prog a -- representation for programs
runProg :: Prog a -> a
sint :: Prog (Prog a -> a) -- self-interpreter for Prog
quoteP :: Prog a -> Prog (Prog a)

data Bf -- Brainfuck
data Io -- Interaction from executing a Brainfuck
bint :: Prog (Bf -> Io) -- Brainfuck interpreter (P-coded)
quoteB :: Bf -> Prog Bf

data Machine a -- Machines/executable; we refer to these with nouns to emphasise their concreteness
runMachine :: Machine a -> a

mix :: Prog (Prog (a -> b) -> Prog a -> Machine b) -- Specializer (P-coded) for a P-Program w.r.t it's first (P-coded) input

-- projection 1 (compile things)

compileB :: Bf -> Machine Io -- compile some Brainfuck
compileB = runProg mix bint . quoteB

compileP :: Prog a -> Machine a -- compile a Prog
compileP = runProg mix sint . quoteP

-- projection 2 (create compilers)

bCompiler :: Machine (Prog Bf -> Machine Io) -- Brainfuck compiler
bCompiler = runProg mix mix (quoteP bint)

pCompiler :: Machine (Prog (Prog a) -> Machine a) -- Prog compiler
pCompiler = runProg mix mix (quoteP sint)

compileB2 :: Bf -> Machine Io -- compile some Brainfuck
compileB2 = runMachine bCompiler . quoteB

compileP2 :: Prog a -> Machine a -- compile a Prog
compileP2 = runMachine pCompiler . quoteP

-- projection 3 (create a compiler-compiler)

cogen :: Machine (Prog (Prog (a -> b)) -> Machine (Prog a -> Machine b)) -- Transform interpreters to compilers
cogen = runProg mix mix (quoteP mix)

bCompiler2 :: Machine (Prog Bf -> Machine Io) -- Brainfuck compiler
bCompiler2 = runMachine cogen (quoteP bint)

pCompiler2 :: Machine (Prog (Prog a) -> Machine a) -- Prog compiler
pCompiler2 = runMachine cogen (quoteP sint)
