module UniversalMachine (
    Tape(..)
    , Number(..)
    , Inst(..)
    , Point(..)
    , Machine(..)
    , readHead
    , moveLeft
    , moveRight
) where

import Number (Number(..), inc, dec)

class Point a where
    point :: a

instance Point Number where
    point = Zero

instance Point Inst where
    point = Halt

{-
    Halt: Stop the machine
    Increment: Erese the Number currenty at read of the memory tape and replece it with the Successor
    Decrement: Replace with Previous, if not present Halt
    MoveRight: Advances the memory tape by one step to the right
    MoveLeft: Advances the memory tape by one step to the left
    EnterLoop: If the Number at the head of memory tape is 0,
               advance the head of the program tape untill the matching ExitLoop instruction.
    ExitLoop: Uf the Number at the head of memeory tape Not 0,
               rewind the head of the program tape to the matching EnterLoop instruction.
-}
data Inst = MoveLeft
        | MoveRight
        | Increment
        | Decrement
        | EnterLoop
        | ExitLoop
        | Halt
        deriving (Show, Eq)

data Tape a = Tape [a] a [a]
    deriving (Show, Eq)

{-
    Machine consists of a program tape and a memory tape
    program consist of a list of instructions
    memory acts as a register of computations
-}
type Program = (Tape Inst)
type Memory = (Tape Number)
data Machine = Machine Program Memory
    deriving (Show, Eq)

readHead :: Point a => Tape a -> a
readHead (Tape _ a _) = a

moveLeft :: Point a => Tape a -> Tape a
moveLeft (Tape (x:xs) a ys) = Tape xs x (a:ys)
moveLeft (Tape [] a ys) = Tape [] point (a:ys)

moveRight :: Point a => Tape a -> Tape a
moveRight (Tape xs a (y:ys)) = Tape (a:xs) y ys
moveRight (Tape xs a []) = Tape (a:xs) point []

{-
read :: State Machine Inst
read = do
    (machine, program, _) <- get
    return (readHead program)
-}