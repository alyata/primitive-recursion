module PRExercise where

import           Prelude hiding (and, const, fst, id, max, not, or, pred, quot,
                          rem, snd, sum)
import           PRelude

-- Exercise 1 --
----------------

diff :: (Nat, Nat) -> Nat
diff = undefined

leq, geq, lt, gt, eq :: (Nat, Nat) -> Nat
leq = undefined
geq = undefined
lt  = undefined
gt  = undefined
eq  = undefined

-- Exercise 2 --
----------------

absdiff :: (Nat, Nat) -> Nat
absdiff = undefined

max :: (Nat, Nat) -> Nat
max = undefined

-- Exercise 3 --
----------------

quot, rem :: (Nat, Nat) -> Nat
rem = undefined
quot = undefined

-- Exercise 4 --
----------------

pair :: (Nat, Nat) -> Nat
pair = undefined

-- Exercise 5 --
----------------

fst, snd :: Nat -> Nat
fst = undefined
snd = undefined

-- not a valid PR function, but for the sake of completeness
unpair :: Nat -> (Nat, Nat)
unpair(n) = (fst(n), snd(n))
