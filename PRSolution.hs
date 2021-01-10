module PRSolution where

import           Prelude hiding (and, const, fst, id, max, not, or, pred, quot,
                          rem, snd, sum)
import           PRelude

-- Exercise 1 --
----------------

pred :: Nat -> Nat
pred = pr1(const 0, id_2_1)

diff :: (Nat, Nat) -> Nat
diff = pr(id, cn1(pred, id_3_3))

leq, geq, lt, gt, eq :: (Nat, Nat) -> Nat
leq = cn1(pr1(const 1, cn1(const 0, id_2_1)), diff)
geq = cn2(leq, id_2_2, id_2_1)
lt  = cn1(not, geq)
gt  = cn1(not, leq)
eq  = cn2(and, leq, geq)

-- Exercise 2 --
----------------

absdiff :: (Nat, Nat) -> Nat
absdiff = cn2(add, diff, cn2(diff, id_2_2, id_2_1))

max :: (Nat, Nat) -> Nat
max = cn2(add, cn2(mult, geq, id_2_1), cn2(mult, lt, id_2_2))

-- Exercise 3 --
----------------

quot, rem :: (Nat, Nat) -> Nat
rem = cn2(rem_flipped, id_2_2, id_2_1)
  where
    rem_flipped = let sRem = cn1(s, id_3_3)
                   in pr(const 0, cn2(mult, sRem, cn2(lt, sRem, id_3_1)))
quot = cn2(quot_flipped, id_2_2, id_2_1)
  where
    quot_flipped = let sRem = cn1(s, cn2(rem, id_3_2, id_3_1))
                    in pr(const 0, cn2(add, id_3_3, cn2(eq, id_3_1, sRem)))

-- Exercise 4 --
----------------

pair :: (Nat, Nat) -> Nat
pair = cn2(quot,
        cn2(add,
          cn2(add,
            cn2(pow, cn2(add, id_2_1, id_2_2), cn1(const 2, id_2_1)),
            cn2(mult, cn1(const 3, id_2_1), id_2_1)
          ),
          id_2_2
        ),
        cn1(const 2, id_2_1)
      )

-- Exercise 5 --
----------------

-- This function finds the "leader" of the group it belongs to. The leader is
-- the encoding of some (0, k) pair, and all pairs (a, b) in the group are such
-- that a + b = k.
--
-- leader(n) = go(n, n)
--   where
--     go(n, 0) = pair(0, 0)
--     go(n, k+1) = if pair(0, k+1) <= n then pair(0, k+1) else go(n, k)
leader :: Nat -> Nat
leader = cn2(go, id_1_1, id_1_1)
  where
    go = pr(
           cn2(pair, const 0, const 0),
           let p = cn2(pair, cn1(const 0, id_3_1), cn1(s, id_3_2)) in
               cn3(pr3(id_2_2, id_4_1), p, id_3_3, cn2(leq, p, id_3_1))
         )

fst :: Nat -> Nat
fst = cn2(diff, id_1_1, leader)

-- compute the k in pair(0, k) = n where n is the leader of the given encoding.
-- leaderk(n) = go(n, n)
--   where
--     go(n, 0) = 0
--     go(n, k+1) = if pair(0, k+1) <= n then k+1 else go(n, k)
leaderk:: Nat -> Nat
leaderk = cn2(go, id_1_1, id_1_1)
  where
    go = pr(
           const 0,
           let k = cn1(s, id_3_2)
               p = cn2(pair, cn1(const 0, id_3_1), k) in
               cn3(pr3(id_2_2, id_4_1), k, id_3_3, cn2(leq, p, id_3_1))
         )

snd :: Nat -> Nat
snd = cn2(diff, leaderk, fst)

-- not a valid PR function, but for the sake of completeness
unpair :: Nat -> (Nat, Nat)
unpair(n) = (fst(n), snd(n))
