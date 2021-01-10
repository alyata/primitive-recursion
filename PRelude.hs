module PRelude where

import           Prelude hiding (and, const, fst, id, max, not, or, pred, quot,
                          rem, snd, sum)

data Nat = Zero | Succ Nat deriving Eq

instance Num Nat where
  fromInteger 0 = Zero
  fromInteger x = if x > 0 then Succ $ fromInteger (x - 1) else undefined

instance Show Nat where
  show n = show (toInt n)
    where
      toInt :: Nat -> Int
      toInt Zero     = 0
      toInt (Succ n) = succ (toInt n)

-- Primitive Functions --
-------------------------

z :: Nat -> Nat
z(_) = 0

s :: Nat -> Nat
s(n) = Succ(n)

id_1_1 :: Nat -> Nat
id_1_1(a) = a

id_2_1, id_2_2 :: (Nat, Nat) -> Nat
id_2_1(a, _) = a
id_2_2(_, b) = b

id_3_1, id_3_2, id_3_3 :: (Nat, Nat, Nat) -> Nat
id_3_1(a, _, _) = a
id_3_2(_, b, _) = b
id_3_3(_, _, c) = c

id_4_1, id_4_2, id_4_3, id_4_4 :: (Nat, Nat, Nat, Nat) -> Nat
id_4_1(a, _, _, _) = a
id_4_2(_, b, _, _) = b
id_4_3(_, _, c, _) = c
id_4_4(_, _, _, d) = d

-- default id
id = id_1_1

-- Function Combinators --
--------------------------

cn1 :: (Nat -> Nat, a -> Nat) -> (a -> Nat)
cn1(f, g1) = \a -> f(g1(a))

cn2 :: ((Nat, Nat) -> Nat, a -> Nat, a -> Nat) -> (a -> Nat)
cn2(f, g1, g2) = \a -> f(g1(a), g2(a))

cn3 :: ((Nat, Nat, Nat) -> Nat, a -> Nat, a -> Nat, a -> Nat) -> (a -> Nat)
cn3(f, g1, g2, g3) = \a -> f(g1(a), g2(a), g3(a))

pr1 :: (Nat -> Nat, (Nat, Nat) -> Nat)
    -> (Nat -> Nat)
pr1(f, g) = cn2(pr2(f, cn2(g, id_3_2, id_3_3)), id_1_1, id_1_1)

pr2 :: (Nat -> Nat, (Nat, Nat, Nat) -> Nat)
    -> ((Nat, Nat) -> Nat)
pr2(f, g) = h
  where
    h(x1, 0)      = f(x1)
    h(x1, Succ y) = g(x1, y, h(x1, y))

pr3 :: ((Nat, Nat) -> Nat, (Nat, Nat, Nat, Nat) -> Nat)
    -> ((Nat, Nat, Nat) -> Nat)
pr3(f, g) = h
  where
    h(x1, x2, 0)      = f(x1, x2)
    h(x1, x2, Succ y) = g(x1, x2, y, h(x1, x2, y))

pr4 :: ((Nat, Nat, Nat) -> Nat, (Nat, Nat, Nat, Nat, Nat) -> Nat)
    -> ((Nat, Nat, Nat, Nat) -> Nat)
pr4(f, g) = h
  where
    h(x1, x2, x3, 0)      = f(x1, x2, x3)
    h(x1, x2, x3, Succ y) = g(x1, x2, x3, y, h(x1, x2, x3, y))

-- default pr
pr = pr2

-- prelude functions --
-----------------------

-- arithmetic functions

const :: Nat -> Nat -> Nat
const Zero     = z
const (Succ n) = cn1(s, const n)

add :: (Nat, Nat) -> Nat
add = pr(id, cn1(s, id_3_3))

sgn :: Nat -> Nat
sgn = pr1(const 0, cn1(const 1, id_2_1))

mult :: (Nat, Nat) -> Nat
mult = pr(const 0, cn2(add, id_3_1, id_3_3))

pow :: (Nat, Nat) -> Nat
pow = pr(const 1, cn2(mult, id_3_1, id_3_3))

sum :: ((Nat, Nat) -> Nat) -> ((Nat, Nat) -> Nat)
sum f = pr(cn2(f, id, z),
           cn2(add, id_3_3, cn2(f, id_3_1, cn1(s, id_3_2))))

prod :: ((Nat, Nat) -> Nat) -> ((Nat, Nat) -> Nat)
prod f = pr(cn2(f, id, z),
            cn2(mult, id_3_3, cn2(f, id_3_1, cn1(s, id_3_2))))

-- logical operators

not :: Nat -> Nat
not = pr1(const 1, cn1(const 0, id_2_1))

and :: (Nat, Nat) -> Nat
and = cn1(sgn, mult)

or :: (Nat, Nat) -> Nat
or = cn1(sgn, add)
