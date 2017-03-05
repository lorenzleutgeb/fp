module Aufgabe6 where

import Data.Ratio
import Data.List

data Zeichen =
	A | B | C | D | E | F | G | H | I | J | K | L | M |
	N | O | P | Q | R | S | T | U | V | W | X | Y | Z
	deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Ziffer =
	Null | Eins | Zwei | Drei | Vier |
	Fuenf | Sechs | Sieben | Acht | Neun
	deriving (Eq, Ord, Show, Read, Enum, Bounded)

newtype Nat = Nat ((Zeichen, Zeichen, Zeichen), (Ziffer, Ziffer, Ziffer))

bridge :: (Integer -> Integer -> Integer) -> Nat -> Nat -> Nat
bridge f a b = fromInteger (f (toInteger a) (toInteger b))

instance Eq Nat where
	(/=) (Nat (a, b)) (Nat (a', b')) = a /= a' || b /= b'

instance Ord Nat where
	compare (Nat (a, b)) (Nat (a', b'))
		| a > a'             = GT
		| a == a' && b > b'  = GT
		| a == a' && b == b' = EQ
		| otherwise          = LT

instance Show Nat where
	show (Nat ((a, b, c), (x, y, z))) =
		"\""                ++
		(show a)            ++
		(show b)            ++
		(show c)            ++
		" "                 ++
		(show (fromEnum x)) ++
		(show (fromEnum y)) ++
		(show (fromEnum z)) ++
		"\""

instance Num Nat where
	negate _ = 0

	abs = id

	signum n
		| n == 0    = 0
		| otherwise = 1

	fromInteger i
		| i <= 0        = minBound
		| i >= 17575999 = maxBound
		| otherwise     = Nat ((a, b, c), (x, y, z))
		where
			a = f 17576000 676000
			b = f   676000  26000
			c = f    26000   1000
			x = f     1000    100
			y = f      100     10
			z = f       10      1
			f m n = toEnum (fromInteger (div (mod i m) n))

	(+) = bridge (+)
	(-) = bridge (-)
	(*) = bridge (*)

instance Enum Nat where
	fromEnum = fromInteger . toInteger
	toEnum   = fromInteger . toInteger

instance Real Nat where
	toRational n = (toInteger n) % 1

instance Integral Nat where
	toInteger (Nat ((a, b, c), (x, y, z))) =
		(a' + b' + c' + x' + y' + z')
		where
			a' = f 676000 a
			b' = f  26000 b
			c' = f   1000 c
			x' = f    100 x
			y' = f     10 y
			z' = f      1 z
			f m n = m * (toInteger (fromEnum n))

	div = bridge div
	mod = bridge mod

	quotRem a b = (fromInteger a', fromInteger b')
		where (a', b') = quotRem (toInteger a) (toInteger b)


instance Bounded Nat where
	minBound = (Nat ((A, A, A), (Null, Null, Null)))
	maxBound = (Nat ((Z, Z, Z), (Neun, Neun, Neun)))

data Num a => Tree a = Nil | Node (a -> a) (a -> Bool) [a] (Tree a) (Tree a) (Tree a)

smt :: Num a => (Tree a) -> (Tree a)
smt Nil = Nil
smt (Node f p l c1 c2 c3) = Node f p l' (smt c1) (smt c2) (smt c3)
				where l' = map f (filter p l)

data Num a => STree a = SNil
	| SNode [a] (STree a) (STree a) (STree a) deriving (Eq, Show)

t2st :: Num a => (Tree a) -> (STree a)
t2st Nil = SNil
t2st (Node _ _ l c1 c2 c3) = SNode l (t2st c1) (t2st c2) (t2st c3)

tsum :: Num a => STree a -> a
tsum SNil = 0
tsum (SNode l c1 c2 c3) = (sum l) + (tsum c1) + (tsum c2) + (tsum c3)

tdepth :: Num a => STree a -> Integer
tdepth SNil = 0
tdepth (SNode _ c1 c2 c3) = (max (max (tdepth c1) (tdepth c2)) (tdepth c3)) + 1

type Node = Integer
type Edge = (Node, Node)
type Source = Node
type Sink = Node

newtype Graph = Gr [(Source, [Sink])] deriving (Eq, Show)

ne :: Graph -> ([Node], [Edge])
ne _ = ([], [])

independent :: Graph -> [Node] -> Bool
independent (Gr g) v' = (length (intersect v' (a ++ (concat b)))) == 0 where (a, b) = unzip g

cover :: Graph -> [Node] -> Bool
cover _ _ = False
