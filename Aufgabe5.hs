-- Lorenz Leutgeb <e1127842@student.tuwien.ac.at>
--
-- 5. Aufgabenblatt zu Funktionale Programmierung, 2015-11-25
--
-- http://www.complang.tuwien.ac.at/knoop/lehre/ws1516/fp185A03/fp_lu05_151118.pdf

-- 1.

import Data.Ratio

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

-- 2.

type PosRat = (Nat, Nat)

type Integers = (Integer, Integer)

-- fromIntegers takes Integers and translates them to a PosRat but
-- does so after a range check: Overflows are caught and translated
-- to the canonical representation of maxNat inside the PosRat
-- domain.
fromIntegers :: Integers -> PosRat
fromIntegers (m, n)
	| m < n * 17575999 = ((fromInteger m), (fromInteger n))
	| otherwise        = (maxBound, 1)

toIntegers :: PosRat -> Integers
toIntegers (m, n) = (toInteger m, toInteger n)

validPR :: PosRat -> Bool
validPR (_, n) = n /= 0

isCanPR :: PosRat -> Bool
isCanPR (m, n)
	| m == 0    = n == 1
	| otherwise = n /= 0 && (gcd m n) == 1

mkCanPR :: PosRat -> PosRat
mkCanPR (m, n)
	| n == 0    = (0, 0)
	| m == 0    = (0, 1)
	| otherwise = ((div m (gcd m n)), (div n (gcd m n)))

-- opPR is a wrapper for wrappers around predicate or function
-- symbols that take two arguments. It guards the passed primitve and
-- makes sure to return a default value in case one of the passed
-- arguments is invalid.
opPR :: a -> (Integers -> Integers -> a) -> PosRat -> PosRat -> a
opPR x f a b
	| (validPR a) && (validPR b) = f (toIntegers a) (toIntegers b)
	| otherwise                  = x

-- pPR is a wrapper for predicate symbols with two arguments, like
-- common comparison operations. It guards the passed predicate and
-- makes sure that False is evaluated in case a passed argument is
-- invalid.
pPR :: (Integer -> Integer -> Bool) -> PosRat -> PosRat -> Bool
pPR f = opPR False (\ (m, n) (p, q) -> f (m * q) (n * p))

-- fPR is a wrapper for function symbols with two arguments, like
-- common mathematical operations. It guards the passed function and
-- makes sure to canonicalize results.
fPR :: (Integers -> Integers -> Integers) -> PosRat -> PosRat -> PosRat
fPR f a b = mkCanPR (fromIntegers (opPR (0, 0) f a b))

plusPR  = fPR (\ (m, n) (p, q) -> ((m * q) + (n * p), (n * q)))
minusPR = fPR (\ (m, n) (p, q) -> ((m * q) - (n * p), (n * q)))
timesPR = fPR (\ (m, n) (p, q) -> (m * p, n * q))
divPR   = fPR (\ (m, n) (p, q) -> (m * q, n * p))

eqPR   = pPR (==)
neqPR  = pPR (/=)
grPR   = pPR (>)
lePR   = pPR (<)
grEqPR = pPR (>=)
leEqPR = pPR (<=)


{--
___________________________________________________
@@@@@@@@@@@@@@@@@@@@@**^^""~~~"^@@^*@*@@**@@@@@@@@@
@@@@@@@@@@@@@*^^'"~   , - ' '; ,@@b. '  -e@@@@@@@@@
@@@@@@@@*^"~      . '     . ' ,@@@@(  e@*@@@@@@@@@@
@@@@@^~         .       .   ' @@@@@@, ~^@@@@@@@@@@@
@@@~ ,e**@@*e,  ,e**e, .    ' '@@@@@@e,  "*@@@@@'^@
@',e@@@@@@@@@@ e@@@@@@       ' '*@@@@@@    @@@'   0
@@@@@@@@@@@@@@@@@@@@@',e,     ;  ~^*^'    ;^~   ' 0
@@@@@@@@@@@@@@@^""^@@e@@@   .'           ,'   .'  @
@@@@@@@@@@@@@@'    '@@@@@ '         ,  ,e'  .    ;@
@@@@@@@@@@@@@' ,&&,  ^@*'     ,  .  i^"@e, ,e@e  @@
@@@@@@@@@@@@' ,@@@@,          ;  ,& !,,@@@e@@@@ e@@
@@@@@,~*@@*' ,@@@@@@e,   ',   e^~^@,   ~'@@@@@@,@@@
@@@@@@, ~" ,e@@@@@@@@@*e*@*  ,@e  @@""@e,,@@@@@@@@@
@@@@@@@@ee@@@@@@@@@@@@@@@" ,e@' ,e@' e@@@@@@@@@@@@@
@@@@@@@@@@@@@@@@@@@@@@@@" ,@" ,e@@e,,@@@@@@@@@@@@@@
@@@@@@@@@@@@@@@@@@@@@@@~ ,@@@,,0@@@@@@@@@@@@@@@@@@@
@@@@@@@@@@@@@@@@@@@@@@@@,,@@@@@@@@@@@@@@@@@@@@@@@@@
"""""""""""""""""""""""""""""""""""""""""""""""""""

              HC SVNT DRACONES
--}
