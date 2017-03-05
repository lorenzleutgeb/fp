> data Zeichen = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P
>                | Q | R | S | T | U | V | W | X | Y | Z
>                deriving (Eq, Ord, Show)
> data Ziffer  = Null | Eins | Zwei | Drei | Vier | Fuenf | Sechs | Sieben
>                | Acht | Neun deriving (Eq, Ord, Show)
> newtype Nat = Nat ((Zeichen, Zeichen, Zeichen), (Ziffer, Ziffer, Ziffer))
>               deriving (Eq, Show)

> zet :: [(Zeichen, Int)]
> zet = [
>	(A,  0),
>	(B,  1),
>	(C,  2),
>	(D,  3),
>	(E,  4),
>	(F,  5),
>	(G,  6),
>	(H,  7),
>	(I,  8),
>	(J,  9),
>	(K, 10),
>	(L, 11),
>	(M, 12),
>	(N, 13),
>	(O, 14),
>	(P, 15),
>	(Q, 16),
>	(R, 17),
>	(S, 18),
>	(T, 19),
>	(U, 20),
>	(V, 21),
>	(W, 22),
>	(X, 23),
>	(Y, 24),
>	(Z, 25)]

> zetoi :: Zeichen -> Int
> zetoi ze
>	| not ((length l) == 1) = 0
>	| otherwise = snd (l !! 0)
>	where l = filter ((==ze).fst) zet

> itoze :: Int -> Zeichen
> itoze i
>	| not ((length l) == 1) = A
>	| otherwise = fst (l !! 0)
>	where l = filter ((==i).snd) zet

> zit :: [(Ziffer, Int)]
> zit = [
>	(Null,   0),
>	(Eins,   1),
>	(Zwei,   2),
>	(Drei,   3),
>	(Vier,   4),
>	(Fuenf,  5),
>	(Sechs,  6),
>	(Sieben, 7),
>	(Acht,   8),
>	(Neun,   9)]

> zitoi :: Ziffer -> Int
> zitoi zi
>	| not((length l) == 1) = 0
>	| otherwise = snd (l !! 0)
>	where l = filter ((==zi).fst) zit

> itozi :: Int -> Ziffer
> itozi i
>	| not((length l) == 1) = Null
>	| otherwise = fst (l !! 0)
>	where l = filter ((==i).snd) zit

> nattoi :: Nat -> Int
> nattoi (Nat ((a', b', c'), (x', y', z'))) = (max (min (a + b + c + x + y + z) 17575999) 0)
>	where   a = 26^2 * 10^3 * zetoi a'
>		b = 26^1 * 10^3 * zetoi b'
>		c = 26^0 * 10^3 * zetoi c'
>		x = 26^0 * 10^2 * zitoi x'
>		y = 26^0 * 10^1 * zitoi y'
>		z = 26^0 * 10^0 * zitoi z'

> itonat :: Int -> Nat
> itonat i = Nat ((a, b, c), (x, y, z))
>	where   a = itoze (div (mod i (26^3 * 10^3)) (26^2 * 10^3))
>		b = itoze (div (mod i (26^2 * 10^3)) (26^1 * 10^3))
>		c = itoze (div (mod i (26^1 * 10^3)) (26^0 * 10^3))
>		x = itozi (div (mod i (26^0 * 10^3)) (26^0 * 10^2))
>		y = itozi (div (mod i (26^0 * 10^2)) (26^0 * 10^1))
>		z = itozi (div (mod i (26^0 * 10^1)) (26^0 * 10^0))

> plusN :: Nat -> Nat -> Nat
> plusN a b = (itonat ((nattoi a) + (nattoi b)))

> minusN :: Nat -> Nat -> Nat
> minusN a b = (itonat ((nattoi a) - (nattoi b)))

> timesN :: Nat -> Nat -> Nat
> timesN a b = (itonat ((nattoi a) * (nattoi b)))

> divN :: Nat -> Nat -> Nat
> divN a b = (itonat (div (nattoi a) (nattoi b)))

> modN :: Nat -> Nat -> Nat
> modN a b
>	| b == (Nat ((A, A, A), (Null, Null, Null))) = error "Division durch 0 nicht moeglich"
>	| otherwise = (itonat (mod (nattoi a) (nattoi b)))

> powerN :: Nat -> Nat -> Nat
> powerN a b
>	| b == (Nat ((A, A, A), (Null, Null, Null))) = error "Division durch 0 nicht moeglich"
>	| otherwise = (itonat ((nattoi a) ^ (nattoi b)))

> eqN :: Nat -> Nat -> Bool
> eqN a b = a == b

> neqN :: Nat -> Nat -> Bool
> neqN a b = not (a == b)

> grN :: Nat -> Nat -> Bool
> grN a b = (nattoi a) > (nattoi b)

> leN :: Nat -> Nat -> Bool
> leN a b = (nattoi a) < (nattoi b)

> grEqN :: Nat -> Nat -> Bool
> grEqN a b = (nattoi a) >= (nattoi b)

> leEqN :: Nat -> Nat -> Bool
> leEqN a b = (nattoi a) <= (nattoi b)

> data Wochentag = Montag | Dienstag | Mittwoch | Donnerstag | Freitag
>                  | Samstag | Sonntag deriving (Eq,Show)
> type Tag       = Nat -- mit Nat aus Teilaufgabe 1
> data Monat     = Jaenner | Feber | Maerz | April | Mai | Juni | Juli
>                  | August | September | Oktober | November | Dezember
>                  deriving (Eq,Show)
> type Jahr      = Nat -- mit Nat aus Teilaufgabe 1
> type Datum     = (Tag, Monat, Jahr)

> sj :: Jahr -> Bool
> sj nj	= (mod j 400) == 0 || ((mod j 4) == 0 && (mod j 100) /= 0)
>	where j = nattoi nj

> tag :: Int -> Wochentag
> tag i
>	| i == 0 = Montag
>	| i == 1 = Dienstag
>	| i == 2 = Mittwoch
>	| i == 3 = Donnerstag
>	| i == 4 = Freitag
>	| i == 5 = Samstag
>	| i == 6 = Sonntag
>	| otherwise = error "Wochentag out of range"

> ej :: Jahr -> Wochentag
> ej nj
>	| j == 0 = Samstag
>	| sj nj = 
>	where j = nattoi nj
