type Wochentag     = String
type Schaltjahr    = Bool
type ErsterJaenner = Wochentag
type NterTagImJahr = Int

-- tdw ("Tag der Woche") übersetzt einen Wochentag in seinen Index
-- beginnend bei 0 (Montag) und endend mit 6 (Sonntag). Fuer unbekannte
-- Eingabe werte wird 7 retourniert.
tdw :: Wochentag -> Int
tdw t
	| t == "Montag"     = 0
	| t == "Dienstag"   = 1
	| t == "Mittwoch"   = 2
	| t == "Donnerstag" = 3
	| t == "Freitag"    = 4
	| t == "Samstag"    = 5
	| t == "Sonnabend"  = 5
	| t == "Sonntag"    = 6
	| otherwise         = 7

-- tag ist das Gegenstueck zu tdw. Ein gegebener Index wird in den
-- entsprechenden Index übersetzt. Dabei wird 5 immer in "Samstag"
-- und nie in "Sonnabend" übersetzt. Für unbekannte Argumente wird
-- "" retourniert.
tag :: Int -> Wochentag
tag i
	| i == 0 = "Montag"
	| i == 1 = "Dienstag"
	| i == 2 = "Mittwoch"
	| i == 3 = "Donnerstag"
	| i == 4 = "Freitag"
	| i == 5 = "Samstag"
	| i == 6 = "Sonntag"
	| otherwise = ""

wochentag :: ErsterJaenner -> Schaltjahr -> NterTagImJahr -> Wochentag
wochentag ej sj ntij
	| tdw ej == 7 = "Falsche Argumente"
	| ntij < 1             = "Falsche Argumente"
	| ntij > 365 && not sj = "Falsche Argumente"
	| ntij > 366 &&     sj = "Falsche Argumente"
	| otherwise = tag(mod ((ntij - 1) + tdw(ej)) 7)

-- fac retourniert die Fakultät einer positiven Ganzzahl.
fac :: Integer -> Integer
fac n = product [1..n]

-- choose retoruniert den Binomialkoeffizient zweier positiver
-- Ganzzahlen.
choose :: Integer -> Integer -> Integer
choose n r
	| r < 0     = 0
	| r > n     = 0
	| otherwise = div (fac n) (fac r * fac (n - r))

vc :: Integer -> Integer -> Integer
vc n r
	| n < r     = -1
	| r < 0     = -1
	| otherwise = n `choose` r * fac r

vuc :: (Integer, Integer) -> Integer
vuc (n, r) = uncurry vc (n, r)

type FrequencyList = [(Int, Char)]

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []
quicksort (x:xs) =
	let lo = quicksort [y | y <- xs, y <= x]
	    hi = quicksort [y | y <- xs, y > x]
	in  lo ++ [x] ++ hi

freq :: String -> FrequencyList
freq "" = []
freq s = [ (c, x) | x <- ['\0'..'\255'], let c = (length.filter (==x)) s, c > 0 ]

unfreq :: FrequencyList -> String
unfreq [] = ""
unfreq f = concat(map unfc f)
	where unfc (f, c) = if f < 1 then "" else [c] ++ unfc (f - 1, c)

frequencySort :: String -> String
frequencySort s = unfreq(quicksort(freq(s)))

pcheck :: Integer -> Bool
pcheck n = pal(oct(n))
	where
		pal s = s == reverse s
		oct n
			| n < 8     = show(n)
			| otherwise = oct(div n 8) ++ show(mod n 8)
