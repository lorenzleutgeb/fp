      3. Aufgabenblatt zu Funktionale Programmierung vom 2015-11-05

Beispiel 1.

> type Wochentag     = String
> type Schaltjahr    = Bool
> type Tag           = Int
> type Monat         = Int
> type Datum         = (Tag, Monat)
> type NterTagImJahr = Int
> type ErsterJaenner = Wochentag

Eine Hilfsfunktion, die einen Monat (und Schaltjahr-Flag) in die Zahl der Tage,
die der Monat hat, transformiert. Das hilft beim Überprüfen der Gültigkeit von
Daten.

> anzTage :: Schaltjahr -> Monat -> Int

Spezialfälle für den Februar, da hier wichtig ist, ob es sich um ein Schaltjahr
handelt.

> anzTage True  2 = 29
> anzTage False 2 = 28
> anzTage _     m = [

Es folgt eine Liste der Tage pro Monat, indiziert nach Monat, wobei hier jedoch
der Jänner ungewohnterweise mit 0 gemeint ist.
Für den Februar ist 0 vermerkt, da er als Spezialfall behandelt wird.

>		31,  0, 31, 30,
>		31, 30, 31, 31,
>		30, 31, 30, 31
>	] !! (m - 1)

Eine weitere Hilfsfunktion um Daten zu validieren, die ja im Wesentlichen den
Wertebereich des Gregorianischen Kalenders abdecken.

> gueltigesDatum :: Schaltjahr -> Datum -> Bool
> gueltigesDatum sj (t, m)
>	| t < 1 || t > (anzTage sj m) = False
>	| otherwise = True

tdw ermittelt den einen numerischen Index für einen Wochentag. Hilft beim
Rechnen und Validieren.

> tdw :: Wochentag -> Int
> tdw t
>	| t == "Montag"     = 0
>	| t == "Dienstag"   = 1
>	| t == "Mittwoch"   = 2
>	| t == "Donnerstag" = 3
>	| t == "Freitag"    = 4
>	| t == "Samstag"    = 5
>	| t == "Sonnabend"  = 5
>	| t == "Sonntag"    = 6
>	| otherwise         = 7

tag ist die (informell gesprochen) "Umkehrungsfunktion" zu tdw.

> tag :: Int -> Wochentag
> tag i
>	| i == 0 = "Montag"
>	| i == 1 = "Dienstag"
>	| i == 2 = "Mittwoch"
>	| i == 3 = "Donnerstag"
>	| i == 4 = "Freitag"
>	| i == 5 = "Samstag"
>	| i == 6 = "Sonntag"
>	| otherwise = ""

tdj übersetzt ein Datum in den entsprechenden jahresbasierten Index.

> tdj :: Schaltjahr -> Datum -> NterTagImJahr
> tdj _ (t, 0) = 0
> tdj sj (0, m) = (anzTage sj m) + (tdj sj (0, m - 1))
> tdj sj (t, m) = t + (tdj sj (0, m - 1))

Eine Verison von wochentag aus Beispielblatt 2 ohne Validierung.

> wochentag :: ErsterJaenner -> Schaltjahr -> NterTagImJahr -> Wochentag
> wochentag ej sj ntij = tag(mod ((ntij - 1) + tdw(ej)) 7)

erster ermittelt den Wochentag des ersten Jänners aus dem Wochentag eines
beliebigen Datums des selben Jahres.

> erster :: NterTagImJahr -> Wochentag -> Wochentag
> erster ntij wt = tag (mod ((6 - (mod ((ntij) - tdw(wt) - 1) 7)) + 1) 7)

wochentag3 ermittelt auf Grund der Information, welcher Wochentag (wt) an einem
gegebenen Datum (t, j) war, auf welchen Wochentag (w', t') fällt.

> wochentag3 :: Datum -> Wochentag -> Schaltjahr -> Datum -> Wochentag
> wochentag3 (t, m) wt sj (t', m')
>	| not (gueltigesDatum sj (t, m))   = "Falsche Eingabe"
>	| not (gueltigesDatum sj (t', m')) = "Falsche Eingabe"
>	| tdw wt == 7                      = "Falsche Eingabe"
>	| otherwise = (wochentag (erster (tdj sj (t, m)) wt) sj (tdj sj (t', m')))

Beispiel 2.

> pwv :: [String] -> Int
> pwv [] = 0
> pwv l = length . distinct . filter threeVocals $ l

> distinct :: Eq a => [a] -> [a]
> distinct [] = []
> distinct l@(x:xs)
>       | not (elem x xs) = x : distinct xs
>       | otherwise       =     distinct xs

> intersect :: Eq a => [a] -> [a] -> [a]
> intersect a b = filter (`elem` a) b

> threeVocals :: String -> Bool
> threeVocals s = (length . intersect ['a','e','i','o','u'] $ s) > 2

Beispiel 3.

> streamline :: Eq a => [a] -> Int -> [a]
> streamline [] _ = []
> streamline l n
>       | n < 1 = []
>       | otherwise = [ x | x <- l, ((length.filter (==x)) l) == n ]

Beispiel 4.

> type Kennzeichen = ((Char, Char, Char), (Int, Int, Int))

> notRange c a b = c < a || c > b
> notCapAlph c = notRange c 'A' 'Z'   
> notDigit   c = notRange c  0   9 

> cg :: (Kennzeichen -> Kennzeichen) -> Kennzeichen -> Kennzeichen
> cg f t@((a, b, c), (x, y, z))
>	| notCapAlph a || notCapAlph b || notCapAlph c = t
>	| notDigit x || notDigit y || notDigit z = t
>	| otherwise = f t

> nf' :: Kennzeichen -> Kennzeichen
> nf' (('Z', 'Z', 'Z'), (9, 9, 9)) = ((   'A',    'A',    'A'), (0, 0, 0))

> nf' ((  x, 'Z', 'Z'), (9, 9, 9)) = ((succ x,    'A',    'A'), (0, 0, 0))
> nf' ((  x,   y, 'Z'), (9, 9, 9)) = ((     x, succ y,    'A'), (0, 0, 0))
> nf' ((  x,   y,   z), (9, 9, 9)) = ((     x,      y, succ z), (0, 0, 0))

> nf' (k, (a, 9, 9)) = (k, (a + 1,     0,     0))
> nf' (k, (a, b, 9)) = (k, (a    , b + 1,     0))
> nf' (k, (a, b, c)) = (k, (a    , b    , c + 1))

> nf :: Kennzeichen -> Kennzeichen
> nf = cg nf'

> vg' :: Kennzeichen -> Kennzeichen
> vg' (('A', 'A', 'A'), (0, 0, 0)) = ((   'Z',    'Z',    'Z'), (9, 9, 9))

> vg' ((  x, 'A', 'A'), (0, 0, 0)) = ((pred x,    'Z',    'Z'), (9, 9, 9))
> vg' ((  x,   y, 'A'), (0, 0, 0)) = ((     x, pred y,    'Z'), (9, 9, 9))
> vg' ((  x,   y,   z), (0, 0, 0)) = ((     x,      y, pred z), (9, 9, 9))

> vg' (k, (a, 0, 0)) = (k, (a - 1,     9,     9))
> vg' (k, (a, b, 0)) = (k, (a    , b - 1,     9))
> vg' (k, (a, b, c)) = (k, (a    , b    , c - 1))

> vg :: Kennzeichen -> Kennzeichen
> vg = cg vg' 
