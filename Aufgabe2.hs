type Pence         = Int
type Shilling      = Int
type PoundSterling = Int
type Amount        = (PoundSterling, Shilling, Pence)

pence :: Amount -> Pence
pence (_, _, p) = p

shilling :: Amount -> Shilling
shilling (_, s, _) = s

poundSterling :: Amount -> PoundSterling
poundSterling (ps, _, _) = ps

isSound :: Amount -> Bool
isSound (ps, s, p)
	| ps < 0 = False
	| s  < 0 = False
	| p  < 0 = False
	| otherwise = True

normalize :: Amount -> Amount
normalize (ps, s, p)
	| not(isSound (ps, s, p)) = (ps, s, p)
	| p >= 20 = normalize(ps, s + 1, p - 20)
	| s >= 12 = normalize(ps + 1, s - 12, p)
	| otherwise = (ps, s, p)

add :: Amount -> Amount -> Amount
add (ps1, s1, p1) (ps2, s2, p2)
	| not(isSound (ps1, s1, p1)) || not(isSound (ps2, s2, p2)) = error "Falsche Eingabe"
	| otherwise = normalize (ps1 + ps2, s1 + s2, p1 + p2)

type InterestRate = Float
type Years        = Int
type Period       = Years

interest :: Amount -> InterestRate -> Period -> Amount
interest a r p
	| not(isSound a) = (0, 0, 0)
	| r < 0 = (0, 0, 0)
	| p < 1 = (0, 0, 0)
	| otherwise = normalize (0, 0, round (accurateInterest (penceOnly a) r p))
	where
		accurateInterest :: Float -> InterestRate -> Period -> Float
		accurateInterest a r 0 = a
		accurateInterest a r p = accurateInterest (a + a * r) r (p - 1)

		penceOnly :: Amount -> Float
		penceOnly (ps, s, p) = fromIntegral (p + s * 20 + ps * 20 * 12) :: Float


type DistanceAtDetection = Float   -- Einheit: km
type Speed               = Float   --          km pro s
type TimeBeforeCollision = Integer --          s
type Distance            = Integer --          km

dist :: DistanceAtDetection -> Speed -> Speed -> TimeBeforeCollision -> Distance
dist d v1 v2 t
	| d < 0 || v1 < 0 || v2 < 0 || t < 0 = -1
	| (tof t) > timeToCrash = -1
	| otherwise = round (d - v1 * timeTravelled - v2 * timeTravelled)
	where
		tof i = fromIntegral i :: Float
		timeToCrash = d / (v1 + v2)
		timeTravelled = timeToCrash - (tof t)

curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c)

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a, b, c) = f a b c

type Wochentag     = String
type Schaltjahr    = Bool
type ErsterJaenner = Wochentag
type Tag           = Int
type Monat         = Int
type Datum         = (Tag, Monat)
type NterTagImJahr = Int

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

gueltigesDatum :: Schaltjahr -> Datum -> Bool
gueltigesDatum sj (t, m)
	| t < 1 || t > (anzTage sj m) = False
	| otherwise = True

wochentag2 :: ErsterJaenner -> Schaltjahr -> Datum -> Wochentag
wochentag2 ej sj (t, m)
	| tdw ej == 7 = "Falsche Eingabe"
	| not (gueltigesDatum sj (t, m)) = "Falsche Eingabe"
	| otherwise = wochentag ej sj (tdj sj (t, m))

-- anzTage gibt die Anzahl der Tage eines gegebenen Monats zurück.
anzTage :: Schaltjahr -> Monat -> Int
anzTage True 2 = 29
anzTage False 2 = 28
anzTage _ m = [31, 0, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] !! (m - 1)

-- tdj ("Tag des Jahres") löst ein Datum in einen NTerTagImJahr auf.
tdj :: Schaltjahr -> Datum -> NterTagImJahr
tdj _ (t, 0) = 0
tdj sj (0, m) = (anzTage sj m) + (tdj sj (0, m - 1))
tdj sj (t, m) = t + (tdj sj (0, m - 1))

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
