-- Imports ---------------------------------------------------------------------

import Aufgabe5 (Nat(..), Zeichen(..), Ziffer(..),
                 isCanPR, mkCanPR, plusPR, minusPR, timesPR, divPR,
                 eqPR, neqPR, grPR, lePR, grEqPR, leEqPR)
import Test.HUnit

-- Numbers ---------------------------------------------------------------------

nat0 = Nat ((A, A, A), (Null, Null, Null))
nat1 = Nat ((A, A, A), (Null, Null, Eins))
nat2 = Nat ((A, A, A), (Null, Null, Zwei))
nat3 = Nat ((A, A, A), (Null, Null, Drei))
nat4 = Nat ((A, A, A), (Null, Null, Vier))
nat5 = Nat ((A, A, A), (Null, Null, Fuenf))
nat6 = Nat ((A, A, A), (Null, Null, Sechs))
nat8 = Nat ((A, A, A), (Null, Null, Acht))
nat11 = Nat ((A, A, A), (Null, Eins, Eins))
nat14 = Nat ((A, A, A), (Null, Eins, Vier))
nat15 = Nat ((A, A, A), (Null, Eins, Fuenf))
nat20 = Nat ((A, A, A), (Null, Zwei, Null))
nat24 = Nat ((A, A, A), (Null, Zwei, Vier))
nat29 = Nat ((A, A, A), (Null, Zwei, Neun))
nat30 = Nat ((A, A, A), (Null, Drei, Null))
nat31 = Nat ((A, A, A), (Null, Drei, Eins))
nat120 = Nat ((A, A, A), (Eins, Zwei, Null))
nat123 = Nat ((A, A, A), (Eins, Zwei, Drei))
nat200 = Nat ((A, A, A), (Zwei, Null, Null))
nat823 = Nat ((A, A, A), (Acht, Zwei, Drei))
nat1337 = Nat ((A, A, B), (Drei, Drei, Sieben))
nat1500 = Nat ((A, A, B), (Fuenf, Null, Null))
nat1600 = Nat ((A, A, B), (Sechs, Null, Null))
nat1700 = Nat ((A, A, B), (Sieben, Null, Null))
nat1750 = Nat ((A, A, B), (Sieben, Fuenf, Null))
nat1751 = Nat ((A, A, B), (Sieben, Fuenf, Eins))
nat1752 = Nat ((A, A, B), (Sieben, Fuenf, Zwei))
nat1755 = Nat ((A, A, B), (Sieben, Fuenf, Fuenf))
nat1760 = Nat ((A, A, B), (Sieben, Sechs, Null))
nat1775 = Nat ((A, A, B), (Sieben, Sieben, Fuenf))
nat1800 = Nat ((A, A, B), (Acht, Null, Null))
nat1900 = Nat ((A, A, B), (Neun, Null, Null))
nat1999 = Nat ((A, A, B), (Neun, Neun, Neun))
nat2000 = Nat ((A, A, C), (Null, Null, Null))
nat2001 = Nat ((A, A, C), (Null, Null, Eins))
nat2015 = Nat ((A, A, C), (Null, Eins, Fuenf))
nat2674 = Nat ((A, A, C), (Sechs, Sieben, Vier))
nat12345 = Nat ((A, A, M), (Drei, Vier, Fuenf))
nat88178 = Nat ((A, D, K), (Eins, Sieben, Acht))
nat255367 = Nat ((A, J, V), (Drei, Sechs, Sieben))
nat1234500 = Nat ((B, V, M), (Fuenf, Null, Null))
nat1246845 = Nat ((B, V, Y), (Acht, Vier, Fuenf))
nat1860867 = Nat ((C, T, O), (Acht, Sechs, Sieben))
nat2960546 = Nat ((E, J, W), (Fuenf, Vier, Sechs))
nat3301053 = Nat ((E, W, Z), (Null, Fuenf, Drei))
nat4060225 = Nat ((G, A, E), (Zwei, Zwei, Fuenf))
nat5921092 = Nat ((I, T, T), (Null, Neun, Zwei))
nat6602106 = Nat ((J, T, Y), (Eins, Null, Sechs))
nat8120450 = Nat ((M, A, I), (Vier, Fuenf, Null))
nat9903159 = Nat ((O, Q, X), (Eins, Fuenf, Neun))
nat11421503 = Nat ((Q, X, H), (Fuenf, Null, Drei))
nat16505265 = Nat ((Y, K, V), (Zwei, Sechs, Fuenf))
natMaxMinusOne = Nat ((Z, Z, Z), (Neun, Neun, Acht))
natMax = Nat ((Z, Z, Z), (Neun, Neun, Neun))

posRatUndefined = (nat0, nat0)

-- Main ------------------------------------------------------------------------

main = runTestTT $ TestList [
                              testEq
                            , testOrd
                            , testShow
                            , testNum
                            , testIsCanPR
                            , testMkCanPR
                            , testPlusPR
                            , testMinusPR
                            , testTimesPR
                            , testDivPR
                            , testEqPR
                            , testNeqPR
                            , testGrPR
                            , testLePR
                            , testGrEqPR
                            , testLeEqPR
                            , testPosRat
                            ]

-- 1 ---------------------------------------------------------------------------

testEq = TestLabel "Eq" $ TestList
  [ TestCase $ assertEqual
      "nat12345 /= nat1234500"
      True
      (nat12345 /= nat1234500)
  , TestCase $ assertEqual
      "nat2000 /= nat200"
      True
      (nat2000 /= nat200)
  , TestCase $ assertEqual
      "nat1 /= nat0"
      True
      (nat1 /= nat0)
  , TestCase $ assertEqual
      "nat0 /= nat2000"
      True
      (nat0 /= nat2000)
  , TestCase $ assertEqual
      "nat123 /= nat3"
      True
      (nat123 /= nat3)
  , TestCase $ assertEqual
      "nat2015 /= nat2015"
      False
      (nat2015 /= nat2015)
  , TestCase $ assertEqual
      "natMax /= natMaxMinusOne"
      True
      (natMax /= natMaxMinusOne)
  , TestCase $ assertEqual
      "natMaxMinusOne /= natMaxMinusOne"
      False
      (natMaxMinusOne /= natMaxMinusOne)
  ]

testOrd  = TestLabel "Ord" $ TestList
  [ TestCase $ assertEqual
      "nat1337 `compare` nat1337"
      EQ
      (nat1337 `compare` nat1337)
  , TestCase $ assertEqual
      "nat0 `compare` nat1"
      LT
      (nat0 `compare` nat1)
  , TestCase $ assertEqual
      "nat1 `compare` nat0"
      GT
      (nat1 `compare` nat0)
  , TestCase $ assertEqual
      "nat4060225 `compare` nat1"
      GT
      (nat4060225 `compare` nat1)
  , TestCase $ assertEqual
      "nat1 `compare` nat4060225"
      LT
      (nat1 `compare` nat4060225)
  , TestCase $ assertEqual
      "nat1500 `compare` nat123"
      GT
      (nat1500 `compare` nat123)
  , TestCase $ assertEqual
      "natMaxMinusOne `compare` natMax"
      LT
      (natMaxMinusOne `compare` natMax)
  , TestCase $ assertEqual
      "natMax `compare` natMax"
      EQ
      (natMax `compare` natMax)
  , TestCase $ assertEqual
      "natMax `compare` nat0"
      GT
      (natMax `compare` nat0)
  ]

testShow  = TestLabel "Show" $ TestList
  [ TestCase $ assertEqual
      "show nat0"
      "\"AAA 000\""
      (show nat0)
  , TestCase $ assertEqual
      "show nat20"
      "\"AAA 020\""
      (show nat20)
  , TestCase $ assertEqual
      "show nat1700"
      "\"AAB 700\""
      (show nat1700)
  , TestCase $ assertEqual
      "show natMax"
      "\"ZZZ 999\""
      (show natMax)
  , TestCase $ assertEqual
      "show natMaxMinusOne"
      "\"ZZZ 998\""
      (show natMaxMinusOne)
  , TestCase $ assertEqual
      "show nat1234500"
      "\"BVM 500\""
      (show nat1234500)
  , TestCase $ assertEqual
      "show nat4060225"
      "\"GAE 225\""
      (show nat4060225)
  ]

testNum  = TestLabel "Num" $ TestList
  [ TestCase $ assertEqual
      "nat0 + nat1"
      nat1
      (nat0 + nat1)
  , TestCase $ assertEqual
      "nat1 + nat0"
      nat1
      (nat1 + nat0)
  , TestCase $ assertEqual
      "nat1337 + nat1337"
      nat2674
      (nat1337 + nat1337)
  , TestCase $ assertEqual
      "nat1860867 + nat4060225"
      nat5921092
      (nat1860867 + nat4060225)
  , TestCase $ assertEqual
      "natMax + nat2"
      natMax
      (natMax + nat2)
  , TestCase $ assertEqual
      "natMax - nat1"
      natMaxMinusOne
      (natMax - nat1)
  , TestCase $ assertEqual
      "nat1 - nat2"
      nat0
      (nat1 - nat2)
  , TestCase $ assertEqual
      "nat16505265 - nat6602106"
      nat9903159
      (nat16505265 - nat6602106)
  , TestCase $ assertEqual
      "nat0 * nat1"
      nat0
      (nat0 * nat1)
  , TestCase $ assertEqual
      "nat1337 * nat0"
      nat0
      (nat1337 * nat0)
  , TestCase $ assertEqual
      "nat0 * nat1337"
      nat0
      (nat0 * nat1337)
  , TestCase $ assertEqual
      "nat1 * nat1337"
      nat1337
      (nat1 * nat1337)
  , TestCase $ assertEqual
      "nat1760 * nat12345"
      natMax
      (nat1760 * nat12345)
  , TestCase $ assertEqual
      "nat1337 * nat12345"
      nat16505265
      (nat1337 * nat12345)
  , TestCase $ assertEqual
      "natMaxMinusOne * nat1"
      natMaxMinusOne
      (natMaxMinusOne * nat1)
  , TestCase $ assertEqual
      "negate natMax"
      nat0
      (negate natMax)
  , TestCase $ assertEqual
      "negate natMaxMinusOne"
      nat0
      (negate natMaxMinusOne)
  , TestCase $ assertEqual
      "negate nat2015"
      nat0
      (negate nat2015)
  , TestCase $ assertEqual
      "negate nat0"
      nat0
      (negate nat0)
  , TestCase $ assertEqual
      "abs nat0"
      nat0
      (abs nat0)
  , TestCase $ assertEqual
      "abs nat5921092"
      nat5921092
      (abs nat5921092)
  , TestCase $ assertEqual
      "abs nat1999"
      nat1999
      (abs nat1999)
  , TestCase $ assertEqual
      "abs natMax"
      natMax
      (abs natMax)
  , TestCase $ assertEqual
      "signum nat0"
      0
      (signum nat0)
  , TestCase $ assertEqual
      "signum nat1"
      1
      (signum nat1)
  , TestCase $ assertEqual
      "signum nat1860867"
      1
      (signum nat1860867)
  , TestCase $ assertEqual
      "signum natMax"
      1
      (signum natMax)
  , TestCase $ assertEqual
      "fromInteger 17575999"
      natMax
      (fromInteger 17575999)
  , TestCase $ assertEqual
      "fromInteger 17576000"
      natMax
      (fromInteger 17576000)
  , TestCase $ assertEqual
      "fromInteger 17575998"
      natMaxMinusOne
      (fromInteger 17575998)
  , TestCase $ assertEqual
      "fromInteger 4060225"
      nat4060225
      (fromInteger 4060225)
  , TestCase $ assertEqual
      "fromInteger 0"
      nat0
      (fromInteger 0)
  , TestCase $ assertEqual
      "fromInteger 1"
      nat1
      (fromInteger 1)
  , TestCase $ assertEqual
      "fromInteger 1246845"
      nat1246845
      (fromInteger 1246845)
  , TestCase $ assertEqual
      "fromInteger 16505265"
      nat16505265
      (fromInteger 16505265)
  , TestCase $ assertEqual
      "fromInteger 12345678910"
      natMax
      (fromInteger 12345678910)
  , TestCase $ assertEqual
      "fromInteger (-1)"
      nat0
      (fromInteger (-1))
  , TestCase $ assertEqual
      "fromInteger (-16505265)"
      nat0
      (fromInteger (-16505265))
  , TestCase $ assertEqual
      "fromInteger (-0)"
      nat0
      (fromInteger (-0))
  ]

-- 2 ---------------------------------------------------------------------------

testIsCanPR = TestLabel "isCanPR" $ TestList
  [ TestCase $ assertEqual
      "isCanPR (nat2, nat1)"
      True
      (isCanPR (nat2, nat1))
  , TestCase $ assertEqual
      "isCanPR (nat1, nat0)"
      False
      (isCanPR (nat1, nat0))
  , TestCase $ assertEqual
      "isCanPR (nat16505265, nat0)"
      False
      (isCanPR (nat16505265, nat0))
  , TestCase $ assertEqual
      "isCanPR (nat6, nat2)"
      False
      (isCanPR (nat6, nat2))
  , TestCase $ assertEqual
      "isCanPR (nat2, nat6)"
      False
      (isCanPR (nat2, nat6))
  , TestCase $ assertEqual
      "isCanPR (nat5921092, nat2)"
      False
      (isCanPR (nat5921092, nat2))
  , TestCase $ assertEqual
      "isCanPR (nat5, nat16505265)"
      False
      (isCanPR (nat5, nat16505265))
  , TestCase $ assertEqual
      "isCanPR (nat200, nat1234500)"
      False
      (isCanPR (nat200, nat1234500))
  , TestCase $ assertEqual
      "isCanPR (nat2, nat12345)"
      True
      (isCanPR (nat2, nat12345))
  , TestCase $ assertEqual
      "isCanPR (nat0, nat12345)"
      False
      (isCanPR (nat0, nat12345))
  , TestCase $ assertEqual
      "isCanPR (nat0, nat1)"
      True
      (isCanPR (nat0, nat1))
  ]

testMkCanPR = TestLabel "mkCanPR" $ TestList
  [ TestCase $ assertEqual
      "mkCanPR (nat2, nat1)"
      (nat2, nat1)
      (mkCanPR (nat2, nat1))
  , TestCase $ assertEqual
      "mkCanPR (nat1, nat0)"
      posRatUndefined
      (mkCanPR (nat1, nat0))
  , TestCase $ assertEqual
      "mkCanPR (nat16505265, nat0)"
      posRatUndefined
      (mkCanPR (nat16505265, nat0))
  , TestCase $ assertEqual
      "mkCanPR (nat6, nat2)"
      (nat3, nat1)
      (mkCanPR (nat6, nat2))
  , TestCase $ assertEqual
      "mkCanPR (nat2, nat6)"
      (nat1, nat3)
      (mkCanPR (nat2, nat6))
  , TestCase $ assertEqual
      "mkCanPR (nat5921092, nat2)"
      (nat2960546, nat1)
      (mkCanPR (nat5921092, nat2))
  , TestCase $ assertEqual
      "mkCanPR (nat5, nat16505265)"
      (nat1, nat3301053)
      (mkCanPR (nat5, nat16505265))
  , TestCase $ assertEqual
      "mkCanPR (nat200, nat1234500)"
      (nat2, nat12345)
      (mkCanPR (nat200, nat1234500))
  , TestCase $ assertEqual
      "mkCanPR (nat2, nat12345)"
      (nat2, nat12345)
      (mkCanPR (nat2, nat12345))
  , TestCase $ assertEqual
      "mkCanPR (nat0, nat12345)"
      (nat0, nat1)
      (mkCanPR (nat0, nat12345))
  , TestCase $ assertEqual
      "mkCanPR (nat0, nat1)"
      (nat0, nat1)
      (mkCanPR (nat0, nat1))
  , TestCase $ assertEqual
      "mkCanPR (nat1800, nat12345)"
      (nat120, nat823)
      (mkCanPR (nat1800, nat12345))
  ]

testPlusPR = TestLabel "plusPR" $ TestList
  [ TestCase $ assertEqual
      "(nat2, nat1) `plusPR` (nat3, nat4)"
      (nat11, nat4)
      ((nat2, nat1) `plusPR` (nat3, nat4))
  , TestCase $ assertEqual
      "(nat2, nat0) `plusPR` (nat3, nat4)"
      posRatUndefined
      ((nat2, nat0) `plusPR` (nat3, nat4))
  , TestCase $ assertEqual
      "(nat2960546, nat5921092) `plusPR` (nat4060225, nat0)"
      posRatUndefined
      ((nat2960546, nat5921092) `plusPR` (nat4060225, nat0))
  , TestCase $ assertEqual
      "(nat2960546, nat5921092) `plusPR` (nat4060225, nat3301053)"
      (nat11421503, nat6602106)
      ((nat2960546, nat5921092) `plusPR` (nat4060225, nat3301053))
  , TestCase $ assertEqual
      "(natMaxMinusOne, natMaxMinusOne) `plusPR` (natMax, natMax)"
      (nat2, nat1)
      ((natMaxMinusOne, natMaxMinusOne) `plusPR` (natMax, natMax))
  , TestCase $ assertEqual
      "(natMaxMinusOne, natMax) `plusPR` (nat0, natMax)"
      (natMaxMinusOne, natMax)
      ((natMaxMinusOne, natMax) `plusPR` (nat0, natMax))
  ]

testMinusPR = TestLabel "minusPR" $ TestList
  [ TestCase $ assertEqual
      "(nat2, nat1) `minusPR` (nat3, nat4)"
      (nat5, nat4)
      ((nat2, nat1) `minusPR` (nat3, nat4))
  , TestCase $ assertEqual
      "(nat2, nat0) `minusPR` (nat3, nat4)"
      posRatUndefined
      ((nat2, nat0) `minusPR` (nat3, nat4))
  , TestCase $ assertEqual
      "(nat2960546, nat5921092) `minusPR` (nat4060225, nat0)"
      posRatUndefined
      ((nat2960546, nat5921092) `minusPR` (nat4060225, nat0))
  , TestCase $ assertEqual
      "(nat1, nat2) `minusPR` (nat2, nat2)"
      (nat0, nat1)
      ((nat1, nat2) `minusPR` (nat2, nat2))
  , TestCase $ assertEqual
      "(nat2960546, nat5921092) `minusPR` (nat4060225, nat3301053)"
      (nat0, nat1)
      ((nat2960546, nat5921092) `minusPR` (nat4060225, nat3301053))
  , TestCase $ assertEqual
      "(natMaxMinusOne, natMaxMinusOne) `minusPR` (natMax, natMax)"
      (nat0, nat1)
      ((natMaxMinusOne, natMaxMinusOne) `minusPR` (natMax, natMax))
  , TestCase $ assertEqual
      "(natMaxMinusOne, natMax) `minusPR` (natMax, natMaxMinusOne)"
      (nat0, nat1)
      ((natMaxMinusOne, natMax) `minusPR` (natMax, natMaxMinusOne))
  , TestCase $ assertEqual
      "(natMaxMinusOne, natMax) `minusPR` (nat0, natMax)"
      (natMaxMinusOne, natMax)
      ((natMaxMinusOne, natMax) `minusPR` (nat0, natMax))
  , TestCase $ assertEqual
      "(natMax, nat1337) `minusPR` (natMaxMinusOne, nat1337)"
      (nat1, nat1337)
      ((natMax, nat1337) `minusPR` (natMaxMinusOne, nat1337))
  ]

testTimesPR = TestLabel "timesPR" $ TestList
  [ TestCase $ assertEqual
      "(nat2, nat1) `timesPR` (nat3, nat4)"
      (nat3, nat2)
      ((nat2, nat1) `timesPR` (nat3, nat4))
  , TestCase $ assertEqual
      "(nat2, nat0) `timesPR` (nat3, nat4)"
      posRatUndefined
      ((nat2, nat0) `timesPR` (nat3, nat4))
  , TestCase $ assertEqual
      "(nat2960546, nat5921092) `timesPR` (nat4060225, nat0)"
      posRatUndefined
      ((nat2960546, nat5921092) `timesPR` (nat4060225, nat0))
  , TestCase $ assertEqual
      "(nat2960546, nat5921092) `timesPR` (nat4060225, nat3301053)"
      (nat4060225, nat6602106)
      ((nat2960546, nat5921092) `timesPR` (nat4060225, nat3301053))
  , TestCase $ assertEqual
      "(natMaxMinusOne, natMaxMinusOne) `timesPR` (natMax, natMax)"
      (nat1, nat1)
      ((natMaxMinusOne, natMaxMinusOne) `timesPR` (natMax, natMax))
  , TestCase $ assertEqual
      "(natMaxMinusOne, natMax) `timesPR` (natMax, natMaxMinusOne)"
      (nat1, nat1)
      ((natMaxMinusOne, natMax) `timesPR` (natMax, natMaxMinusOne))
  , TestCase $ assertEqual
      "(natMaxMinusOne, natMax) `timesPR` (nat0, natMax)"
      (nat0, nat1)
      ((natMaxMinusOne, natMax) `timesPR` (nat0, natMax))
  , TestCase $ assertEqual
      "(natMax, nat1337) `timesPR` (natMaxMinusOne, nat1337)"
      (natMax, nat1)
      ((natMax, nat1337) `timesPR` (natMaxMinusOne, nat1337))
  ]

testDivPR = TestLabel "divPR" $ TestList
  [ TestCase $ assertEqual
      "(nat2, nat1) `divPR` (nat3, nat4)"
      (nat8, nat3)
      ((nat2, nat1) `divPR` (nat3, nat4))
  , TestCase $ assertEqual
      "(nat2, nat0) `divPR` (nat3, nat4)"
      posRatUndefined
      ((nat2, nat0) `divPR` (nat3, nat4))
  , TestCase $ assertEqual
      "(nat2960546, nat5921092) `divPR` (nat4060225, nat0)"
      posRatUndefined
      ((nat2960546, nat5921092) `divPR` (nat4060225, nat0))
  , TestCase $ assertEqual
      "(nat1, nat2) `divPR` (nat0, nat3)"
      posRatUndefined
      ((nat1, nat2) `divPR` (nat0, nat3))
  , TestCase $ assertEqual
      "(nat2960546, nat5921092) `divPR` (nat4060225, nat3301053)"
      (nat3301053, nat8120450)
      ((nat2960546, nat5921092) `divPR` (nat4060225, nat3301053))
  , TestCase $ assertEqual
      "(natMaxMinusOne, natMaxMinusOne) `divPR` (natMax, natMax)"
      (nat1, nat1)
      ((natMaxMinusOne, natMaxMinusOne) `divPR` (natMax, natMax))
  , TestCase $ assertEqual
      "(natMax, nat1337) `divPR` (natMaxMinusOne, nat1337)"
      (natMax, natMaxMinusOne)
      ((natMax, nat1337) `divPR` (natMaxMinusOne, nat1337))
  ]

testEqPR = TestLabel "eqPR" $ TestList
  [ TestCase $ assertEqual
      "(nat2, nat1) `eqPR` (nat3, nat4)"
      False
      ((nat2, nat1) `eqPR` (nat3, nat4))
  , TestCase $ assertEqual
      "(nat2, nat0) `eqPR` (nat2, nat1)"
      False
      ((nat2, nat0) `eqPR` (nat2, nat1))
  , TestCase $ assertEqual
      "(nat1, nat1) `eqPR` (nat1, nat0)"
      False
      ((nat1, nat1) `eqPR` (nat1, nat0))
  , TestCase $ assertEqual
      "(nat0, nat0) `eqPR` (nat0, nat0)"
      False
      ((nat0, nat0) `eqPR` (nat0, nat0))
  , TestCase $ assertEqual
      "(nat2960546, nat3301053) `eqPR` (nat2960546, nat3301053)"
      True
      ((nat2960546, nat3301053) `eqPR` (nat2960546, nat3301053))
  , TestCase $ assertEqual
      "(nat2, nat1) `eqPR` (nat8, nat4)"
      True
      ((nat2, nat1) `eqPR` (nat8, nat4))
  ]

testNeqPR = TestLabel "neqPR" $ TestList
  [ TestCase $ assertEqual
      "(nat2, nat1) `neqPR` (nat3, nat4)"
      True
      ((nat2, nat1) `neqPR` (nat3, nat4))
  , TestCase $ assertEqual
      "(nat2, nat0) `neqPR` (nat2, nat1)"
      False
      ((nat2, nat0) `neqPR` (nat2, nat1))
  , TestCase $ assertEqual
      "(nat1, nat1) `neqPR` (nat1, nat0)"
      False
      ((nat1, nat1) `neqPR` (nat1, nat0))
  , TestCase $ assertEqual
      "(nat0, nat0) `neqPR` (nat0, nat0)"
      False
      ((nat0, nat0) `neqPR` (nat0, nat0))
  , TestCase $ assertEqual
      "(nat2960546, nat3301053) `neqPR` (nat2960546, nat3301053)"
      False
      ((nat2960546, nat3301053) `neqPR` (nat2960546, nat3301053))
  , TestCase $ assertEqual
      "(nat2, nat1) `neqPR` (nat8, nat4)"
      False
      ((nat2, nat1) `neqPR` (nat8, nat4))
  ]

testGrPR = TestLabel "grPR" $ TestList
  [ TestCase $ assertEqual
      "(nat2, nat1) `grPR` (nat3, nat4)"
      True
      ((nat2, nat1) `grPR` (nat3, nat4))
  , TestCase $ assertEqual
      "(nat2, nat0) `grPR` (nat2, nat1)"
      False
      ((nat2, nat0) `grPR` (nat2, nat1))
  , TestCase $ assertEqual
      "(nat1, nat1) `grPR` (nat1, nat0)"
      False
      ((nat1, nat1) `grPR` (nat1, nat0))
  , TestCase $ assertEqual
      "(nat0, nat0) `grPR` (nat0, nat0)"
      False
      ((nat0, nat0) `grPR` (nat0, nat0))
  , TestCase $ assertEqual
      "(nat2960546, nat3301053) `grPR` (nat2960546, nat3301053)"
      False
      ((nat2960546, nat3301053) `grPR` (nat2960546, nat3301053))
  , TestCase $ assertEqual
      "(nat2, nat1) `grPR` (nat8, nat4)"
      False
      ((nat2, nat1) `grPR` (nat8, nat4))
  , TestCase $ assertEqual
      "(natMax, nat12345) `grPR` (natMaxMinusOne, nat12345)"
      True
      ((natMax, nat12345) `grPR` (natMaxMinusOne, nat12345))
  , TestCase $ assertEqual
      "(nat1999, nat30) `grPR` (nat2000, nat30)"
      False
      ((nat1999, nat30) `grPR` (nat2000, nat30))
  ]

testLePR = TestLabel "lePR" $ TestList
  [ TestCase $ assertEqual
      "(nat2, nat1) `lePR` (nat3, nat4)"
      False
      ((nat2, nat1) `lePR` (nat3, nat4))
  , TestCase $ assertEqual
      "(nat2, nat0) `lePR` (nat2, nat1)"
      False
      ((nat2, nat0) `lePR` (nat2, nat1))
  , TestCase $ assertEqual
      "(nat1, nat1) `lePR` (nat1, nat0)"
      False
      ((nat1, nat1) `lePR` (nat1, nat0))
  , TestCase $ assertEqual
      "(nat0, nat0) `lePR` (nat0, nat0)"
      False
      ((nat0, nat0) `lePR` (nat0, nat0))
  , TestCase $ assertEqual
      "(nat2960546, nat3301053) `lePR` (nat2960546, nat3301053)"
      False
      ((nat2960546, nat3301053) `lePR` (nat2960546, nat3301053))
  , TestCase $ assertEqual
      "(nat2, nat1) `lePR` (nat8, nat4)"
      False
      ((nat2, nat1) `lePR` (nat8, nat4))
  , TestCase $ assertEqual
      "(natMax, nat12345) `lePR` (natMaxMinusOne, nat12345)"
      False
      ((natMax, nat12345) `lePR` (natMaxMinusOne, nat12345))
  , TestCase $ assertEqual
      "(nat1999, nat30) `lePR` (nat2000, nat30)"
      True
      ((nat1999, nat30) `lePR` (nat2000, nat30))
  , TestCase $ assertEqual
      "(nat2000, nat2001) `lePR` (nat2000, nat2000)"
      True
      ((nat2000, nat2001) `lePR` (nat2000, nat2000))
  ]

testGrEqPR = TestLabel "grEqPR" $ TestList
  [ TestCase $ assertEqual
      "(nat2, nat1) `grEqPR` (nat3, nat4)"
      True
      ((nat2, nat1) `grEqPR` (nat3, nat4))
  , TestCase $ assertEqual
      "(nat2, nat0) `grEqPR` (nat2, nat1)"
      False
      ((nat2, nat0) `grEqPR` (nat2, nat1))
  , TestCase $ assertEqual
      "(nat1, nat1) `grEqPR` (nat1, nat0)"
      False
      ((nat1, nat1) `grEqPR` (nat1, nat0))
  , TestCase $ assertEqual
      "(nat0, nat0) `grEqPR` (nat0, nat0)"
      False
      ((nat0, nat0) `grEqPR` (nat0, nat0))
  , TestCase $ assertEqual
      "(nat2960546, nat3301053) `grEqPR` (nat2960546, nat3301053)"
      True
      ((nat2960546, nat3301053) `grEqPR` (nat2960546, nat3301053))
  , TestCase $ assertEqual
      "(nat2, nat1) `grEqPR` (nat8, nat4)"
      True
      ((nat2, nat1) `grEqPR` (nat8, nat4))
  , TestCase $ assertEqual
      "(natMax, nat12345) `grEqPR` (natMaxMinusOne, nat12345)"
      True
      ((natMax, nat12345) `grEqPR` (natMaxMinusOne, nat12345))
  , TestCase $ assertEqual
      "(nat1999, nat30) `grEqPR` (nat2000, nat30)"
      False
      ((nat1999, nat30) `grEqPR` (nat2000, nat30))
  ]

testLeEqPR = TestLabel "leEqPR" $ TestList
  [ TestCase $ assertEqual
      "(nat2, nat1) `leEqPR` (nat3, nat4)"
      False
      ((nat2, nat1) `leEqPR` (nat3, nat4))
  , TestCase $ assertEqual
      "(nat2, nat0) `leEqPR` (nat2, nat1)"
      False
      ((nat2, nat0) `leEqPR` (nat2, nat1))
  , TestCase $ assertEqual
      "(nat1, nat1) `leEqPR` (nat1, nat0)"
      False
      ((nat1, nat1) `leEqPR` (nat1, nat0))
  , TestCase $ assertEqual
      "(nat0, nat0) `leEqPR` (nat0, nat0)"
      False
      ((nat0, nat0) `leEqPR` (nat0, nat0))
  , TestCase $ assertEqual
      "(nat0, nat1) `leEqPR` (nat0, nat1)"
      True
      ((nat0, nat1) `leEqPR` (nat0, nat1))
  , TestCase $ assertEqual
      "(nat2960546, nat3301053) `leEqPR` (nat2960546, nat3301053)"
      True
      ((nat2960546, nat3301053) `leEqPR` (nat2960546, nat3301053))
  , TestCase $ assertEqual
      "(nat2, nat1) `leEqPR` (nat8, nat4)"
      True
      ((nat2, nat1) `leEqPR` (nat8, nat4))
  , TestCase $ assertEqual
      "(natMax, nat12345) `leEqPR` (natMaxMinusOne, nat12345)"
      False
      ((natMax, nat12345) `leEqPR` (natMaxMinusOne, nat12345))
  , TestCase $ assertEqual
      "(nat1999, nat30) `leEqPR` (nat2000, nat30)"
      True
      ((nat1999, nat30) `leEqPR` (nat2000, nat30))
  ]

testPosRat = TestLabel "PosRat-tests from exercise sheet" $ TestList
  [ TestCase $ assertEqual
        ("isCanPR (Nat ((A, A, A), (Neun, Neun, Acht)),  " ++
         "Nat ((A, A, A), (Null, Null, Sechs)))")
        False
        (isCanPR (Nat ((A, A, A), (Neun, Neun, Acht)),
                  Nat ((A, A, A), (Null, Null, Sechs))))
  , TestCase $ assertEqual
        ("mkCanPR (Nat ((A, A, A), (Null, Null, Acht)),  " ++
         "Nat ((A, A, A), (Null, Null, Sechs))")
        (Nat ((A, A, A), (Null, Null, Vier)),
         Nat ((A, A, A), (Null, Null, Drei)))
        (mkCanPR (Nat ((A, A, A), (Null, Null, Acht)),
                  Nat ((A, A, A), (Null, Null, Sechs))))
  , TestCase $ assertEqual
        ("plusPR (Nat ((A, A, A), (Null, Null, Acht)), " ++
         "Nat ((A, A, A), (Null, Null, Vier))) " ++
         "(Nat ((A, A, A), (Null, Vier, Zwei)), " ++
         "Nat ((A, A, A), (Null, Null, Eins)))")
        (Nat ((A, A, A), (Null, Vier, Vier)),
         Nat ((A, A, A), (Null, Null, Eins)))
        (plusPR (Nat ((A, A, A), (Null, Null, Acht)),
                 Nat ((A, A, A), (Null, Null, Vier)))
                (Nat ((A, A, A), (Null, Vier, Zwei)),
                 Nat ((A, A, A), (Null, Null, Eins))))
  , TestCase $ assertEqual
        ("minusPR (Nat ((A, A, A), (Null, Null, Acht)), " ++
         "Nat ((A, A, A), (Null, Null, Vier))) " ++
         "(Nat ((A, A, A), (Null, Vier, Zwei)), " ++
         "Nat ((A, A, A), (Null, Null, Eins)))")
        (Nat ((A, A, A), (Null, Null, Null)),
         Nat ((A, A, A), (Null, Null, Eins)))
        (minusPR (Nat ((A, A, A), (Null, Null, Acht)),
                  Nat ((A, A, A), (Null, Null, Vier)))
                 (Nat ((A, A, A), (Null, Vier, Zwei)),
                  Nat ((A, A, A), (Null, Null, Eins))))
  , TestCase $ assertEqual
        ("timesPR (Nat ((A, A, A), (Null, Null, Acht)), " ++
         "Nat ((A, A, A), (Null, Null, Null))) " ++
         "(Nat ((A, A, A), (Null, Vier, Zwei)), " ++
         "Nat ((A, A, A), (Null, Null, Eins)))")
        (Nat ((A, A, A), (Null, Null, Null)),
         Nat ((A, A, A), (Null, Null, Null)))
        (timesPR (Nat ((A, A, A), (Null, Null, Acht)),
                  Nat ((A, A, A), (Null, Null, Null)))
                 (Nat ((A, A, A), (Null, Vier, Zwei)),
                  Nat ((A, A, A), (Null, Null, Eins))))
  , TestCase $ assertEqual
        ("eqPR (Nat ((A, A, A), (Null, Null, Acht)), " ++
         "Nat ((A, A, A), (Null, Null, Vier))) " ++
         "(Nat ((A, A, A), (Null, Null, Vier)), " ++
         "Nat ((A, A, A), (Null, Null, Zwei)))")
        True
        (eqPR (Nat ((A, A, A), (Null, Null, Acht)),
               Nat ((A, A, A), (Null, Null, Vier)))
              (Nat ((A, A, A), (Null, Null, Vier)),
               Nat ((A, A, A), (Null, Null, Zwei))))
  , TestCase $ assertEqual
        ("leEqPR (Nat ((A, A, A), (Null, Null, Acht)), " ++
         "Nat ((A, A, A), (Null, Null, Vier))) " ++
         "(Nat ((A, A, A), (Null, Vier, Zwei)), " ++
         "Nat ((A, A, A), (Null, Null, Eins)))")
        True
        (leEqPR (Nat ((A, A, A), (Null, Null, Acht)),
                 Nat ((A, A, A), (Null, Null, Vier)))
                (Nat ((A, A, A), (Null, Vier, Zwei)),
                 Nat ((A, A, A), (Null, Null, Eins))))
