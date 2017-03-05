-- Imports ---------------------------------------------------------------------

import Aufgabe6 (
                  Nat(..), Ziffer(..), Zeichen(..)
                , Tree(..), smt
                , STree(..), t2st
                , tsum, tdepth
                , Graph(..), ne, independent, cover
                )
import Test.HUnit

-- Data ------------------------------------------------------------------------

nat0        = Nat ((A, A, A), (Null, Null, Null))
nat1        = Nat ((A, A, A), (Null, Null, Eins))
nat849      = Nat ((A, A, A), (Acht, Vier, Neun))
nat1337     = Nat ((A, A, B), (Drei, Drei, Sieben))
nat8542     = Nat ((A, A, I), (Fuenf, Vier, Zwei))
nat8120450  = Nat ((M, A, I), (Vier, Fuenf, Null))
nat1860867  = Nat ((C, T, O), (Acht, Sechs, Sieben))
nat11421503 = Nat ((Q, X, H), (Fuenf, Null, Drei))
nat16505265 = Nat ((Y, K, V), (Zwei, Sechs, Fuenf))
natMax      = Nat ((Z, Z, Z), (Neun, Neun, Neun))

numMax = 17575999

tree1 = Node (+12) (<10) [1..100] Nil Nil Nil
tree2 = Node (*3) (>=5) [-1..81] Nil Nil Nil
tree3 = Node (^2) (<(-5)) [-10..0] Nil Nil Nil
tree4 = Node (+(-3)) (>2) [-1,-3,-4] tree1 Nil Nil
tree5 = Node (`div` 2) (==2) [0,2,4] Nil tree2 tree3
tree6 = Node (`mod` 3) (/=3) [-20, 3, 4, 5, 6, 9, 18] tree4 Nil tree5

tree1SmtSimple = SNode [13..21] SNil SNil SNil
tree2SmtSimple = SNode (map (*3) [5..81]) SNil SNil SNil
tree3SmtSimple = SNode (map (^2) [-10..(-6)]) SNil SNil SNil
tree4SmtSimple = SNode [] tree1SmtSimple SNil SNil
tree5SmtSimple = SNode [1] SNil tree2SmtSimple tree3SmtSimple
tree6SmtSimple = SNode [1, 1, 2, 0, 0, 0] tree4SmtSimple SNil tree5SmtSimple
tree7SmtSimple = SNode [] tree6SmtSimple SNil tree4SmtSimple

tree1Sum = sum [13..21]
tree2Sum = sum (map (*3) [5..81])
tree3Sum = sum (map (^2) [-10..(-6)])
tree4Sum = tree1Sum
tree5Sum = sum [1] + tree2Sum + tree3Sum
tree6Sum = sum [1, 1, 2, 0, 0, 0] + tree4Sum + tree5Sum

graph1 = Gr [
              (1,  [2, 7])
            , (2,  [3])
            , (3,  [2, 8])
            , (4,  [5, 9])
            , (5,  [6])
            , (6,  [5, 11])
            , (7,  [1, 14])
            , (8,  [9, 16])
            , (9,  [10])
            , (10, [9, 11])
            , (11, [19, 10, 12, 6])
            , (12, [13, 20])
            , (13, [21])
            , (14, [7, 15])
            , (15, [16])
            , (16, [15, 17, 8])
            , (17, [18])
            , (18, [19])
            , (19, [22, 11])
            , (20, [23])
            , (21, [24, 13])
            , (22, [23])
            , (24, [23])
            ]

edges1 = [
           (1, 2), (1,7)
         , (2, 3)
         , (3, 8)
         , (4, 5), (4, 9)
         , (5, 6)
         , (6, 11)
         , (7, 14)
         , (8, 9), (8, 16)
         , (9, 10)
         , (10, 11)
         , (11, 12), (11, 19)
         , (12, 13), (12, 20)
         , (13, 21)
         , (14, 15)
         , (15, 16)
         , (16, 17)
         , (17, 18)
         , (18, 19)
         , (19, 22)
         , (20, 23)
         , (21, 24)
         , (22, 23)
         , (23, 24)
         ]

nodes1 = [1..24]

-- Main ------------------------------------------------------------------------

main = runTestTT $ TestList [
    testBounded
    , testEnum
    , testReal
    , testIntegral
    , testSmt
    , testTsum
    , testTdepth
--  , testNe
  , testIndependent
--  , testCover
  ]

-- 1 ---------------------------------------------------------------------------

testBounded = TestLabel "Bounded" $ TestList [
    TestCase $ assertEqual "maxBound :: Nat" natMax (maxBound :: Nat)
  , TestCase $ assertEqual "minBound :: Nat" nat0 (minBound :: Nat)
  ]

testEnum = TestLabel "Enum" $ TestList [
    TestCase $ assertEqual "toEnum 11421503" nat11421503 (toEnum 11421503)
  , TestCase $ assertEqual "toEnum (-123)" nat0 (toEnum (-123))
  , TestCase $ assertEqual "toEnum . fromInteger $ numMax" natMax
    (toEnum . fromInteger $ numMax)
  , TestCase $ assertEqual "toEnum . fromInteger $ (numMax+1))" natMax
    (toEnum . fromInteger $ (numMax+1))
  , TestCase $ assertEqual "fromEnum natMax" (fromInteger numMax)
    (fromEnum natMax)
  , TestCase $ assertEqual "fromEnum nat16505265" 16505265
    (fromEnum nat16505265)
  , TestCase $ assertEqual "fromEnum nat1" 1 (fromEnum nat1)
  ]

testReal = TestLabel "Real" $ TestList [
    TestCase $ assertEqual "toRational nat1337" (toRational 1337)
      (toRational nat1337)
  , TestCase $ assertEqual "toRational nat1860867" (toRational 1860867)
      (toRational nat1860867)
  , TestCase $ assertEqual "toRational natMax" (toRational numMax)
      (toRational natMax)
  ]

testIntegral = TestLabel "Integral" $ TestList [
    TestCase $ assertEqual "quotRem nat1 nat1" (nat1, nat0) (quotRem nat1 nat1)
  , TestCase $ assertEqual "quotRem nat11421503 nat1337" (nat8542, nat849)
      (quotRem nat11421503 nat1337)
  , TestCase $ assertEqual "quotRem nat8542 nat8120450" (nat0, nat8542)
      (quotRem nat8542 nat8120450)
  , TestCase $ assertEqual "toInteger nat8542" 8542 (toInteger nat8542)
  , TestCase $ assertEqual "toInteger nat0" 0 (toInteger nat0)
  , TestCase $ assertEqual "toInteger natMax"
    (fromIntegral numMax) (toInteger natMax)
  , TestCase $ assertEqual "toInteger nat11421503" 11421503
    (toInteger nat11421503)
  ]

-- 2 ---------------------------------------------------------------------------

testSmt = TestLabel "smt" $ TestList [
    TestCase $ assertEqual "t2st . smt $ tree1" tree1SmtSimple
      (t2st . smt $ tree1)
  , TestCase $ assertEqual "t2st . smt $ tree6" tree6SmtSimple
      (t2st . smt $ tree6)
  ]

-- 3 ---------------------------------------------------------------------------

testTsum = TestLabel "tsum" $ TestList [
    TestCase $ assertEqual "tsum tree1SmtSimple" tree1Sum (tsum tree1SmtSimple)
  , TestCase $ assertEqual "tsum tree6SmtSimple" tree6Sum (tsum tree6SmtSimple)
  ]

testTdepth = TestLabel "tdepth" $ TestList [
    TestCase $ assertEqual "tdepth SNil" 0 (tdepth SNil)
  , TestCase $ assertEqual "tdepth tree1SmtSimple" 1 (tdepth tree1SmtSimple)
  , TestCase $ assertEqual "tdepth tree1SmtSimpl4" 2 (tdepth tree4SmtSimple)
  , TestCase $ assertEqual "tdepth tree1SmtSimpl6" 3 (tdepth tree6SmtSimple)
  , TestCase $ assertEqual "tdepth tree1SmtSimpl7" 4 (tdepth tree7SmtSimple)
  ]

-- 4 ---------------------------------------------------------------------------

testNe = TestLabel "ne" $ TestList [
    TestCase $ assertEqual "ne graph1" (nodes1, edges1) (ne graph1)
  ]

testIndependent = TestLabel "independent" $ TestList [
    TestCase $ assertEqual "independent graph1 [1, 2]" False
    (independent graph1 [1, 2])
  , TestCase $ assertEqual
    "independent graph1 [1, 3, 5, 9, 11, 13, 14, 16, 18, 20, 22, 24]" True
    (independent graph1 [1, 3, 5, 9, 11, 13, 14, 16, 18, 20, 22, 24])
  , TestCase $ assertEqual
    "independent graph1 [1, 3, 5, 9, 11, 13, 14, 16, 18, 20, 22, 23, 24]" False
    (independent graph1 [1, 3, 5, 9, 11, 13, 14, 16, 18, 20, 22, 23, 24])
  , TestCase $ assertEqual
    "independent graph1 [2, 4, 6, 7, 8, 10, 12, 15, 17, 19, 21, 23]" True
    (independent graph1 [2, 4, 6, 7, 8, 10, 12, 15, 17, 19, 21, 23])
  , TestCase $ assertEqual
    "independent graph1 [2, 4, 6, 7, 14, 8, 10, 12, 15, 17, 19, 21, 23]" False
    (independent graph1 [2, 4, 6, 7, 14, 8, 10, 12, 15, 17, 19, 21, 23])
  , TestCase $ assertEqual
    "independent graph1 [14, 8, 10, 12, 15, 19, 23, 23, 23, 8, 25, (-1)]" False
    (independent graph1 [14, 8, 10, 12, 15, 19, 23, 23, 23, 8, 25, (-1)])
  , TestCase $ assertEqual
    "independent graph1 [14, 8, 10, 12, 19, 23, 23, 23, 8, 25, (-1)]" False
    (independent graph1 [14, 8, 10, 12, 19, 23, 23, 23, 8, 25, (-1)])
  , TestCase $ assertEqual
    "independent graph1 [14, 8, 10, 12, 19, 23, 23, 23, 8]" True
    (independent graph1 [14, 8, 10, 12, 19, 23, 23, 23, 8])
  ]

testCover = TestLabel "cover" $ TestList [
    TestCase $ assertEqual "cover graph1 [1..23]" True (cover graph1 [1..23])
  , TestCase $ assertEqual "cover graph1 [-23..1]" False (cover graph1 [-23..1])
  , TestCase $ assertEqual
    "cover graph1 [1, 3, 5, 9, 11, 13, 14, 16, 18, 20, 22, 24]" True
    (cover graph1 [1, 3, 5, 9, 11, 13, 14, 16, 18, 20, 22, 24])
  , TestCase $ assertEqual
    "cover graph1 [1, 3, 5, 9, 11, 13, 14, 18, 20, 22, 24]" False
    (cover graph1 [1, 3, 5, 9, 11, 13, 14, 18, 20, 22, 24])
  , TestCase $ assertEqual
    "cover graph1 [2, 4, 6, 7, 8, 10, 12, 15, 17, 19, 21, 23]" True
    (cover graph1 [2, 4, 6, 7, 8, 10, 12, 15, 17, 19, 21, 23])
  , TestCase $ assertEqual
    "cover graph1 [2, 4, 6, 7, 8, 12, 15, 17, 19, 21, 23]" False
    (cover graph1 [2, 4, 6, 7, 8, 12, 15, 17, 19, 21, 23])
  ]
