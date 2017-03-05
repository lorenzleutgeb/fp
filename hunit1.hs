import Aufgabe1
import Test.HUnit


main = runTestTT $ TestList [test1, test2, test3, test4]


test1 = TestLabel "Test wochentag" $ ( 
  TestList [
    TestCase $ assertEqual "wochentag Falsche Argumente NterTagImJahr > 366" ("Falsche Argumente") (wochentag "Montag" True 367), 
    TestCase $ assertEqual "wochentag Falsche Argumente NterTagImJahr > 365" ("Falsche Argumente") (wochentag "Montag" False 366), 
    TestCase $ assertEqual "wochentag Falsche Argumente NterTagImJahr < 0" ("Falsche Argumente") (wochentag "Montag" False (-1)), 
    TestCase $ assertEqual "wochentag Falsche Argumente Kein Wochentag" ("Falsche Argumente") (wochentag "KeinWochenTag" False 1), 
    TestCase $ assertEqual "wochentag Donnerstag, False, 1" ("Donnerstag") (wochentag "Donnerstag" False 1), 
    TestCase $ assertEqual "wochentag Donnerstag, False, 365" ("Donnerstag") (wochentag "Donnerstag" True 365),
    TestCase $ assertEqual "wochentag Samstag, False, 1" ("Samstag") (wochentag "Samstag" False 1), 
    TestCase $ assertEqual "wochentag Freitag, True, 366" ("Samstag") (wochentag "Freitag" True 366)
  ])  


test2 = TestLabel "Test vc" $ ( 
  TestList [
    TestCase $ assertEqual "vc -1 -1" (-1) (vc (-1) (-1)), 
    TestCase $ assertEqual "vc 1 -1" (-1) (vc 1 (-1)), 
    TestCase $ assertEqual "vc 1 1" (1) (vc 1 1), 
    TestCase $ assertEqual "vc 0 0 " (1) (vc 0 0 ),
    TestCase $ assertEqual "vuc (0,0)" (1) (vuc (0,0)),
    TestCase $ assertEqual "vc 50 5" (254251200) (vc 50 5), 
    TestCase $ assertEqual "vuc (50 5)" (vc 50 5) (vuc (50,5))
  ])  


test3 = TestLabel "Test frequencySort" $ ( 
  TestList [
    TestCase $ assertEqual "frequencySort et9Ea9earE9E9" ("rtaaeeEEE9999") (frequencySort "et9Ea9earE9E9"),
    TestCase $ assertEqual "frequencySort empty string" ("") (frequencySort ""),
    TestCase $ assertEqual "frequencySort 111" ("111") (frequencySort "111"), 
    TestCase $ assertEqual "frequencySort 111" ("112233AADDFFSSaaddffss") (frequencySort "asdfASDFasdfASDF123321") 
  ])  


test4 = TestLabel "Test pcheck" $ ( 
  TestList [
    TestCase $ assertEqual "pcheck 10 (octal: 12)" (False) (pcheck 10),
    TestCase $ assertEqual "pcheck 9 (octal: 11)" (True) (pcheck 9), 
    TestCase $ assertEqual "pcheck 148644 (octal: 442244)" (True) (pcheck 148644)
  ])
