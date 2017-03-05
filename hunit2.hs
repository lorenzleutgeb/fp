import Aufgabe2
import Test.HUnit

main = runTestTT $ TestList [test1, test3]

test1 = TestLabel "Beispiel 2.1" $ ( 
  TestList [
    TestCase $ assertEqual "isSound (0,0,0) True"                                   (True)                              (isSound (0,0,0)), 
    TestCase $ assertEqual "isSound negative PoundSterling False"                   (False)                             (isSound (-1,0,0)),
    TestCase $ assertEqual "isSound negative Shilling False"                        (False)                             (isSound (0,-1,0)),
    TestCase $ assertEqual "isSound negative Pence False"                           (False)                             (isSound (0,0,-1)),
    TestCase $ assertEqual "isSound normalized True"                                (True)                              (isSound (147,11,19)),
    TestCase $ assertEqual "isSound not normalized True"                            (True)                              (isSound (666,66,66)),
    TestCase $ assertEqual "normalize (0,0,0) True"                                 (0,0,0)                             (normalize (0,0,0)), 
    TestCase $ assertEqual "normalize negative PoundSterling return input"          (-1,0,0)                            (normalize (-1,0,0)),
    TestCase $ assertEqual "normalize negative Shilling return input"               (0,-1,0)                            (normalize (0,-1,0)),
    TestCase $ assertEqual "normalize negative Pence return input"                  (0,0,-1)                            (normalize (0,0,-1)),
    TestCase $ assertEqual "normalize 20 Pence"                                     (0,1,0)                             (normalize (0,0,20)),
    TestCase $ assertEqual "normalize 12 Shilling"                                  (1,0,0)                             (normalize (0,12,0)),
    TestCase $ assertEqual "normalize 1000 Pence -> Shilling, PoundSterling"        (4,2,0)                             (normalize (0,0,1000)),
    TestCase $ assertEqual "normalize 1019 Pence -> Pence, Shilling, PoundSterling" (4,2,19)                            (normalize (0,0,1019)),
    TestCase $ assertEqual "normalize 120 Shilling -> PoundSterling"                (10,0,19)                           (normalize (0,120,19)),
    TestCase $ assertEqual "normalize not normalized"                               (671,9,6)                           (normalize (666,66,66)),
    TestCase $ assertEqual "normalize normalized"                                   (147,11,19)                         (normalize (147,11,19)),
    --TestCase $ assertError "add invalid to valid"                                 ("Falsche Eingabe")                 (add (-1,0,0) (0,0,0)),
    --TestCase $ assertError "add valid to invalid"                                 ("Falsche Eingabe")                 (add (0,0,0) (0,-1,0)),
    --TestCase $ assertError "add invalid to invalid"                               ("Falsche Eingabe")                 (add (-1,0,0) (0,0,-1)),
    TestCase $ assertEqual "add 1000 Pence + 1000 pence"                            (8,4,0)                             (add (0,0,1000) (0,0,1000)),
    TestCase $ assertEqual "add 120 Shilling + 120 Shilling"                        (20,0,0)                            (add (0,120,0) (0,120,0)),
    TestCase $ assertEqual "add not normalized + not normalized"                    (1343,6,12)                         (add (666,66,66) (666,66,66)),
    TestCase $ assertEqual "add normalized + normalized"                            (147,11,19)                         (add (100,5,10) (47,6,9)),
    TestCase $ assertEqual "interest invalid amount"                                (0,0,0)                             (interest (-1,0,0) 0 1),
    TestCase $ assertEqual "interest invalid rate"                                  (0,0,0)                             (interest (0,0,0) (-1) 1),
    TestCase $ assertEqual "interest invalid period"                                (0,0,0)                             (interest (0,0,0) 0 0),
    TestCase $ assertEqual "interest"                                               (normalize (0,0,25617))             (interest (0,0,16000) 0.04 12),
    TestCase $ assertEqual "interest"                                               (normalize (0,0,30419))             (interest (0,0,16000) 0.055 12),
    TestCase $ assertEqual "interest"                                               (normalize (0,0,1460))              (interest (0,0,1200) 0.04 5)
  ])  

test3 = TestLabel "Beispiel 2.3" $ ( 
  TestList [
    TestCase $ assertEqual "dist distance < 0"                                      (-1)                                (dist (-1) 1 1 1),
    TestCase $ assertEqual "dist speed1 < 0"                                        (-1)                                (dist 1 (-1) 1 1),
    TestCase $ assertEqual "dist speed2 < 0"                                        (-1)                                (dist 1 1 (-1) 1),
    TestCase $ assertEqual "dist TimeBeforeCollision < 0"                           (-1)                                (dist 1 1 1 (-1)),
    TestCase $ assertEqual "dist timeToCollision < time"                            (-1)                                (dist 10 5 5 2),
    TestCase $ assertEqual "dist distance, timeToCollision == 0"                    (0)                                 (dist 0 99 99 0),
    TestCase $ assertEqual "dist"                                                   (10)                                (dist 10 5 5 1),
    TestCase $ assertEqual "dist precision"                                         (22)                                (dist 44.493827156 11.123456789 11.123456789 1),
    TestCase $ assertEqual "dist precision2"                                        (89)                                (dist 100 11.123456789 11.123456789 4)
  ])
