
import Aufgabe3
import Test.HUnit


main = runTestTT $ TestList [test2, test4 ]

test2 = TestLabel "Beispiel 3.2" $ ( 
  TestList [
    TestCase $ assertEqual "pwv empty"                                                    (0)                                 (pwv []),
    TestCase $ assertEqual "pwv 0x max 2xae.."                                            (0)                                 (pwv ["Test", "a", "bb", "aa", "e", "ff", "e", "a", "X", "I", "AEIOUAEIOUAEIOU"]),
    TestCase $ assertEqual "pwv 0x max 3xae.."                                            (1)                                 (pwv ["Test", "a", "bb", "aaa", "aaa", "e", "ff", "e", "a", "X", "I", "AEIOUAEIOUAEIOU"]),
    TestCase $ assertEqual "pwv 1x max 4xae.."                                            (1)                                 (pwv ["Test", "a", "bb", "aa", "e", "ff", "eeee", "a", "X", "I", "AEIOUAEIOUAEIOU", "eeee"]),
    TestCase $ assertEqual "pwv 2x max 4xae.."   (2)                                 (pwv ["Test", "a", "bb", "aaaa", "e", "ff", "eeee", "a", "X", "I", "AEIOUAEIOUAEIOU", "aaaa","eeee"]),
    TestCase $ assertEqual "pwv 1x only a valid" (1)                                 (pwv ["Test", "a", "bb", "aaa", "e", "ff", "e", "a"]),
    TestCase $ assertEqual "pwv 1x only e valid" (1)                                 (pwv ["Test", "a", "bb", "aa", "e", "ff", "eee", "a"]),
    TestCase $ assertEqual "pwv 1x only i valid" (1)                                 (pwv ["Test", "a", "bb", "aa", "e", "ff", "e", "a", "iii"]),
    TestCase $ assertEqual "pwv 1x only o valid" (1)                                 (pwv ["Test", "a", "bb", "aa", "e", "ff", "e", "a", "ooo"]),
    TestCase $ assertEqual "pwv 1x only u valid" (1)                                 (pwv ["Test", "a", "bb", "aa", "e", "ff", "e", "a", "uuu"]),
    TestCase $ assertEqual "pwv 1x alle mix-variationen" (25)                                (pwv ["Test", "a", "bb", "aa", "e", "ff", "e", "a", "aei","eia","aie","aeo","eoa","aoe","aeu","uea","aue","aio","ioa","aio","aiu","uia","aiu","aou","uoa","aou","eio","eio","ioe","eiu","uei","iue","eou","ueo","oue","iou","uio","iou"])
  ])

test4 = TestLabel "Beispiel 3.4" $ ( 
  TestList [
    TestCase $ assertEqual "nf invalid character"                                            ((('A','A','a'),(0,0,0)))           (nf (('A','A','a'),(0,0,0))),
    TestCase $ assertEqual "nf invalid number"                                              ((('A','A','A'),(0,0,20)))          (nf (('A','A','A'),(0,0,20))),
    TestCase $ assertEqual "vg invalid character"                                           ((('A','A','a'),(0,0,0)))           (nf (('A','A','a'),(0,0,0))),
    TestCase $ assertEqual "vg invalid number"                                             ((('A','A','A'),(0,0,20)))          (nf (('A','A','A'),(0,0,20))),
    TestCase $ assertEqual "nf normal"                                                        ((('A','A','A'),(0,0,1)))           (nf (('A','A','A'),(0,0,0))),
    TestCase $ assertEqual "nf overflow number 1"                                        ((('A','A','A'),(0,1,0)))           (nf (('A','A','A'),(0,0,9))),
    TestCase $ assertEqual "nf overflow number 2"                                        ((('A','A','A'),(1,0,0)))           (nf (('A','A','A'),(0,9,9))),
    TestCase $ assertEqual "nf overflow number 3"                                        ((('A','A','B'),(0,0,0)))           (nf (('A','A','A'),(9,9,9))),
    TestCase $ assertEqual "nf overflow character 1"                                      ((('A','B','A'),(0,0,0)))           (nf (('A','A','Z'),(9,9,9))),
    TestCase $ assertEqual "nf overflow character 2"                                      ((('B','A','A'),(0,0,0)))           (nf (('A','Z','Z'),(9,9,9))),
    TestCase $ assertEqual "nf overflow character 3"                                      ((('A','A','A'),(0,0,0)))           (nf (('Z','Z','Z'),(9,9,9))),
    TestCase $ assertEqual "vg normal"                                                        ((('Z','Z','Z'),(9,9,8)))           (vg (('Z','Z','Z'),(9,9,9))),
    TestCase $ assertEqual "vg overflow number 1"                                        ((('Z','Z','Z'),(9,8,9)))           (vg (('Z','Z','Z'),(9,9,0))),
    TestCase $ assertEqual "vg overflow number 2"                                        ((('Z','Z','Z'),(8,9,9)))           (vg (('Z','Z','Z'),(9,0,0))),
    TestCase $ assertEqual "vg overflow number 3"                                        ((('Z','Z','Y'),(9,9,9)))           (vg (('Z','Z','Z'),(0,0,0))),
    TestCase $ assertEqual "vg overflow character 1"                                      ((('Z','Y','Z'),(9,9,9)))           (vg (('Z','Z','A'),(0,0,0))),
    TestCase $ assertEqual "vg overflow character 2"                                      ((('Y','Z','Z'),(9,9,9)))           (vg (('Z','A','A'),(0,0,0))),
    TestCase $ assertEqual "vg overflow character 3"                                      ((('Z','Z','Z'),(9,9,9)))           (vg (('A','A','A'),(0,0,0)))
  ])

