module Tests.WSEdit.Buffer
    ( testWSEditBuffer
    ) where

import Tests.Base

import qualified WSEdit.Buffer as B



stallman :: B.Buffer Char
stallman = B.Buffer { B.prefix  = "tS"
                    , B.prefLen = 2
                    , B.curr    = 'a'
                    , B.suffix  = "llman"
                    , B.sufLen  = 5
                    }



tSingleton :: Test
tSingleton = TestLabel "singleton"
    $ TestCase $ assertPretty "singleton"
        ( B.Buffer { B.prefix  = []
                   , B.prefLen = 0
                   , B.curr    = "Stallman"
                   , B.suffix  = []
                   , B.sufLen  = 0
                   }
        ) $ B.singleton "Stallman"



tFromList :: Test
tFromList = TestLabel "fromList" $ TestList
    [ TestCase
        $ assertPretty "fromList: []" Nothing
        $ B.fromList ""

    , TestCase
        $ assertPretty "fromList: singleton" (Just $ B.singleton "Stallman")
        $ B.fromList ["Stallman"]

    , TestCase
        $ assertPretty "fromList: nontrivial list"
            (Just $ B.Buffer { B.prefix  = []
                             , B.prefLen = 0
                             , B.curr    = 'S'
                             , B.suffix  = "tallman"
                             , B.sufLen  = 7
                             }
            ) $ B.fromList "Stallman"
    ]



tToList :: Test
tToList = TestLabel "toList" $ TestList
    [ TestCase
        $ assertPretty "toList: singleton" "b"
        $ B.toList $ B.singleton 'b'

    , TestCase
        $ assertPretty "toList: nontrivial buffer" "Stallman"
        $ B.toList stallman
    ]



tLength :: Test
tLength = TestLabel "length"
    $ TestCase $ assertPretty "length" 8
    $ B.length stallman



tSub :: Test
tSub = TestLabel "sub" $ TestList
    [ TestCase
        $ assertPretty "sub: prefix only" "t"
        $ B.sub 1 1 stallman

    , TestCase
        $ assertPretty "sub: suffix only" "ma"
        $ B.sub 5 6 stallman

    , TestCase
        $ assertPretty "sub: identity" "Stallman"
        $ B.sub 0 7 stallman
    ]



tLeft :: Test
tLeft = TestLabel "left" $ TestList
    [ TestCase
        $ assertPretty "left: Nothing" Nothing
        $ B.left $ B.singleton "Stallman"

    , TestCase
        $ assertPretty "left: nontrivial buffer" (Just 't')
        $ B.left stallman
    ]



tRight :: Test
tRight = TestLabel "right" $ TestList
    [ TestCase
        $ assertPretty "right: Nothing" Nothing
        $ B.right $ B.singleton "Stallman"

    , TestCase
        $ assertPretty "right: nontrivial buffer" (Just 'l')
        $ B.right stallman
    ]



tAtMay :: Test
tAtMay = TestLabel "atMay" $ TestList
    [ TestCase
        $ assertPretty "atMay: out of bounds left"        Nothing
        $ B.atMay stallman (-1)

    , TestCase
        $ assertPretty "atMay: leftmost prefix element"  (Just 'S')
        $ B.atMay stallman   0

    , TestCase
        $ assertPretty "atMay: rightmost prefix element" (Just 't')
        $ B.atMay stallman   1

    , TestCase
        $ assertPretty "atMay: current element"          (Just 'a')
        $ B.atMay stallman   2

    , TestCase
        $ assertPretty "atMay: leftmost suffix element"  (Just 'l')
        $ B.atMay stallman   3

    , TestCase
        $ assertPretty "atMay: rightmost suffix element" (Just 'n')
        $ B.atMay stallman   7

    , TestCase
        $ assertPretty "atMay: out of bounds right"       Nothing
        $ B.atMay stallman   8
    ]



tAtDef :: Test
tAtDef = TestLabel "atDef" $ TestList
    [ TestCase
        $ assertPretty "atDef: out of bounds" 'x'
        $ B.atDef 'x' stallman (-1)

    , TestCase
        $ assertPretty "atDef: in bounds" 'S'
        $ B.atDef 'x' stallman   0
    ]



tFirst :: Test
tFirst = TestLabel "first" $ TestList
    [ TestCase
        $ assertPretty "first: empty prefix" "Stallman"
        $ B.first $ B.singleton "Stallman"

    , TestCase
        $ assertPretty "first: nonempty prefix" 'S'
        $ B.first stallman

    ]



tLast :: Test
tLast = TestLabel "last" $ TestList
    [ TestCase
        $ assertPretty "last: empty suffix" "Stallman"
        $ B.last $ B.singleton "Stallman"

    , TestCase
        $ assertPretty "last: nonempty suffix" 'n'
        $ B.last stallman

    ]



tResembles :: Test
tResembles = TestLabel "resembles" $ TestList
    [ TestCase
        $ assertPretty "resembles: identity check"            True
        $ B.resembles 2 stallman stallman

    , TestCase
        $ assertPretty "resembles: curr mismatch"             False
        $ B.resembles 2 stallman stallman { B.curr    = 'b' }

    , TestCase
        $ assertPretty "resembles: prefLen mismatch"          False
        $ B.resembles 2 stallman stallman { B.prefix  = "atS."
                                          , B.prefLen = 4
                                          }

    , TestCase
        $ assertPretty "resembles: sufLen mismatch"           False
        $ B.resembles 2 stallman stallman { B.suffix  = "llman."
                                          , B.sufLen  = 6
                                          }

    , TestCase
        $ assertPretty "resembles: prefix mismatch in radius" False
        $ B.resembles 2 stallman stallman { B.prefix = "aTS" }

    , TestCase
        $ assertPretty "resembles: suffix mismatch in radius" False
        $ B.resembles 2 stallman stallman { B.suffix = "lLman" }
    ]



tCurrPos :: Test
tCurrPos = TestLabel "currPos"
    $ TestCase $ assertPretty "currPos" 2 $ B.currPos stallman



tMove :: Test
tMove = TestLabel "move" $ TestList
    [ TestCase
        $ assertPretty "move 0" stallman
        $ B.move 0 stallman

    , TestCase
        $ assertPretty "move: identity" stallman
        $ B.move 1 $ B.move (-1) stallman

    , TestCase
        $ assertPretty "move: left"
            (B.Buffer { B.prefix  = "S"
                      , B.prefLen = 1
                      , B.curr    = 't'
                      , B.suffix  = "allman"
                      , B.sufLen  = 6
                      }
            ) $ B.move (-1) stallman

    , TestCase
        $ assertPretty "move: left edge"
            (B.Buffer { B.prefix  = []
                      , B.prefLen = 0
                      , B.curr    = 'S'
                      , B.suffix  = "tallman"
                      , B.sufLen  = 7
                      }
            ) $ B.move (-1000) stallman
    ]



tMoveTo :: Test
tMoveTo = TestLabel "moveTo"
    $ TestCase $ assertPretty "moveTo: identity" stallman
    $ B.moveTo 2 stallman





-- | Collection of all tests.
testWSEditBuffer :: Test
testWSEditBuffer = TestLabel "WSEdit.Buffer" $ TestList
    [ tSingleton
    , tFromList
    , tToList
    , tLength
    , tSub
    , tLeft
    , tRight
    , tAtMay
    , tAtDef
    , tFirst
    , tLast
    , tResembles
    , tCurrPos
    , tMove
    , tMoveTo
    ]
