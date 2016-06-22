module Tests.WSEdit
    ( testWSEdit
    ) where

import Tests.Base
import System.Exit

import WSEdit
import WSEdit.Data
import WSEdit.Data.Pretty
import WSEdit.Util



tFilterFileArgs :: Test
tFilterFileArgs = TestLabel "filterFileArgs" $ TestList
    [ TestCase
        $ assertPretty "filterFileArgs: Nothing"
            (["-bcd", "-flc+:"])
            ( filterFileArgs Nothing
                $ unlines ["-bcd", "hs: -bcd", "-flc+:"]
            )

    , TestCase
        $ assertPretty "filterFileArgs: Just"
            (["-bcd", "-flc+:", "-bcd"])
            ( filterFileArgs (Just "hs")
                $ unlines ["-bcd", "hs: -bcd", "lua: -asdf", "-flc+:"]
            )
    ]



tArgLoop :: Test
tArgLoop = TestLabel "argLoop" $ TestList
    [ TestCase
        $ assertPretty "argLoop: -s does nothing"
            (Right $ withFst prettyEdConfig $ defaultObjects)
            ( fmap (withFst prettyEdConfig)
                $ argLoop "home" ["-s"] defaultObjects
            )

    , TestCase
        $ assertPretty "argLoop: switch negation"
            (Right $ withFst prettyEdConfig $ defaultObjects)
            ( fmap (withFst prettyEdConfig)
                $ flip (argLoop "home") defaultObjects
                $ [ "-b"
                  , "-B"
                  , "-fe+\\"
                  , "-fe-"
                  , "-fk+keyword"
                  , "-fk-keyword"
                  , "-flc+//"
                  , "-flc-//"
                  , "-fs+''"
                  , "-fs-''"
                  , "-i8"
                  , "-i4"
                  , "-p"
                  , "-P"
                  , "-x"
                  , "-X"
                  , "-y"
                  , "-Y"
                  , "-d1"
                  , "-D"
                  , "-r"
                  , "-R"
                  , "-ts"
                  , "-tt"
                  , "-T"
                  ]
            )

    , TestCase
        $ assertBool "argLoop: incorrect parameter failed"
        $ case argLoop "home" ["--shit"] defaultObjects of
               Left  (e, _) -> e == ExitFailure 1
               Right _      -> False

    , TestCase
        $ assertBool "argLoop: keybinds help failed"
        $ case argLoop "home" ["-r", "-hk"] defaultObjects of
               Left  (e, _) -> e == ExitSuccess
               Right _      -> False

    , TestCase
        $ assertBool "argLoop: general help failed"
        $ case argLoop "home" ["-r", "-h"] defaultObjects of
               Left  (e, _) -> e == ExitSuccess
               Right _      -> False

    , TestCase
        $ assertBool "argLoop: version info failed"
        $ case argLoop "home" ["-r", "-V"] defaultObjects of
               Left  (e, _) -> e == ExitSuccess
               Right _      -> False
    ]



tVersionInfo :: Test
tVersionInfo = TestLabel "versionInfo" $ TestList
    [ TestCase
        $ assertBool "versionInfo: exit code failed"
        $ case versionInfo of
               Left  (e, _) -> e == ExitSuccess
               Right _      -> False
    ]



tKeymapInfo :: Test
tKeymapInfo = TestLabel "keymapInfo" $ TestList
    [ TestCase
        $ assertBool "keymapInfo: exit code failed"
        $ case keymapInfo $ fst defaultObjects of
               Left  (e, _) -> e == ExitSuccess
               Right _      -> False
    ]



-- | Collection of all tests.
testWSEdit :: Test
testWSEdit = TestLabel "WSEdit" $ TestList
    [ tArgLoop
    , tFilterFileArgs
    , tVersionInfo
    , tKeymapInfo
    ]
