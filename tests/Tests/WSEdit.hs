module Tests.WSEdit
    ( testWSEdit
    ) where

import Tests.Base
import System.Exit

import WSEdit
import WSEdit.Data
import WSEdit.Data.Pretty
import WSEdit.Util



tSplitArgs :: Test
tSplitArgs = TestLabel "splitArgs" $ TestList
    [ TestCase
        $ assertPretty "splitArgs: Nothing at all" (Nothing, 1, 1, [], False)
        $ splitArgs [] "" ""

    , TestCase
        $ assertBool "splitArgs: file name plain"
        $ case splitArgs ["testfile"] undefined undefined of
               (Just f, _, _, _, _) -> f == "testfile"
               _                    -> False

    , TestCase
        $ assertPretty "splitArgs: line number detection" (123, 1)
        $ (\(_, l, c, _, _) -> (l, c))
        $ splitArgs ["testfile", "123"] undefined undefined

    , TestCase
        $ assertPretty "splitArgs: line + col number detection" (123, 21)
        $ (\(_, l, c, _, _) -> (l, c))
        $ splitArgs ["testfile", "123", "21"] undefined undefined

    , TestCase
        $ assertPretty "splitArgs: bogus line + col number detection" (1, 1)
        $ (\(_, l, c, _, _) -> (l, c))
        $ splitArgs ["testfile", "toast", "Stallman"] undefined undefined

    , TestCase
        $ assertPretty "splitArgs: file extension detection" ["-b"]
        $ (\(_, _, _, sw, _) -> sw)
        $ splitArgs ["testfile.some.ext"] "ext: -b\nsome.ext: -p" ""

    , TestCase
        $ assertPretty "splitArgs: file extensions for config files" ["-a", "-cl"]
        $ (\(_, _, _, sw, _) -> sw)
        $ splitArgs ["-cl"] "wsconf: -a" ""

    , TestCase
        $ assertPretty "splitArgs: argument order"
                       ["-a", "-b", "-c", "-d", "-e", "-f"]
        $ (\(_, _, _, sw, _) -> sw)
        $ splitArgs ["-e", "testfile.some.ext", "-f"]
                    "ext: -b\n-a"
                    "ext: -d\n-c"
    ]



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

    , tOpt "-b"      $ withFst (\c -> c { drawBg       = False                        })
    , tOpt "-fe+\\"  $ withFst (\c -> c { escape       = Just '\\'                    })
    , tOpt "-fk+kw"  $ withFst (\c -> c { keywords     = ["kw"]                       })
    , tOpt "-flc+//" $ withFst (\c -> c { lineComment  = ["//"]                       })
    , tOpt "-fs+''"  $ withFst (\c -> c { strDelim     = [('\'','\'')]                })
    , tOpt "-i20"    $ withFst (\c -> c { tabWidth     = 20                           })
    , tOpt "-p"      $ withFst (\c -> c { purgeOnClose = True                         })
    , tOpt "-x"      $ withFst (\c -> c { edDesign     = brightTheme                  })
    , tOpt "-y"      $ withFst (\c -> c { dumpEvents   = True                         })
    , tOpt "-cg"     $ withSnd (\s -> s { fname        = "home/.config/wsedit.wsconf" })
    , tOpt "-cl"     $ withSnd (\s -> s { fname        = "./.local.wsconf"            })
    , tOpt "-d1"     $ withSnd (\s -> s { buildDict    = Just 1                       })
    , tOpt "-r"      $ withSnd (\s -> s { readOnly     = True                         })
    , tOpt "-ts"     $ withSnd (\s -> s { replaceTabs  = True
                                        , detectTabs   = False                        })
    , tOpt "-tt"     $ withSnd (\s -> s { replaceTabs  = False
                                        , detectTabs   = False                        })

    , TestCase
        $ assertPretty "argLoop: switch identities"
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
                  , "-i20"
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

    , tExit "incorrect parameter" "--shit" $ ExitFailure 1
    , tExit "keybinds help"       "-hk"    $ ExitSuccess
    , tExit "general help"        "-h"     $ ExitSuccess
    , tExit "version info"        "-V"     $ ExitSuccess
    , TestCase
         $ assertBool ("argLoop: no filename failed")
         $ case argLoop "home" []
                    (withSnd (\s -> s { fname = "" }) defaultObjects) of
                Left  (e, _) -> e == ExitFailure 1
                _            -> False
    ]
    where
        tOpt :: String -> ((EdConfig, EdState) -> (EdConfig, EdState)) -> Test
        tOpt opt objM = TestCase
                      $ assertPretty ("argLoop: " ++ opt)
                        (Right $ withFst prettyEdConfig $ objM defaultObjects)
                        ( fmap (withFst prettyEdConfig)
                            $ argLoop "home" [opt] defaultObjects
                        )

        tExit :: String -> String -> ExitCode -> Test
        tExit name opt ex = TestCase
                     $ assertBool ("argLoop: " ++ name ++ " failed")
                     $ case argLoop "home" [opt] defaultObjects of
                            Left  (e, _) -> e == ex
                            _            -> False



tVersionInfo :: Test
tVersionInfo = TestLabel "versionInfo" $ TestList
    [ TestCase
        $ assertBool "versionInfo: exit code failed"
        $ case versionInfo of
               Left  (e, _) -> e == ExitSuccess
               _            -> False
    ]



tKeymapInfo :: Test
tKeymapInfo = TestLabel "keymapInfo" $ TestList
    [ TestCase
        $ assertBool "keymapInfo: exit code failed"
        $ case keymapInfo $ fst defaultObjects of
               Left  (e, _) -> e == ExitSuccess
               _            -> False
    ]



-- | Collection of all tests.
testWSEdit :: Test
testWSEdit = TestLabel "WSEdit" $ TestList
    [ tSplitArgs
    , tArgLoop
    , tFilterFileArgs
    , tVersionInfo
    , tKeymapInfo
    ]
