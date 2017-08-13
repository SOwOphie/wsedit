module WSEdit.Data.AlgorithmsSpec (spec) where


import Test.Hspec
import Test.QuickCheck

import WSEdit.Data
import WSEdit.Data.Algorithms


spec :: SpecWith ()
spec = do
    describe "fileMatch" $ fmTests

fmTests :: SpecWith ()
fmTests = mapM_
    (\(file, matches)
        -> context (file ++ " relative to /usr")
         $ mapM_ (\(match, expected) -> specify (show match)
                                      $ ioProperty $ do
                                            p <- canonicalPath (Just $ CanonicalPath "/usr") file
                                            return $ fileMatch match p `shouldBe` expected
                 )
                 matches
    )
    [ ( "file.ext"
      , [ (mf "file.ext"           , True )
        , (mf "file.*"             , True )
        , (mf "*.ext"              , True )
        , (mf "*"                  , True )
        , (mp "/usr/local/file.ext", False)
        , (mp "/usr/local/file.*"  , False)
        , (mp "/usr/local/*.ext"   , False)
        , (mp "/usr/local/*"       , False)
        , (mp "/usr/file.ext"      , True )
        , (mp "/usr/file.*"        , True )
        , (mp "/usr/*.ext"         , True )
        , (mp "/usr/*"             , True )
        , (mp "/file.ext"          , False)
        , (mp "/file.*"            , False)
        , (mp "/*.ext"             , True )
        , (mp "/*"                 , True )
        ]
      )
    , ( "local/file.ext"
      , [ (mf "file.ext"           , True )
        , (mf "file.*"             , True )
        , (mf "*.ext"              , True )
        , (mf "*"                  , True )
        , (mp "/usr/local/file.ext", True )
        , (mp "/usr/local/file.*"  , True )
        , (mp "/usr/local/*.ext"   , True )
        , (mp "/usr/local/*"       , True )
        , (mp "/usr/file.ext"      , False)
        , (mp "/usr/file.*"        , False)
        , (mp "/usr/*.ext"         , True)
        , (mp "/usr/*"             , True )
        , (mp "/file.ext"          , False)
        , (mp "/file.*"            , False)
        , (mp "/*.ext"             , True )
        , (mp "/*"                 , True )
        ]
      )
    , ( "../file.ext"
      , [ (mf "file.ext"           , True )
        , (mf "file.*"             , True )
        , (mf "*.ext"              , True )
        , (mf "*"                  , True )
        , (mp "/usr/local/file.ext", False)
        , (mp "/usr/local/file.*"  , False)
        , (mp "/usr/local/*.ext"   , False)
        , (mp "/usr/local/*"       , False)
        , (mp "/usr/file.ext"      , False)
        , (mp "/usr/file.*"        , False)
        , (mp "/usr/*.ext"         , False)
        , (mp "/usr/*"             , False)
        , (mp "/file.ext"          , True )
        , (mp "/file.*"            , True )
        , (mp "/*.ext"             , True )
        , (mp "/*"                 , True )
        ]
      )
    ]

    where
        mf = MatchFilename
        mp = MatchPath . CanonicalPath
