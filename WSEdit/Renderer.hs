module WSEdit.Renderer
    ( rebuildAll
    ) where


import Control.Monad            (foldM)
import Control.Monad.RWS.Strict (ask, get, modify, put)
import Data.Ix                  (inRange)
import Data.List                (nubBy, sort, sortOn, (\\))
import Data.Maybe               (fromMaybe)
import Data.Ord                 (Down (Down))
import Safe                     (headDef)

import WSEdit.Data              ( BracketCache
                                , BracketCacheElem
                                , BracketStack
                                , EdConfig ( brackets, chrDelim, blockComment
                                           , escape, keywords, mStrDelim
                                           , lineComment, strDelim
                                           )
                                , EdState ( bracketCache, edLines, fullRebdReq
                                          , rangeCache, scrollOffset
                                          , searchTerms, tokenCache
                                          )
                                , HighlightMode ( HComment, HError, HKeyword
                                                , HSearch, HString
                                                )
                                , RangeCache
                                , RangeCacheElem
                                , FmtParserState ( PNothing, PChString
                                                 , PBComment, PLnString
                                                 , PMLString
                                                 )
                                , WSEdit
                                )
import WSEdit.Output            (getViewportDimensions)
import WSEdit.Util              ( findInStr, findIsolated, withFst, withPair
                                , withSnd
                                )

import qualified WSEdit.Buffer as B





-- | Rebuilds the token cache. A past state may be given as a parameter to speed
--   up the process.
rebuildTk :: Maybe EdState -> WSEdit ()
rebuildTk Nothing  = do
    s <- get
    c <- B.mapM tkLn $ edLines s
    put $ s { tokenCache = c }

rebuildTk (Just h) = do
    s <- get

    let
        (n1, n2) = B.diffZone (edLines h) (edLines s)

        cHull    = B.dropPrefix n1
                 $ B.dropRight (n2 + 1)
                 $ B.moveTo (B.currPos $ edLines h)
                 $ tokenCache h

        rebdFrom = B.moveTo (B.currPos cHull)
                 $ edLines s

    c <- rebdTk cHull rebdFrom
    modify $ \st -> st { tokenCache = c }

    where
        rebdTk :: B.Buffer [(Int, String)] -> B.Buffer (Bool, String) -> WSEdit (B.Buffer [(Int, String)])
        rebdTk cHull rebdFrom
            | B.length cHull == B.length rebdFrom
                = do
                    ln <- tkLn $ B.pos rebdFrom
                    return $ B.setPos ln cHull

            | otherwise
                = do
                    ln <- tkLn $ B.pos rebdFrom
                    rebdTk ( B.insertBefore ln cHull )
                           ( fromMaybe rebdFrom
                           $ B.forward rebdFrom
                           )


-- | Token line processor. Turns a line of text into the corresponding cache
--   entry. The 'Bool' is only there for convenience when folding over the
--   'edLines' buffer and will be completely ignored.
tkLn :: (Bool, String) -> WSEdit [(Int, String)]
tkLn (_, str) = do
    c <- ask
    s <- get

    let
        esc  = map (+1)
             $ fromMaybe []
             $ fmap ((`findInStr` str) . return)
             $ escape c

        esc' = esc \\ map (+1) esc

    return $ sort
           $ nubBy overlap
           $ sortOn (\(n, x) -> Down (length x, n))
           $ token  str (         lineComment  c)
          ++ token  str (unpack $ blockComment c)
          ++ token  str (unpack $ brackets     c)
          ++ token  str (         searchTerms  s)
          ++ tokenI str (         keywords     c)
          ++ filter ((`notElem` esc') . (subtract 1) . fst)
             ( token  str (unpack $ strDelim     c)
            ++ token  str (unpack $ mStrDelim    c)
            ++ token  str (unpack $ chrDelim     c)
             )

    where
        token :: String -> [String] -> [(Int, String)]
        token s l = map (withFst (+1))
                  $ concatMap (\tk -> [(x, tk) | x <- tk `findInStr` s]) l

        tokenI :: String -> [String] -> [(Int, String)]
        tokenI s l = map (withFst (+1))
                   $ concatMap (\tk -> [(x, tk) | x <- tk `findIsolated` s]) l

        unpack :: [(a, a)] -> [a]
        unpack []         = []
        unpack ((a,b):xs) = a:b:unpack xs

        overlap :: (Int, String) -> (Int, String) -> Bool
        overlap (n1, s1) (n2, s2) = (n1, n1 + length s1 - 1) `inRange` n2
                                 || (n2, n2 + length s2 - 1) `inRange` n1





-- | Rebuilds the range cache from the token cache, therefore this should
--   normally be called after 'rebuildTk'. A past state may be given to speed up
--   the process (not implemented yet, the parameter is ignored).
rebuildFmt :: Maybe EdState -> WSEdit ()
rebuildFmt _ = fullRebuild

    where
        fsm :: (EdConfig, EdState)              -- ^ Editor parameters
            -> Int                              -- ^ Line number
            -> (FmtParserState, BracketStack)   -- ^ State of the FSM leaving the last line
            -> [(Int, String)]                  -- ^ Tokens in the line
            -> (RangeCacheElem, BracketCacheElem)

        fsm _      _   (PMLString n1 str  , st         ) []             = (([((n1, maxBound), HString )], PMLString 1 str), ([], st))
        fsm _      _   (PBComment n1 str  , st         ) []             = (([((n1, maxBound), HComment)], PBComment 1 str), ([], st))
        fsm _      _   (PLnString n1 _    , st         ) []             = (([((n1, maxBound), HError  )], PNothing       ), ([], st))
        fsm _      _   (_                 , st         ) []             = (([]                          , PNothing       ), ([], st))

        fsm (c, s) lNo (PChString n1 str l, st         ) (_:(n2, x):xs)
            | n2 <= l && str == x = withFst (withFst (((n1, n2 + length x - 1), HString):)) $ fsm (c, s) lNo (PNothing, st)          xs

        fsm (c, s) lNo (PChString n1 str l, st         ) ((n2, x):xs)
            | n2 <= l && str == x = withFst (withFst (((n1, n2 + length x - 1), HString):)) $ fsm (c, s) lNo (PNothing, st)          xs
            | otherwise           =                                                           fsm (c, s) lNo (PNothing, st) ((n2, x):xs)

        fsm (c, s) lNo st                                ((n , x):xs)
            | x `elem` searchTerms s = withFst (withFst (((n, n + length x - 1), HSearch):)) $ fsm (c, s) lNo st xs

        fsm (c, s) lNo (PLnString n1 str  , st         ) ((n2, x):xs)
            | str == x  = withFst (withFst (((n1, n2 + length x - 1), HString ):)) $ fsm (c, s) lNo (PNothing        , st) xs
            | otherwise =                                                            fsm (c, s) lNo (PLnString n1 str, st) xs

        fsm (c, s) lNo (PMLString n1 str  , st         ) ((n2, x):xs)
            | str == x  = withFst (withFst (((n1, n2 + length x - 1), HString ):)) $ fsm (c, s) lNo (PNothing        , st) xs
            | otherwise =                                                            fsm (c, s) lNo (PMLString n1 str, st) xs

        fsm (c, s) lNo (PBComment n1 str  , st         ) ((n2, x):xs)
            | str == x  = withFst (withFst (((n1, n2 + length x - 1), HComment):)) $ fsm (c, s) lNo (PNothing        , st) xs
            | otherwise =                                                            fsm (c, s) lNo (PBComment n1 str, st) xs

        fsm (c, s) lNo (PNothing          , (p, str):bs) ((n1, x):xs)
            | str == x = withSnd (withFst ((p, (lNo, n1)):)) $ fsm (c, s) lNo (PNothing, bs) xs

        fsm (c, s) lNo (PNothing          , st         ) ((n1, x):xs)
            |            x `elem`   lineComment  c = (([((n1, maxBound), HComment)], PNothing), ([], st))
            | Just cl <- x `lookup` brackets     c = fsm (c, s) lNo (PNothing                           , ((lNo, n1), cl) : st) xs
            | Just cl <- x `lookup` blockComment c = fsm (c, s) lNo (PBComment n1 cl                    ,                   st) xs
            | Just cl <- x `lookup` strDelim     c = fsm (c, s) lNo (PLnString n1 cl                    ,                   st) xs
            | Just cl <- x `lookup` mStrDelim    c = fsm (c, s) lNo (PMLString n1 cl                    ,                   st) xs
            | Just cl <- x `lookup` chrDelim     c = fsm (c, s) lNo (PChString n1 cl $ n1 + length x + 2,                   st) xs
            |            x `elem`   keywords     c = withFst (withFst (((n1, n1 + length x - 1), HKeyword):))
                                                   $ fsm (c, s) lNo (PNothing                           ,                   st) xs

            | otherwise                            = withFst (withFst (((n1, n1 + length x - 1), HError  ):))
                                                   $ fsm (c, s) lNo (PNothing                           ,                   st) xs


        ln :: (RangeCache, BracketCache) -> (Int, [(Int, String)]) -> WSEdit (RangeCache, BracketCache)
        ln (rc, bc) (lNo, l) = do
            c <- ask
            s <- get
            return $ withPair (:rc) (:bc) $ fsm (c,s) lNo (headDef PNothing $ map snd rc, headDef [] $ map snd bc) l


        fullRebuild :: WSEdit ()
        fullRebuild = do
            s        <- get
            (rs, _ ) <- getViewportDimensions

            (rc, bc) <- foldM ln ([], [])
                      $ zip [1..]
                      $ B.sub 0 (rs + fst (scrollOffset s))
                      $ tokenCache s

            put $ s { rangeCache   = rc
                    , bracketCache = bc
                    }





-- | Rebuilds all caches in order. A past state may be given to speed up the
--   process.
rebuildAll :: Maybe EdState -> WSEdit ()
rebuildAll h = do
    s <- get
    if fullRebdReq s
       then rebuildTk Nothing >> rebuildFmt Nothing
       else rebuildTk h       >> rebuildFmt h

    modify $ \s' -> s' { fullRebdReq = False }
