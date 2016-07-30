module WSEdit.Renderer
    ( rebuildL1
    , rebuildL2
    ) where


import Control.Monad            (foldM)
import Control.Monad.RWS.Strict (ask, get, put)
import Data.List                (nubBy, sort, sortOn)
import Data.Maybe               (fromMaybe)
import Data.Ord                 (Down (Down))
import Safe                     (headDef)

import WSEdit.Data              ( EdConfig ( chrDelim, blockComment, escape
                                           , keywords, mStrDelim, lineComment
                                           , strDelim
                                           )
                                , EdState ( edLines, history, l1Cache, l2Cache
                                          , scrollOffset, searchTerms
                                          )
                                , HighlightMode ( HComment, HError, HKeyword
                                                , HSearch, HString
                                                )
                                , L2Cache
                                , L2ParserState ( PNothing, PChString, PBComment
                                                , PLnString, PMLString
                                                )
                                , WSEdit
                                )
import WSEdit.Output            (getViewportDimensions)
import WSEdit.Util              (findInStr, findIsolated, withFst)

import qualified WSEdit.Buffer as B



rebuildL1 :: WSEdit ()
rebuildL1 = do
    s <- get
    case history s of
         Nothing -> fullRebuild
         Just _  -> fullRebuild

    where
        token :: String -> [String] -> [(Int, String)]
        token str l = map (withFst (+1))
                    $ concatMap (\tk -> [(x, tk) | x <- tk `findInStr` str]) l

        tokenI :: String -> [String] -> [(Int, String)]
        tokenI str l = map (withFst (+1))
                     $ concatMap (\tk -> [(x, tk) | x <- tk `findIsolated` str]) l

        unpack :: [(a, a)] -> [a]
        unpack []         = []
        unpack ((a,b):xs) = a:b:unpack xs

        ln :: (Bool, String) -> WSEdit [(Int, String)]
        ln (_, str) = do
            c <- ask
            s <- get

            let
                esc  = map (+1)
                     $ fromMaybe []
                     $ fmap ((`findInStr` str) . return)
                     $ escape c

            return $ sort
                   $ nubBy (\a b -> fst a == fst b)
                   $ sortOn Down
                   $ filter ((`notElem` esc) . (subtract 1) . fst)
                   $ token  str (         lineComment  c)
                  ++ token  str (unpack $ blockComment c)
                  ++ token  str (unpack $ strDelim     c)
                  ++ token  str (unpack $ mStrDelim    c)
                  ++ token  str (unpack $ chrDelim     c)
                  ++ token  str (         searchTerms  s)
                  ++ tokenI str (         keywords     c)

        fullRebuild :: WSEdit ()
        fullRebuild = do
            s <- get
            c <- B.mapM ln $ edLines s
            put $ s { l1Cache = c }



rebuildL2 :: WSEdit ()
rebuildL2 = do
    s <- get
    case history s of
         Nothing -> fullRebuild
         Just _  -> fullRebuild

    where
        fsm :: (EdConfig, EdState) -> L2ParserState -> [(Int, String)] -> ([((Int, Int), HighlightMode)], L2ParserState)

        fsm _         (PMLString n1 str  ) []           = ([((n1, maxBound), HString )], PMLString 1 str)
        fsm _         (PBComment n1 str  ) []           = ([((n1, maxBound), HComment)], PBComment 1 str)
        fsm _         (PLnString n1 _    ) []           = ([((n1, maxBound), HError  )], PNothing       )
        fsm _         _                    []           = ([]                          , PNothing       )

        fsm (c, s)    (PChString n1 str l) ((n2, x):xs)
            | str == x && n2 <= l = withFst (((n1, n2 + length x - 1), HString):) $ fsm (c, s) PNothing          xs
            | otherwise           =                                                 fsm (c, s) PNothing ((n2, x):xs)

        fsm (c, s) st                      ((n , x):xs)
            | x `elem` searchTerms s = withFst (((n, n + length x - 1), HSearch):) $ fsm (c, s) st xs

        fsm (c, s) st@(PLnString n1 str  ) ((n2, x):xs)
            | str == x  = withFst (((n1, n2 + length x - 1), HString):) $ fsm (c, s) PNothing xs
            | otherwise =                                  fsm (c, s) st       xs

        fsm (c, s) st@(PMLString n1 str  ) ((n2, x):xs)
            | str == x  = withFst (((n1, n2 + length x - 1), HString):) $ fsm (c, s) PNothing xs
            | otherwise =                                  fsm (c, s) st       xs

        fsm (c, s) st@(PBComment n1 str  ) ((n2, x):xs)
            | str == x  = withFst (((n1, n2 + length x - 1), HComment):) $ fsm (c, s) PNothing xs
            | otherwise =                                  fsm (c, s) st       xs

        fsm (c,s)      PNothing            ((n1, x):xs)
            |            x `elem`   lineComment  c = ([((n1, maxBound), HComment)], PNothing)
            | Just cl <- x `lookup` blockComment c = fsm (c, s) (PBComment n1 cl                    ) xs
            | Just cl <- x `lookup` strDelim     c = fsm (c, s) (PLnString n1 cl                    ) xs
            | Just cl <- x `lookup` mStrDelim    c = fsm (c, s) (PMLString n1 cl                    ) xs
            | Just cl <- x `lookup` chrDelim     c = fsm (c, s) (PChString n1 cl $ n1 + length x + 2) xs
            |            x `elem`   keywords     c = withFst (((n1, n1 + length x - 1), HKeyword):)
                                                   $ fsm (c, s) PNothing xs

        fsm (c,s)      PNothing            (_      :xs) = fsm (c,s) PNothing xs


        ln :: L2Cache -> [(Int, String)] -> WSEdit L2Cache
        ln cs l = do
            c <- ask
            s <- get
            return $ (:cs) $ fsm (c,s) (headDef PNothing $ map snd cs) l


        fullRebuild :: WSEdit ()
        fullRebuild = do
            s       <- get
            (rs, _) <- getViewportDimensions

            c       <- foldM ln []
                     $ B.sub 0 (rs + fst (scrollOffset s))
                     $ l1Cache s

            put $ s { l2Cache = c }
