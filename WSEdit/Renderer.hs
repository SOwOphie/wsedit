module WSEdit.Renderer
    ( rebuildAll
    ) where


import Control.Monad            (foldM)
import Control.Monad.RWS.Strict (ask, get, modify, put)
import Data.Ix                  (inRange)
import Data.List                (nubBy, sort, sortOn)
import Data.Maybe               (fromMaybe)
import Data.Ord                 (Down (Down))
import Safe                     (headDef)

import WSEdit.Data              ( BracketCache
                                , BracketCacheElem
                                , BracketStack
                                , EdConfig ( brackets, chrDelim, blockComment
                                           , escapeO, escapeS, keywords
                                           , mStrDelim, lineComment, strDelim
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
                                , FmtParserState ( PNothing, PBComment
                                                 , PLnString, PMLString
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

    return $ sort
           $ nubBy overlap
           $ sortOn (\(n, x) -> (n, Down (length x)))
           $ token  str (         lineComment  c)
          ++ token  str (unpack $ blockComment c)
          ++ token  str (unpack $ brackets     c)
          ++ token  str (         searchTerms  s)
          ++ tokenI str (         keywords     c)
          ++ token  str (unpack $ strDelim     c)
          ++ token  str (unpack $ mStrDelim    c)
          ++ token  str (unpack $ chrDelim     c)
          ++ token  str (         escapeO      c)
          ++ token  str (         escapeS      c)

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
--   the process.
rebuildFmt :: Maybe EdState -> WSEdit ()
rebuildFmt Nothing  = do
    s        <- get
    (rs, _ ) <- getViewportDimensions

    (rc, bc) <- foldM rLn ([], [])
              $ zip [1..]
              $ B.sub 0 (rs + fst (scrollOffset s))
              $ tokenCache s

    put $ s { rangeCache   = rc
            , bracketCache = bc
            }

rebuildFmt (Just h) = do
    s       <- get
    (rs, _) <- getViewportDimensions

    let commonLen = B.currPos (tokenCache h)
                  - fst (B.diffZone (tokenCache h) (tokenCache s))

        rcHull    = drop (length (rangeCache   h) - commonLen) $ rangeCache   h
        bcHull    = drop (length (bracketCache h) - commonLen) $ bracketCache h

    (rc, bc) <- foldM rLn (rcHull, bcHull)
              $ zip [commonLen + 1 ..]
              $ B.sub (commonLen) (rs + fst (scrollOffset s))
              $ tokenCache s

    put $ s { rangeCache   = rc
            , bracketCache = bc
            }



-- | Range line processor. Processes a single line of token buffer into
--   corresponding ranges.
rLn :: (RangeCache, BracketCache) -> (Int, [(Int, String)]) -> WSEdit (RangeCache, BracketCache)
rLn (rc, bc) (lNr, l) = do
    c <- ask
    s <- get
    return $ withPair (:rc) (:bc)
           $ fsm (c,s) lNr (headDef PNothing $ map snd rc, headDef [] $ map snd bc) l

    where
        fsm :: (EdConfig, EdState)              -- ^ Editor parameters
            -> Int                              -- ^ Line number
            -> (FmtParserState, BracketStack)   -- ^ State of the FSM leaving the previous line
            -> [(Int, String)]                  -- ^ Tokens in the line
            -> (RangeCacheElem, BracketCacheElem)

        {- ====================================================================#
        ||                                                                    ||
        ||  Ready? Go! \(^.^)/                                                ||
        ||                                                                    ||
        #==================================================================== -}


        {- --------------------------------------------------------------------*
        |  Highlight search terms regardless of current parser state           |
        *-------------------------------------------------------------------- -}

        fsm (c, s) lNo st ((n, x):xs)
            | x `elem` searchTerms s
                = withFst (withFst (((n, n + length x - 1), HSearch):))
                $ fsm (c, s) lNo st xs

        {- --------------------------------------------------------------------*
        |  Skip next token on escape if it follows directly                    |
        *-------------------------------------------------------------------- -}

        fsm (c, s) lNo st ((n1, e):(n2, _):xs)
            | n1 + length e == n2 && e `elem` escapeO c
                = fsm (c, s) lNo st xs

        {- --------------------------------------------------------------------*
        |  Multi-line strings                                                  |
        *-------------------------------------------------------------------- -}

        -- Escape character
        fsm (c, s) lNo (PMLString n1 str, st) ((n2, e):(n3, _):xs)
            | n2 + length e == n3 && e `elem` escapeS c
                = fsm (c, s) lNo (PMLString n1 str, st) xs

        fsm (c, s) lNo (PMLString n1 str, st) ((n2, x):xs)

            -- Closing the string
            | str == x
                = withFst (withFst (((n1, n2 + length x - 1), HString):))
                $ fsm (c, s) lNo (PNothing, st) xs

            -- Otherwise: just random garbage inside a string, carry on.
            | otherwise
                = fsm (c, s) lNo (PMLString n1 str, st) xs

        {- --------------------------------------------------------------------*
        |  Regular strings                                                     |
        *-------------------------------------------------------------------- -}

        -- Escape character
        fsm (c, s) lNo (PLnString n1 str, st) ((n2, e):(n3, _):xs)
            | n2 + length e == n3 && e `elem` escapeS c
                = fsm (c, s) lNo (PLnString n1 str, st) xs

        fsm (c, s) lNo (PLnString n1 str, st) ((n2, x):xs)

            -- Closing the string
            | str == x
                = withFst (withFst (((n1, n2 + length x - 1), HString):))
                $ fsm (c, s) lNo (PNothing, st) xs

            -- Otherwise: just random garbage inside a string, carry on.
            | otherwise
                = fsm (c, s) lNo (PLnString n1 str, st) xs

        {- --------------------------------------------------------------------*
        |  Character strings                                                   |
        *-------------------------------------------------------------------- -}

        -- (opening token):(escape token):(some token):(closing token):xs
        -- Number of enclosed characters: length escape + 1
        fsm (c, s) lNo (PNothing, st) ((n1, x1):(_, x2):_:(n4, x4):xs)
            | Just x4' <- x1 `lookup` chrDelim c
                    , x4 == x4'
                   && n4 == n1 + length x1 + length x2 + 1
                   && x2 `elem` escapeS c
                = withFst (withFst (((n1, n4 + length x4 - 1), HString):))
                $ fsm (c, s) lNo (PNothing, st) xs

        -- (opening token):(escape token):(closing token):xs
        -- Number of enclosed characters: length escape + 1
        fsm (c, s) lNo (PNothing, st) ((n1, x1):(_, x2):(n3, x3):xs)
            | Just x3' <- x1 `lookup` chrDelim c
                    , x3 == x3'
                   && n3 == n1 + length x1 + length x2 + 1
                   && x2 `elem` escapeS c
                = withFst (withFst (((n1, n3 + length x3 - 1), HString):))
                $ fsm (c, s) lNo (PNothing, st) xs

        -- (opening token):(some non-escape token):(closing token):xs
        -- Number of enclosed characters: 1
        fsm (c, s) lNo (PNothing, st) ((n1, x1):(_,x2):(n3, x3):xs)
            | Just x3' <- x1 `lookup` chrDelim c
                    , x3 == x3'
                   && n3 == n1 + length x1 + 1
                   && x2 `notElem` escapeS c
                = withFst (withFst (((n1, n3 + length x3 - 1), HString):))
                $ fsm (c, s) lNo (PNothing, st) xs

        -- (opening token):(closing token):xs
        -- Number of enclosed characters: 1
        fsm (c, s) lNo (PNothing, st) ((n1, x1):(n2, x2):xs)
            | Just x2' <- x1 `lookup` chrDelim c
                    , x2 == x2'
                   && n2 == n1 + length x1 + 1
                = withFst (withFst (((n1, n2 + length x2 - 1), HString):))
                $ fsm (c, s) lNo (PNothing, st) xs

        {- --------------------------------------------------------------------*
        |  End of line                                                         |
        *-------------------------------------------------------------------- -}

        -- If the line ends in a state that should transfer to the next one,
        -- facilitate that.
        fsm _      _   (PMLString n1 str  , st         ) []
            = (([((n1, maxBound), HString )], PMLString 1 str), ([], st))

        fsm _      _   (PBComment n1 str  , st         ) []
            = (([((n1, maxBound), HComment)], PBComment 1 str), ([], st))

        -- Single-line strings shouldn't, so mark the opening tag as an error
        -- and proceed with the next line.
        fsm _      _   (PLnString n1 _    , st         ) []
            = (([((n1, maxBound), HError  )], PNothing       ), ([], st))

        -- Otherwise, set the parser to the empty state for the next line.
        fsm _      _   (_                 , st         ) []
            = (([]                          , PNothing       ), ([], st))

        {- --------------------------------------------------------------------*
        |  Closing various other constructs                                    |
        *-------------------------------------------------------------------- -}

        -- Close block comments
        fsm (c, s) lNo (PBComment n1 str  , st         ) ((n2, x):xs)
            | str == x
                = withFst (withFst (((n1, n2 + length x - 1), HComment):))
                $ fsm (c, s) lNo (PNothing        , st) xs

            | otherwise
                = fsm (c, s) lNo (PBComment n1 str, st) xs

        -- Close brackets (only outside highlighted ranges)
        fsm (c, s) lNo (PNothing          , (p, str):bs) ((n1, x):xs)
            | str == x
                = withSnd (withFst ((p, (lNo, n1)):))
                $ fsm (c, s) lNo (PNothing, bs) xs

        -- Stray closing bracket? Tag it as an error!
        fsm (c, s) lNo (PNothing          , st         ) ((n1, x):xs)
            | x `elem` map snd (brackets c)
                = withFst (withFst (((n1, n1 + length x - 1), HError):))
                $ fsm (c, s) lNo (PNothing, st) xs

        {- --------------------------------------------------------------------*
        |  Open various ranges                                                 |
        *-------------------------------------------------------------------- -}
        fsm (c, s) lNo (PNothing          , st         ) ((n1, x):xs)

            -- Line comment
            |            x `elem`   lineComment  c
                = (([((n1, maxBound), HComment)], PNothing), ([], st))

            -- Bracket
            | Just cl <- x `lookup` brackets     c
                = fsm (c, s) lNo (PNothing       , ((lNo, n1), cl) : st) xs

            -- Block comment
            | Just cl <- x `lookup` blockComment c
                = fsm (c, s) lNo (PBComment n1 cl,                   st) xs

            -- Regular string
            | Just cl <- x `lookup` strDelim     c
                = fsm (c, s) lNo (PLnString n1 cl,                   st) xs

            -- Multi-line string
            | Just cl <- x `lookup` mStrDelim    c
                = fsm (c, s) lNo (PMLString n1 cl,                   st) xs

            -- Keyword
            |            x `elem`   keywords     c
                = withFst (withFst (((n1, n1 + length x - 1), HKeyword):))
                $ fsm (c, s) lNo (PNothing       ,                   st) xs

            | otherwise
                = fsm (c, s) lNo (PNothing       ,                   st) xs





-- | Rebuilds all caches in order. A past state may be given to speed up the
--   process.
rebuildAll :: Maybe EdState -> WSEdit ()
rebuildAll h = do
    s <- get
    if fullRebdReq s
       then rebuildTk Nothing >> rebuildFmt Nothing
       else rebuildTk h       >> rebuildFmt h

    modify $ \s' -> s' { fullRebdReq = False }
