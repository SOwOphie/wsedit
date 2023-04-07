module WSEdit.Control.Marks
    ( toggleJumpMark
    , forwardToMark
    , backwardsToMark
    ) where


import Control.Monad.RWS.Strict
    ( get
    , modify
    )
import Safe
    ( headMay
    , lastMay
    )

import WSEdit.Control.Base
    ( moveCursor
    , moveCursorHome
    , moveCursorEnd
    , refuseOnReadOnly
    )
import WSEdit.Data
    ( EdState
        ( cursorPos
        , edLines
        , searchTerms
        , tokenCache
        )
    , WSEdit
    )
import WSEdit.Util
    ( withFst
    )

import qualified WSEdit.Buffer as B



-- | Toggle a jump mark in the current line.
toggleJumpMark :: WSEdit ()
toggleJumpMark = refuseOnReadOnly
    $ modify (\s -> s { edLines = B.withCurr (withFst not)
                                $ edLines s
                      }
             )



-- | Advance to the next jump mark or search term.
forwardToMark :: WSEdit ()
forwardToMark = refuseOnReadOnly $ go True
    where
        go :: Bool -> WSEdit ()
        go first = do
            s <- get
            let
                l     = edLines s
                t     = searchTerms s
                cur   = cursorPos   s
                found = filter ((`elem` t) . snd) $ B.atDef [] (tokenCache s) (B.prefLength l)

            case headMay $ filter (\(n, _) -> not first || n > cur) found of
                 _           | fst (B.pos l) && not first -> moveCursorHome
                 Just (n, _)                              -> moveCursorHome >> moveCursor 0 (n-1)
                 _           | B.sufLength l == 0         -> moveCursorEnd
                             | otherwise                  -> moveCursor 1 0 >> moveCursorHome >> go False



-- | Go back to the previous jump mark or search term.
backwardsToMark :: WSEdit ()
backwardsToMark = refuseOnReadOnly $ go True
    where
        go :: Bool -> WSEdit ()
        go first = do
            s <- get
            let
                l     = edLines s
                t     = searchTerms s
                cur   = cursorPos   s
                found = filter ((`elem` t) . snd) $ B.atDef [] (tokenCache s) (B.prefLength l)

            case lastMay $ filter (\(n, _) -> not first || n < cur) found of
                 Just (n, _)                              -> moveCursorHome >> moveCursor 0 (n-1)
                 _           | fst (B.pos l) && not first -> moveCursorHome
                             | B.prefLength l == 0        -> moveCursorHome
                             | otherwise                  -> moveCursor (-1) 0 >> moveCursorEnd >> go False
