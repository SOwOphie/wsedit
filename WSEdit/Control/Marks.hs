module WSEdit.Control.Marks
    ( toggleJumpMark
    , forwardToMark
    , backwardsToMark
    ) where


import Control.Monad.RWS.Strict (get, modify)
import Data.Char                (toLower)

import WSEdit.Control.Base      (moveCursor, refuseOnReadOnly)
import WSEdit.Data              ( EdState (edLines, searchTerms)
                                , WSEdit
                                )
import WSEdit.Util              (findInStr, withFst)

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
forwardToMark = refuseOnReadOnly $ do
    moveCursor 1 (-65535)
    s <- get

    let
        l = edLines s
        t = searchTerms s

    case concatMap ( flip findInStr (map toLower $ snd $ B.curr l)
                   . map toLower
                   ) t of
        _  | fst (B.curr l)  -> return ()
        [] | B.sufLen l == 0 -> return ()
        []                   -> forwardToMark
        (x:_)                -> moveCursor 0 x



-- | Go back to the previous jump mark or search term.
backwardsToMark :: WSEdit ()
backwardsToMark = refuseOnReadOnly $ do
    moveCursor (-1) (-65535)
    s <- get

    let
        l = edLines s
        t = searchTerms s

    case concatMap ( flip findInStr (map toLower $ snd $ B.curr l)
                   . map toLower
                   ) t of
        _  | fst (B.curr l)   -> return ()
        [] | B.prefLen l == 0 -> return ()
        []                    -> backwardsToMark
        (x:_)                 -> moveCursor 0 x
