module WSEdit.Control.Marks
    ( toggleJumpMark
    , forwardToMark
    , backwardsToMark
    ) where


import Control.Monad.RWS.Strict (get, modify)

import WSEdit.Control.Base      (moveCursor, moveCursorHome, refuseOnReadOnly)
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
    moveCursorHome
    moveCursor 1 0
    s <- get

    let
        l = edLines s
        t = searchTerms s

    case concatMap (flip findInStr (snd $ B.pos l)) t of
         _  | fst (B.pos l)      -> return ()
         [] | B.sufLength l == 0 -> return ()
         []                      -> forwardToMark
         (x:_)                   -> moveCursor 0 x



-- | Go back to the previous jump mark or search term.
backwardsToMark :: WSEdit ()
backwardsToMark = refuseOnReadOnly $ do
    moveCursorHome
    moveCursor (-1) 0
    s <- get

    let
        l = edLines s
        t = searchTerms s

    case concatMap (flip findInStr (snd $ B.pos l)) t of
         _  | fst (B.pos l)       -> return ()
         [] | B.prefLength l == 0 -> return ()
         []                       -> backwardsToMark
         (x:_)                    -> moveCursor 0 x
