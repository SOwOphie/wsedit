{-# LANGUAGE LambdaCase #-}

module WSEdit.ElasticTabstops
    ( rebuildTabCache
    ) where


import Control.Monad
    ( mplus
    )
import Control.Monad.RWS.Strict
    ( asks
    , gets
    , modify
    )
import Data.List.Split
    ( splitOn
    )
import Data.Maybe
    ( fromMaybe
    )
import Safe
    ( atMay
    , initSafe
    )

import WSEdit.Data
    ( EdConfig
        ( tabWidth
        )
    , EdState
        ( edLines
        , elTabCache
        )
    , WSEdit
    )
import WSEdit.Output
    ( stringWidthRaw
    )

import qualified WSEdit.Buffer as B





tableRep :: WSEdit (B.Buffer [String])
tableRep = B.map (initSafe . splitOn "\t") <$> gets (B.map snd . edLines)



widths :: B.Buffer [String] -> WSEdit (B.Buffer [Int])
widths b = B.mapM (mapM (stringWidthRaw 1)) b



fieldPad :: B.Buffer [Int] -> Int -> Int -> Maybe Int
fieldPad b r c = do
    let b0 = B.moveTo (r - 1) b
    w <- B.pos b0 `atMay` (c - 1)
    return $ max (fromMaybe 0 $ go B.forward  c b0)
                 (fromMaybe 0 $ go B.backward c b0)
           - w
    where
        go :: (B.Buffer [Int] -> Maybe (B.Buffer [Int])) -> Int -> B.Buffer [Int] -> Maybe Int
        go mov n b' = do
                curr <- B.pos b' `atMay` (n - 1)
                ((max curr) <$> (mov b' >>= go mov n)) `mplus` (return curr)



rebuildTabCache :: WSEdit ()
rebuildTabCache = gets elTabCache >>= \case
    Nothing -> return ()
    Just _  -> do
        ws     <- widths =<< tableRep
        t      <- asks tabWidth
        numLns <- gets (B.zip [1..] . edLines)

        let tcNew = B.map (\(lNo, (_, txt)) ->
                                map (\(fNo, (col, _)) -> (col, fromMaybe t
                                                             $ fieldPad ws lNo fNo
                                                         )
                                    )
                              $ zip [1..]
                              $ filter ((== '\t') . snd)
                              $ zip [1..] txt
                          ) numLns

        modify $ \st -> st { elTabCache = Just tcNew }
