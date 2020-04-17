{-# LANGUAGE LambdaCase #-}

module WSEdit.ElasticTabstops
    ( rebuildTabCache
    ) where


import Control.DeepSeq
    ( force
    )
import Control.Exception
    ( evaluate
    )
import Control.Monad.RWS.Strict
    ( asks
    , gets
    , liftIO
    , modify
    )
import Data.Foldable
    ( foldl'
    )
import Data.List.Split
    ( splitOn
    )
import Safe
    ( atDef
    , atNote
    , fromJustNote
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



fqn :: String -> String
fqn = ("WSEdit.ElasticTabstops." ++)





-- | Get the text buffer contents in table form (tab-delimited) without the last
--   cell.
tableRep :: WSEdit (B.Buffer [String])
tableRep = B.map (initSafe . splitOn "\t") <$> gets (B.map snd . edLines)



-- | Get the display width of every cell's content, excluding the delimiting tab
--   stop.
widths :: B.Buffer [String] -> WSEdit [[Int]]
widths b = mapM (mapM (stringWidthRaw 1)) $ B.toList b
    -- We can safely use `1` as the start position here since the parameter is
    -- only used for tab alignment and the tab-delimited table should not
    -- contain those any more.



-- | Get the final width of every cell, including the delimiting tab stop.
fWidths :: Int -> [[Int]] -> [[Int]]
fWidths t l = map (map (+t))
            $ fst $ foldl' f ([], [])
            $ fst $ foldl' f ([], [])
            $ l
    where
        f (out, work) curr =
            let r = zipWith max curr $ work ++ repeat 0
            in  (r:out, r)



-- | Rebuild the `elTabCache`, if not `Nothing`.
rebuildTabCache :: WSEdit ()
rebuildTabCache = gets elTabCache >>= \case
    Nothing -> return ()
    Just _  -> do
        ws     <- widths =<< tableRep
        t      <- asks tabWidth
        let fw =  fWidths t ws
        lns    <- gets edLines

        tcNew <- liftIO
               $ evaluate
               $ force
               $ B.moveTo (B.prefLength lns)
               $ fromJustNote (fqn "rebuildTabCache")
               $ B.fromList
               $ zipWith3 (\w f (_, txt) ->
                            map (\(fNo, _) ->
                                    (   1
                                      + sum (take (fNo - 1) f)
                                      + atNote (fqn "rebuildTabCache") w (fNo - 1)
                                    ,   atDef t f (fNo - 1)
                                      - atDef t w (fNo - 1)
                                    )
                                )
                                $ zip [1..]
                                $ filter (== '\t') txt
                          ) ws fw
               $ B.toList lns

        modify $ \st -> st { elTabCache = Just tcNew }
