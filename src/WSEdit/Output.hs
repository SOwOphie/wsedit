{-# LANGUAGE LambdaCase
           , MultiWayIf
           #-}

module WSEdit.Output
    ( charWidthRaw
    , charWidth
    , stringWidthRaw
    , stringWidth
    , charRep
    , lineRep
    , visToTxtPos
    , txtToVisPos
    , lineNoWidth
    , toCursorDispPos
    , getViewportDimensions
    , cursorOffScreen
    , makeFrame
    , draw
    , drawExitFrame
    ) where

import Control.Monad
    ( foldM
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Control.Monad.RWS.Strict
    ( ask
    , asks
    , get
    , gets
    )
import Data.Char
    ( ord
    )
import Data.Ix
    ( inRange
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Tuple
    ( swap
    )
import Graphics.Vty
    ( Attr
        ( Attr
        , attrBackColor
        , attrForeColor
        , attrStyle
        )
    , Background
        ( ClearBackground
        )
    , Cursor
        ( Cursor
        , NoCursor
        )
    , Image
    , MaybeDefault
        ( KeepCurrent
        )
    , Picture
        ( Picture
        , picBackground
        , picCursor
        , picLayers
        )
    , char
    , cropBottom
    , cropRight
    , defAttr
    , emptyImage
    , horizCat
    , pad
    , picForImage
    , reverseVideo
    , string
    , update
    , vertCat
    , withStyle
    , (<|>)
    , (<->)
    )
import Numeric
    ( showHex
    )
import Safe
    ( atDef
    , lookupJustDef
    )

import WSEdit.Data
    ( EdConfig
        ( drawBg
        , edDesign
        , tabWidth
        , vtyObj
        )
    , EdDesign
        ( dBGChar
        , dBGFormat
        , dBrMod
        , dCharStyles
        , dColChar
        , dColNoFormat
        , dColNoInterval
        , dCurrLnMod
        , dFrameFormat
        , dHLStyles
        , dLineNoFormat
        , dLineNoInterv
        , dJumpMarkFmt
        , dStatusFormat
        , dTabExt
        , dTabStr
        )
    , EdState
        ( badgeText
        , changed
        , edLines
        , elTabCache
        , fname
        , markPos
        , overwrite
        , rangeCache
        , readOnly
        , replaceTabs
        , scrollOffset
        , status
        )
    , HighlightMode
        ( HError
        , HNone
        , HSelected
        , HString
        )
    , WSEdit
    )
import WSEdit.Data.Algorithms
    ( getCurrBracket
    , getCursor
    , getDisplayBounds
    , getOffset
    , getSelBounds
    )
import WSEdit.Util
    ( CharClass
        ( Bracket
        , Lower
        , Special
        , Unprintable
        , Whitesp
        )
    , charClass
    , combineAttrs
    , iff
    , lookupBy
    , padLeft
    , padRight
    )

import qualified WSEdit.Buffer as B





-- | Returns the display width of a given char in a given column, ignoring
--   elastic tab adjustments.
charWidthRaw :: Int -> Char -> WSEdit Int
charWidthRaw n '\t'                           = (\w -> w - (n-1) `mod` w) <$> asks tabWidth
charWidthRaw _ c | charClass c == Unprintable = return $ length (showHex (ord c) "") + 3
                 | otherwise                  = return 1


-- | Returns the display width of a given char in a given line and column,
--   including elastic tab adjustments if enabled.
charWidth :: Int -> Int -> Char -> WSEdit Int
charWidth r n '\t' = do
    t <- asks tabWidth
    gets elTabCache >>= \case
        Nothing -> charWidthRaw n '\t'
        Just c  -> do
            l    <- gets (snd . flip (B.atDef (False, "")) (r-1) . edLines)
            tPos <- visToTxtPos l r n
            return $ maybe 0 (+t) $ lookup tPos $ B.atDef [] c $ r - 1
charWidth _ n c    = charWidthRaw n c

-- | Returns the display width of a given string starting at a given column,
--   ignoring elastic tab adjustments.
stringWidthRaw :: Int -> String -> WSEdit Int
stringWidthRaw n = foldM (\n' c -> (+ n') <$> charWidthRaw (n' + 1) c) $ n - 1

-- | Returns the display width of a given string starting at a given line and
--   column, including elastic tab adjustments if enabled.
stringWidth :: Int -> Int -> String -> WSEdit Int
stringWidth r n = foldM (\n' c -> (+ n') <$> charWidth r (n' + 1) c) $ n - 1




-- | Intermediate format for rendering.
data Snippet = Snippet
    { sNominal :: Attr
        -- | What it should look like.

    , sStr     :: String
        -- | Contained string.
    }

-- | Intermediate format for rendering.
data CalcSnippet = CalcSnippet
    { csNominal :: Attr
        -- | What it should look like.

    , csActual  :: Attr
        -- | Optimized `Attr` carrying over attributes from the previous snippet, if possibe.

    , csStr     :: String
        -- | Contained string.
    }


-- | Returns the visual representation of a character at a given buffer position
--   and in a given display column. The first argument toggles the bracket
--   format modifier.
charRep :: Bool -> HighlightMode -> (Int, Int) -> Char -> WSEdit Snippet
charRep br hl pos '\t' = do
    (r, _)    <- getCursor
    st        <- get
    conf      <- ask
    dispWidth <- charWidth (fst pos) (snd pos) '\t'

    let
        maxTabWidth = 1023
        d           = edDesign   conf
        currSty     = dCurrLnMod d
        extTab      = padLeft maxTabWidth (dTabExt d) $ dTabStr d

    return $ Snippet
        { sNominal = iff (r == fst pos && hl /= HSelected && not (readOnly st))
                         (flip combineAttrs currSty)
                   $ iff br (combineAttrs $ dBrMod d)
                   $ case hl of
                          HSelected -> lookupJustDef defAttr HSelected $ dHLStyles   d
                          _         -> lookupJustDef defAttr Whitesp   $ dCharStyles d

        , sStr     =  drop (length extTab - dispWidth) extTab
        }

charRep br hl pos ' ' = do
    (r, _) <- getCursor
    st     <- get
    d      <- edDesign <$> ask

    return $ Snippet
        { sNominal = iff (r == fst pos && hl /= HSelected && not (readOnly st))
                         (flip combineAttrs $ dCurrLnMod d)
                   $ iff br (combineAttrs $ dBrMod d)
                   $ lookupJustDef defAttr hl
                   $ dHLStyles d

        , sStr     = " "
        }

charRep br hl pos ch = do
    (r, _) <- getCursor
    st     <- get
    d      <- edDesign <$> ask

    return $ Snippet
        { sNominal = iff (r == fst pos && hl /= HSelected && not (readOnly st))
                         (flip combineAttrs $ dCurrLnMod d)
                   $ iff br (combineAttrs $ dBrMod d)
                   $ lookupJustDef
                        (lookupJustDef defAttr (charClass ch) (dCharStyles d))
                     hl
                    (dHLStyles d)

        , sStr     = if charClass ch == Unprintable
                        then "?#" ++ showHex (ord ch) ";"
                        else [ch]
        }



-- | Returns the visual representation of a line with a given line number at a
--   given horizontal offset.
lineRep :: Int -> Int -> String -> WSEdit Image
lineRep lNo off str = do
    st     <- get
    maySel <- getSelBounds
    mayBr  <- getCurrBracket

    let
        -- | `rangeCache` excerpt for the current line.
        fmt = atDef [] (map fst $ rangeCache st) (length (rangeCache st) - lNo)

        -- | Folding helper, renders `Char`s into `Snippet`s. Folds over the
        --   line chars with a
        --   ([Snippet], textual position, visual position, columns left to skip)
        --   state.
        f :: ([Snippet], Int, Int, Int) -> Char -> WSEdit ([Snippet], Int, Int, Int)
        f (im, tPos, vPos, toSkip) c = do
            i <- charRep
                    (fromMaybe False $ fmap (`inBracketHL` (lNo, tPos)) mayBr)
                    (case (maySel, lookupBy (`inRange` tPos) fmt) of
                          (Just (sS, sE), _     ) | sS <= (lNo, tPos) && (lNo, tPos) <= sE -> HSelected
                          (_            , Just s)                                          -> s
                          _                       | otherwise                              -> HNone
                    )
                    (lNo, vPos)
                    c

            return $ if
                | toSkip <= 0              -> (i:im, tPos + 1, vPos + length (sStr i), 0)
                | toSkip < length (sStr i) -> ( i { sStr = drop toSkip $ sStr i} : im
                                              , tPos + 1
                                              , vPos + length (sStr i)
                                              , 0
                                              )
                | otherwise                -> ( im
                                              , tPos + 1
                                              , vPos + length (sStr i)
                                              , toSkip - length (sStr i)
                                              )

    (r,_,_,_) <- foldM f ([], 1, 1, off) str

    return $ horizCat
           $ map (\CalcSnippet { csActual = a, csStr = b } -> string a b)
           $ calcSnippet Nothing
           $ reverse r

    where
        -- | Snippet combiner. The first argument is an accumulator for the
        --   current snippet, the second is a list of snippets to combine.
        calcSnippet :: Maybe CalcSnippet -> [Snippet] -> [CalcSnippet]
        calcSnippet  Nothing      []             = []
        calcSnippet (Just x     ) []             = [x]
        calcSnippet  Nothing      (x       :xs ) =
            calcSnippet (Just $ CalcSnippet { csNominal = sNominal x
                                            , csActual  = sNominal x
                                            , csStr     = sStr     x
                                            }
                        ) xs

        calcSnippet (Just cs) (x:xs)
            | csNominal cs == sNominal x =
                calcSnippet (Just $ cs { csStr = csStr cs ++ sStr x }) xs

            | otherwise                  =
                cs : calcSnippet (Just $ CalcSnippet
                                    { csNominal = sNominal x
                                    , csActual  = Attr
                                        { attrStyle     = tryPreserve
                                            (attrStyle     $ csNominal cs)
                                            (attrStyle     $  sNominal x )

                                        , attrForeColor = tryPreserve
                                            (attrForeColor $ csNominal cs)
                                            (attrForeColor $  sNominal x )

                                        , attrBackColor = tryPreserve
                                            (attrBackColor $ csNominal cs)
                                            (attrBackColor $  sNominal x )
                                        }
                                    , csStr     = sStr x
                                    }
                                 ) xs


        tryPreserve :: (Eq a) => MaybeDefault a -> MaybeDefault a -> MaybeDefault a
        tryPreserve x y | x == y    = KeepCurrent
                        | otherwise = y


        inBracketHL :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
        inBracketHL ((a, b), (c, d)) (x, y) = (x, y) `elem` [(a, b), (c, d)]



-- | Returns the textual position of the cursor in a line, given the line number
--   and its visual position.
visToTxtPos :: String -> Int -> Int -> WSEdit Int
visToTxtPos = pos 1
    where
        pos :: Int -> String -> Int -> Int -> WSEdit Int
        pos _   []     _ _            = return 1
        pos col _      _ c | col == c = return 1
        pos col _      _ c | col >  c = return 0
        pos col (x:xs) r c            = do
            w <- charWidth r col x
            (+ 1) <$> pos (col + w) xs r c


-- | Returns the visual position of the cursor in a line, given the line number
--  and its textual position.
txtToVisPos :: String -> Int -> Int -> WSEdit Int
txtToVisPos txt r n = (+1) <$> stringWidth r 1 (take (n - 1) txt)



-- | Returns the width of the line numbers column, based on the number of
--   lines the file has.
lineNoWidth :: WSEdit Int
lineNoWidth =  length
            .  show
            .  B.length
            .  edLines
           <$> get



-- | Return the cursor's position on the display, given its position in the
--   buffer.
toCursorDispPos :: (Int, Int) -> WSEdit (Int, Int)
toCursorDispPos (r, c) = do
    currLine <-  snd
              .  B.pos
              .  edLines
             <$> get

    offset <- txtToVisPos currLine r c

    n <- lineNoWidth
    (r', c') <- scrollOffset <$> get
    return (r - r' + 1, offset - c' + n + 3)


-- | Returns the number of rows, columns of text displayed.
getViewportDimensions :: WSEdit (Int, Int)
getViewportDimensions = do
    (nRows, nCols) <- getDisplayBounds
    lNoWidth <- lineNoWidth

    return (nRows - 4, nCols - lNoWidth - 6)



-- | Returns the number of (rows up, rows down), (cols left, cols right)
--   the cursor is off screen by.
cursorOffScreen :: WSEdit ((Int, Int), (Int, Int))
cursorOffScreen = do
    s <- get

    let
        currLn       = snd $ B.pos $ edLines s
        (scrR, scrC) = scrollOffset s

    (curR, curC_) <- getCursor
    curC <- txtToVisPos currLn curR curC_

    (maxR, maxC) <- getViewportDimensions

    return ( ( max 0 $ (1 + scrR) - curR
             , max 0 $ curR - (maxR + scrR)
             )
           , ( max 0 $ (1 + scrC) - curC
             , max 0 $ curC - (maxC + scrC)
             )
           )





-- | Creates the top border of the interface.
makeHeader :: WSEdit Image
makeHeader = do
    d <- edDesign <$> ask
    lNoWidth <- lineNoWidth

    (_, scrollCols) <- getOffset
    (_, txtCols   ) <- getViewportDimensions

    return
        $ (  string (dFrameFormat d) (replicate (lNoWidth + 2) ' ' ++ "│")
         <|> string (dColNoFormat d)
                (take (txtCols + 1)
                      ( drop scrollCols
                      $ (' ':)
                      $ concatMap ( padRight (dColNoInterval d) ' '
                                  . show
                                  )
                        [1, dColNoInterval d + 1 ..]
                      )
                )
         <|> string (dFrameFormat d) "│"
          )
       <-> string (dFrameFormat d)
            ( replicate (lNoWidth + 2) '─'
           ++ "╬"
           ++ take (txtCols + 1)
                  ( drop scrollCols
                  $ ('═':)
                  $ cycle
                  $ '╬' : replicate (dColNoInterval d - 1) '═'
                  )
           ++ "╬─"
            )



-- | Creates the left border of the interface.
makeLineNos :: WSEdit Image
makeLineNos = do
    s <- get
    d <- edDesign <$> ask
    lNoWidth <- lineNoWidth

    (scrollRows, _) <- getOffset
    (r         , _) <- getCursor
    (txtRows   , _)<- getViewportDimensions

    return $ vertCat
           $ map (mkLn s d lNoWidth r)
             [scrollRows + 1, scrollRows + 2 .. scrollRows + txtRows]

    where
        mkLn :: EdState -> EdDesign -> Int -> Int -> Int -> Image
        mkLn s d lNoWidth r n =
             char (dLineNoFormat d) ' '
         <|> string (if | r == n && not (readOnly s)   -> combineAttrs (dCurrLnMod    d)
                                                                       (dLineNoFormat d)
                        | n `mod` dLineNoInterv d == 0 ->               dLineNoFormat d
                        | otherwise                    ->               dFrameFormat  d
                    )
             ( padLeft lNoWidth ' '
             $ if | n > B.length (edLines s)               -> ""
                  | n `mod` dLineNoInterv d == 0 || r == n -> show n
                  | otherwise                              -> "·"
             )
         <|> string (dFrameFormat d) " ║"



-- | Creates the bottom border of the interface.
makeFooter :: WSEdit Image
makeFooter = do
    s <- get
    d <- edDesign <$> ask

    lNoWidth <- lineNoWidth

    (_         , txtCols) <- getViewportDimensions
    (_         ,   nCols) <- getDisplayBounds
    (r         , c      ) <- getCursor

    return $ string (dFrameFormat d)
                ( replicate (lNoWidth + 2) '─'
               ++ "╬═══════════╦"
               ++ replicate (txtCols - 11) '═'
               ++ "╩─"
                )
          <-> (  string (dFrameFormat d)
                    (" " ++ show (B.length $ edLines s) ++ " │ ")
             <|> (if replaceTabs s
                     then string (charStyle Lower   d) "SPC"
                     else string (charStyle Whitesp d) "TAB"
                 )
             <|> string (dFrameFormat d) " "
             <|> (if overwrite s
                     then string (hlStyle HError d) "OVR"
                     else string (dFrameFormat   d) "INS"
                 )
             <|> string (dFrameFormat d) " "
             <|> (if readOnly s
                     then string (charStyle Special d) "R"
                     else string (dFrameFormat      d) "W"
                 )
             <|> string (dFrameFormat d) " │ "
             <|> string (dFrameFormat d) (fname s)
             <|> (if changed s
                     then string (hlStyle HString d) "*"
                     else string (dFrameFormat    d) " "
                 )
             <|> string (dFrameFormat d)
                 ( if readOnly s
                      then ""
                      else ( case markPos s of
                                  Nothing -> ""
                                  Just  m -> show m
                                          ++ " -> "
                           )
                ++ show (r, c)
                ++ " "
                 )
             <|> string (dStatusFormat d) ( padRight nCols ' '
                                          $ filter (/= '\n')
                                          $ status s
                                          )
              )

    where
        charStyle :: CharClass -> EdDesign -> Attr
        charStyle c d = lookupJustDef defAttr c $ dCharStyles d

        hlStyle :: HighlightMode -> EdDesign -> Attr
        hlStyle m d = lookupJustDef defAttr m $ dHLStyles d



-- | Assembles the entire interface frame.
makeFrame :: WSEdit Image
makeFrame = do
    h <- makeHeader
    l <- makeLineNos
    f <- makeFooter
    return $ h <-> l <-> f



-- | Render the current text window.
makeTextFrame :: WSEdit Image
makeTextFrame = do
    s <- get
    c <- ask

    let d = edDesign c

    lNoWidth <- lineNoWidth
    mayBr    <- getCurrBracket

    (scrollRows, scrollCols) <- getOffset
    (   txtRows, txtCols   ) <- getViewportDimensions
    (cR        , _         ) <- getCursor

    txt <- mapM (\(n, (b, l)) -> lineRep n scrollCols l >>= \l' -> return (b, l'))
         $ zip [1 + scrollRows ..]
         $ B.sub scrollRows (scrollRows + txtRows - 1)
         $ edLines s

    return $ pad (lNoWidth + 3) 2 0 0
           $ cropRight (txtCols + 1)  -- +1 to compensate for the leading blank
           $ vertCat
           $ map (\(l, (b, ln)) ->
                    (case (mayBr, b) of
                          (_                    , True) -> (char (dJumpMarkFmt d) '•' <|>)

                          (Just ((x, _), (y, _)), _   )
                            | x == l && l == y -> (char (                                dLineNoFormat d) ' ' <|>)
                            | x == l           -> (char (lookupJustDef defAttr Bracket $ dCharStyles   d) '┌' <|>)
                            |           l == y -> (char (lookupJustDef defAttr Bracket $ dCharStyles   d) '└' <|>)
                            | x <  l && l <  y -> (char (lookupJustDef defAttr Bracket $ dCharStyles   d) '│' <|>)

                          _ -> (char (dLineNoFormat d) ' ' <|>)
                    )
                    $ pad 0 0 1 0
                    $ (iff (not $ drawBg c)
                           (<|> char ( iff (l == cR && not (readOnly s))
                                           (combineAttrs $ dCurrLnMod d)
                                     $ lookupJustDef defAttr Whitesp
                                     $ dCharStyles d
                                     ) (dBGChar d)
                           )
                      ) ln
                 )
           $ zip [1 + scrollRows ..] txt



-- | Render the background.
makeBackground :: WSEdit Image
makeBackground = do
    conf <- ask
    s <- get

    (nRows     , nCols     ) <- getViewportDimensions
    (scrollRows, scrollCols) <- getOffset

    cursor   <- getCursor
    lNoWidth <- lineNoWidth

    let
        bgChar  = if drawBg conf
                     then            dBGChar    $ edDesign conf
                     else ' '
        bgSty   =                    dBGFormat  $ edDesign conf
        colChar = fromMaybe bgChar $ dColChar   $ edDesign conf

    return $ pad (lNoWidth + 3) 2 0 0
           $ vertCat
           $ map (\(n, l) -> string (if n == fst cursor && not (readOnly s)
                                          then bgSty `withStyle` reverseVideo
                                          else bgSty
                                    ) l
                 )
           $ take nRows
           $ drop scrollRows
           $ zip [1..]
           $ repeat
           $ ('#' :)
           $ take nCols
           $ drop scrollCols
           $ cycle
           $ colChar : replicate (dColNoInterval (edDesign conf) - 1) bgChar



-- | Render the scroll bar to the right.
makeScrollbar :: WSEdit Image
makeScrollbar = do
    d <- edDesign <$> ask
    s <- get

    (nRows, nCols) <- getViewportDimensions
    (sRows, _    ) <- getOffset
    (curR , _    ) <- getCursor

    lNoWidth <- lineNoWidth

    let
        -- Number of lines the document has
        nLns     = B.length $ edLines s

        -- List of jump marks
        markList = map fst
                 $ filter (\(_, (b, _)) -> b)
                 $ zip [(1 :: Int)..]
                 $ B.toList
                 $ edLines s

        marksAt  = map ( floor
                       . (*  fromIntegral (nRows - 1)       )
                       . (/ (fromIntegral  nLns :: Rational))
                       . fromIntegral
                       ) markList

        -- Relative position of the viewport's upper edge
        stProg   = floor $  fromIntegral (nRows - 1     )
                         *  fromIntegral  sRows
                         / (fromIntegral  nLns :: Rational)

        -- Position of the viewport's lower edge
        endProg  = floor $ 1
                         +  fromIntegral (nRows - 1     )
                         *  fromIntegral (sRows + nRows )
                         / (fromIntegral  nLns :: Rational)

        -- Position of the cursor
        cProg    = floor $  fromIntegral (nRows - 1     )
                         *  fromIntegral  curR
                         / (fromIntegral  nLns :: Rational)

    return $ pad (lNoWidth + 4 + nCols) 2 0 0
           $ cropBottom nRows
           $ vertCat
           $ map (repl (d, s, cProg, marksAt))
           $ zip [(0 :: Int)..]
           $ replicate  stProg                 ' '
          ++                                  ['┬']
          ++ replicate (endProg - stProg - 2 ) '│'
          ++                                  ['┴']
          ++ replicate (nRows - endProg      ) ' '

    where
        repl :: (EdDesign, EdState, Int, [Int]) -> (Int, Char) -> Image
        repl (d, s, cProg, marksAt) (n, c) =
            char (dFrameFormat d) '║'
                <|> if | readOnly s       -> char (dFrameFormat  d)  c
                       | n == cProg       -> char (dLineNoFormat d) '<'
                       | n `elem` marksAt -> char (dJumpMarkFmt  d) '•'
                       | otherwise        -> char (dFrameFormat  d)  c



-- | Generates a badge over the top right corner.
makeShittyBadge :: String -> WSEdit Image
makeShittyBadge str = do
    d         <- edDesign <$> ask
    (_, cols) <- getDisplayBounds

    return $ vertCat
           $ (++ [ pad (cols - 2) 0 0 0 $ string (dFrameFormat d) "\\ "
                 , pad (cols - 1) 0 0 0 $ string (dFrameFormat d) "\\"
                 ]
             )
           $ map (\(n, c) -> pad (cols - length str - 2 + n) 0 0 0
                           $  string (dFrameFormat  d) "\\ "
                          <|> string (dLineNoFormat d) [c]
                          <|> string (dFrameFormat  d) " \\"
                 )
           $ zip [0..] str



-- | Draws everything.
draw :: WSEdit ()
draw = do
    s <- get
    c <- ask

    cursor <- getCursor >>= toCursorDispPos
    ((ru, rd), (cl, cr)) <- cursorOffScreen

    frame <- makeFrame
    txt   <- makeTextFrame
    bg    <- makeBackground

    scr   <- makeScrollbar
    bad   <- case badgeText s of
                  Just str  -> makeShittyBadge $ " " ++ str ++ " "
                  Nothing -> return emptyImage

    liftIO $ update (vtyObj c)
             Picture
                { picCursor     = if readOnly s || ru + rd + cl + cr > 0
                                     then NoCursor
                                     else uncurry Cursor $ swap cursor
                , picLayers     = [ bad
                                  , frame
                                  , txt
                                  , bg
                                  , scr
                                  ]
                , picBackground = ClearBackground
                }



-- | Draw a frame that will keep some terminals from breaking.
drawExitFrame :: WSEdit ()
drawExitFrame = do
    v <- vtyObj <$> ask

    liftIO $ update v $ picForImage $ char defAttr ' '
