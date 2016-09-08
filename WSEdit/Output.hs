{-# LANGUAGE LambdaCase #-}

module WSEdit.Output
    ( charWidth
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

import Control.Monad            (foldM)
import Control.Monad.IO.Class   (liftIO)
import Control.Monad.RWS.Strict (ask, get)
import Data.Char                (ord)
import Data.Default             (def)
import Data.Ix                  (inRange)
import Data.Maybe               (fromMaybe)
import Data.Tuple               (swap)
import Graphics.Vty             ( Attr
                                , Background (ClearBackground)
                                , Cursor (Cursor, NoCursor)
                                , Image
                                , Picture ( Picture, picBackground, picCursor
                                          , picLayers
                                          )
                                , char, cropBottom, cropRight, horizCat
                                , pad, picForImage, reverseVideo, string
                                , translateX, update, vertCat, withStyle
                                , (<|>), (<->)
                                )
import Numeric                  (showHex)
import Safe                     (atDef, lookupJustDef)

import WSEdit.Data              ( EdConfig (drawBg, edDesign, tabWidth, vtyObj)
                                , EdDesign ( dBGChar, dBGFormat, dBrMod
                                           , dCharStyles, dColChar, dColNoFormat
                                           , dColNoInterval, dCurrLnMod
                                           , dFrameFormat, dHLStyles
                                           , dLineNoFormat, dLineNoInterv
                                           , dJumpMarkFmt, dStatusFormat
                                           , dTabExt, dTabStr
                                           )
                                , EdState ( changed, edLines, fname, markPos
                                          , overwrite, rangeCache, readOnly
                                          , replaceTabs, scrollOffset, status
                                          )
                                , HighlightMode (HNone, HSelected)
                                , WSEdit
                                , getCurrBracket, getCursor, getDisplayBounds
                                , getOffset, getSelBounds
                                )
import WSEdit.Util              ( CharClass (Unprintable, Whitesp)
                                , charClass, combineAttrs, iff, lookupBy
                                , padLeft, padRight
                                )

import qualified WSEdit.Buffer as B





-- | Returns the display width of a given char in a given column.
charWidth :: Int -> Char -> WSEdit Int
charWidth n '\t'                           = (\w -> w - (n-1) `mod` w) . tabWidth <$> ask
charWidth _ c | charClass c == Unprintable = return $ length (showHex (ord c) "") + 3
              | otherwise                  = return 1

-- | Returns the display width of a given string starting at a given column.
stringWidth :: Int -> String -> WSEdit Int
stringWidth n = foldM (\n' c -> (+ n') <$> charWidth (n' + 1) c) $ n - 1




-- | Intermediate format for rendering.
type Snippet = (Attr, String)


-- | Returns the visual representation of a character at a given buffer position
--   and in a given display column. The first argument toggles the bracket
--   format modifier.
charRep :: Bool -> HighlightMode -> (Int, Int) -> Int -> Char -> WSEdit Snippet
charRep br hl pos n '\t' = do
    (r, _) <- getCursor
    st     <- get
    c      <- ask

    let
        d       = edDesign   c
        currSty = dCurrLnMod d
        tW      = tabWidth c
        extTab  = padLeft tW (dTabExt d) $ dTabStr d

    return ( iff (r == fst pos && hl /= HSelected && not (readOnly st))
                 (combineAttrs currSty)
           $ iff br (combineAttrs $ dBrMod d)
           $ case hl of
                  HSelected -> lookupJustDef def HSelected $ dHLStyles   d
                  _         -> lookupJustDef def Whitesp   $ dCharStyles d
           , drop (length extTab - (tW - (n-1) `mod` tW)) extTab
           )

charRep br hl pos _ ' ' = do
    (r, _) <- getCursor
    st     <- get
    d      <- edDesign <$> ask

    return ( iff (r == fst pos && hl /= HSelected && not (readOnly st))
                 (combineAttrs $ dCurrLnMod d)
           $ iff br (combineAttrs $ dBrMod d)
           $ lookupJustDef def hl (dHLStyles d)
           , " "
           )

charRep br hl pos _ c = do
    (r, _) <- getCursor
    st     <- get
    d      <- edDesign <$> ask

    return ( iff (r == fst pos && hl /= HSelected && not (readOnly st))
                 (combineAttrs $ dCurrLnMod d)
           $ iff br (combineAttrs $ dBrMod d)
           $ lookupJustDef
                (lookupJustDef def (charClass c) (dCharStyles d))
                hl
                    (dHLStyles d)
           , if charClass c == Unprintable
                then "?#" ++ showHex (ord c) ";"
                else [c]
           )



-- | Returns the visual representation of a line with a given line number.
lineRep :: Int -> String -> WSEdit Image
lineRep lNo str = do
    st <- get
    maySel <- getSelBounds
    mayBr  <- getCurrBracket

    let
        fmt = atDef [] (map fst $ rangeCache st) (length (rangeCache st) - lNo)

        f :: ([Snippet], Int, Int) -> Char -> WSEdit ([Snippet], Int, Int)
        f (im, tPos, vPos) c = do
            i <- charRep (fromMaybe False $ fmap (`inBracketHL` (lNo, tPos)) mayBr)
                         (case (maySel, lookupBy (`inRange` tPos) fmt) of
                               (Just (sS, sE), _     )
                                    | sS <= (lNo, tPos)
                                         && (lNo, tPos) <= sE
                                    -> HSelected

                               (_            , Just s) -> s

                               _    | otherwise
                                    -> HNone
                         )
                         (lNo, tPos)
                         vPos
                         c

            return (i:im, tPos + 1, vPos + length (snd i))

    (r,_ ,_) <- foldM f ([], 1, 1) str

    return $ horizCat
           $ map (uncurry string)
           $ groupSnippet Nothing
           $ reverse r

    where
        groupSnippet :: Maybe Snippet -> [Snippet] -> [Snippet]
        groupSnippet  Nothing      []             = []
        groupSnippet (Just x     ) []             = [x]
        groupSnippet  Nothing      (x       :xs ) = groupSnippet (Just x) xs
        groupSnippet (Just (a, s)) ((xa, xs):xxs)
            | a == xa   =          groupSnippet (Just (a , s ++ xs)) xxs
            | otherwise = (a, s) : groupSnippet (Just (xa,      xs)) xxs

        inBracketHL :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
        inBracketHL ((a, b), (c, d)) (x, y)
            | a == x && x == c = (b,d        ) `inRange` y
            | a == x           = (b, maxBound) `inRange` y
            |           x == c = (0, d       ) `inRange` y
            | a <  x && x <  c = y == 1
            | otherwise        = False



-- | Returns the textual position of the cursor in a line, given its visual one.
visToTxtPos :: String -> Int -> WSEdit Int
visToTxtPos = pos 1
    where
        pos :: Int -> String -> Int -> WSEdit Int
        pos _   []     _            = return 1
        pos col _      c | col == c = return 1
        pos col _      c | col >  c = return 0
        pos col (x:xs) c            = do
            w <- charWidth col x
            (+ 1) <$> pos (col + w) xs c


-- | Returns the visual position of the cursor in a line, given its textual one.
txtToVisPos :: String -> Int -> WSEdit Int
txtToVisPos txt n = (+1) <$> stringWidth 1 (take (n - 1) txt)



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

    offset <- txtToVisPos currLine c

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
    curC <- txtToVisPos currLn curC_

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
        $ string (dColNoFormat d)
            ( replicate (lNoWidth + 3) ' '
           ++ take (txtCols + 1)
                   ( drop scrollCols
                   $ (' ':)
                   $ concatMap ( padRight (dColNoInterval d) ' '
                               . show
                               )
                     [1, dColNoInterval d + 1 ..]
                   )
            )
       <-> string (dFrameFormat d)
            ( replicate (lNoWidth + 2) ' '
           ++ "+"
           ++ take (txtCols + 1)
                  ( drop scrollCols
                  $ ('-':)
                  $ cycle
                  $ 'v' : replicate (dColNoInterval d - 1) '-'
                  )
           ++ "+-"
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
         <|> string (case () of
                    _ | r == n && not (readOnly s)   -> combineAttrs (dCurrLnMod    d)
                                                                     (dLineNoFormat d)
                      | n `mod` dLineNoInterv d == 0 ->               dLineNoFormat d
                      | otherwise                    ->               dFrameFormat  d
                    )
             ( padLeft lNoWidth ' '
             $ case () of
                _ | n > B.length (edLines s)               -> ""
                  | n `mod` dLineNoInterv d == 0 || r == n -> show n
                  | otherwise                              -> "·"
             )
         <|> string (dFrameFormat d) " |"



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
                ( replicate (lNoWidth + 2) ' '
               ++ "+-----------+"
               ++ replicate (txtCols - 11) '-'
               ++ "+-"
                )
          <-> (  string (dFrameFormat d)
                (replicate lNoWidth ' ' ++ "  | ")
             <|> string (dFrameFormat d)
                ( (if replaceTabs s
                      then "SPC "
                      else "TAB "
                  )
               ++ (if overwrite s
                      then "OVR "
                      else "INS "
                  )
               ++ (if readOnly s
                      then "R "
                      else "W "
                  )
               ++ "| "
                )
             <|> string (dFrameFormat d)
                ( fname s
               ++ (if changed s
                      then "*"
                      else " "
                  )
               ++ if readOnly s
                     then ""
                     else ( case markPos s of
                                 Nothing -> ""
                                 Just  m -> show m
                                         ++ " -> "
                          )
               ++ show (r, c)
               ++ " "
                )
             <|> string (dStatusFormat d) (padRight nCols ' ' $ status s)
              )



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

    (scrollRows, scrollCols) <- getOffset
    (   txtRows, txtCols   ) <- getViewportDimensions
    (cR        , _         ) <- getCursor

    txt <- mapM (\(n, (b, l)) -> lineRep n l >>= \l' -> return (b, l'))
         $ zip [1 + scrollRows ..]
         $ B.sub scrollRows (scrollRows + txtRows - 1)
         $ edLines s

    return $ pad (lNoWidth + 3) 2 0 0
           $ cropRight (txtCols + 1)  -- +1 to compensate for the leading blank
           $ vertCat
           $ map (\(l, (b, ln)) -> (if b
                                       then (char (dJumpMarkFmt  d) '•' <|>)
                                       else (char (dLineNoFormat d) ' ' <|>)
                                   )
                                 $ translateX (-scrollCols)
                                 $ pad 0 0 (scrollCols + 1) 0
                                 $ (if drawBg c
                                       then id
                                       else (<|> char ( (if l == cR && not (readOnly s)
                                                            then combineAttrs $ dCurrLnMod d
                                                            else id
                                                        )
                                                      $ lookupJustDef def Whitesp
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
          ++                                  ['-']
          ++ replicate (endProg - stProg - 2 ) '|'
          ++                                  ['-']
          ++ replicate (nRows - endProg      ) ' '

    where
        repl :: (EdDesign, EdState, Int, [Int]) -> (Int, Char) -> Image
        repl (d, s, cProg, marksAt) (n, c) =
            char (dFrameFormat d) '|' <|> case () of
                _ | readOnly s       -> char (dFrameFormat  d)  c
                  | n == cProg       -> char (dLineNoFormat d) '<'
                  | n `elem` marksAt -> char (dJumpMarkFmt  d) '•'
                  | otherwise        -> char (dFrameFormat  d)  c



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

    liftIO $ update (vtyObj c)
             Picture
                { picCursor     = if readOnly s || ru + rd + cl + cr > 0
                                     then NoCursor
                                     else uncurry Cursor $ swap cursor
                , picLayers     = [ frame
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

    liftIO $ update v $ picForImage $ char def ' '
