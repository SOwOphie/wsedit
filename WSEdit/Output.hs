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
import Data.Char                (toLower)
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
import Safe                     (lookupJustDef, minimumNote)

import WSEdit.Data              ( EdConfig ( drawBg, edDesign, escape, keywords
                                           , lineComment, strDelim, tabWidth
                                           , vtyObj
                                           )
                                , EdDesign ( dBGChar, dBGFormat, dCharStyles
                                           , dColChar, dColNoFormat
                                           , dColNoInterval, dCurrLnMod
                                           , dFrameFormat, dHLStyles
                                           , dLineNoFormat, dLineNoInterv
                                           , dJumpMarkFmt, dStatusFormat
                                           , dTabExt, dTabStr
                                           )
                                , EdState ( changed, edLines, fname, markPos
                                          , readOnly, replaceTabs, scrollOffset
                                          , searchTerms, status
                                          )
                                , HighlightMode ( HComment, HKeyword, HNone
                                                , HSearch, HSelected, HString
                                                )
                                , WSEdit
                                , getCursor, getDisplayBounds, getOffset
                                , getSelBounds
                                )
import WSEdit.Util              ( CharClass (Unprintable, Whitesp)
                                , charClass, findDelimBy, findInStr
                                , findIsolated, padLeft, padRight, withPair
                                )

import qualified WSEdit.Buffer as B



fqn :: String -> String
fqn = ("WSEdit.Output." ++)





-- | Returns the display width of a given char in a given column.
charWidth :: Int -> Char -> WSEdit Int
charWidth n '\t' = (\w -> w - (n-1) `mod` w) . tabWidth <$> ask
charWidth _ _    = return 1

-- | Returns the display width of a given string starting at a given column.
stringWidth :: Int -> String -> WSEdit Int
stringWidth n = foldM (\n' c -> (+ n') <$> charWidth (n' + 1) c) $ n - 1




-- | Intermediate format for rendering.
type Snippet = (Attr, String)


-- | Returns the visual representation of a character at a given buffer position
--   and in a given display column.
charRep :: HighlightMode -> (Int, Int) -> Int -> Char -> WSEdit Snippet
charRep hl pos n '\t' = do
    (r, _) <- getCursor
    st     <- get
    c      <- ask

    let
        d       = edDesign   c
        currSty = dCurrLnMod d
        tW      = tabWidth c
        extTab  = padLeft tW (dTabExt d) $ dTabStr d

    return ( (if r == fst pos
                   && hl /= HSelected
                   && not (readOnly st)
                 then currSty
                 else id
             ) $ case hl of
                      HSelected -> lookupJustDef def HSelected $ dHLStyles   d
                      _         -> lookupJustDef def Whitesp   $ dCharStyles d
           , drop (length extTab - (tW - (n-1) `mod` tW)) extTab
           )

charRep hl pos _ c = do
    (r, _) <- getCursor
    st     <- get
    d      <- edDesign <$> ask

    return ( (if r == fst pos
                   && hl /= HSelected
                   && not (readOnly st)
                 then dCurrLnMod d
                 else id
             ) $ lookupJustDef
                    (lookupJustDef def (charClass c) (dCharStyles d))
                    hl
                    (dHLStyles d)
           , if charClass c == Unprintable
                then "?"
                else [c]
           )



-- | Returns the visual representation of a line with a given line number.
lineRep :: Int -> String -> WSEdit Image
lineRep lNo str = do
    conf <- ask
    st <- get
    maySel <- getSelBounds

    let
        -- Initial list of comment starting points
        comL :: [Int]
        comL = map (+1)
             $ concatMap (flip findInStr str)
             $ lineComment conf

        -- List of string bounds
        strL :: [(Int, Int)]
        strL = map (withPair (+1) (+1))
             $ findDelimBy (escape conf) (strDelim conf) str

        -- List of keyword bounds
        kwL :: [(Int, Int)]
        kwL = concatMap (\k -> map (\p -> (p + 1, p + length k))
                            $ findIsolated k str
                        )
            $ keywords conf

        -- List of search terms
        sL :: [(Int, Int)]
        sL = concatMap (\k -> map (\p -> (p + 1, p + length k))
                           $ findInStr ( map toLower k )
                                       $ map toLower str
                       )
            $ searchTerms st

        -- List of comment starting points, minus those that are inside a string
        comL' :: [Int]
        comL' = filter (\c -> not $ any (\r -> inRange r c) strL) comL

        -- First comment starting point in the line
        comAt :: Maybe Int
        comAt = if null comL'
                   then Nothing
                   else Just $ minimumNote (fqn "lineRep") comL



        f :: ([Snippet], Int, Int) -> Char -> WSEdit ([Snippet], Int, Int)
        f (im, tPos, vPos) c = do
            i <- charRep
                    (case maySel of
                          Just (sS, sE) | sS <= (lNo, tPos)
                                             && (lNo, tPos) <= sE -> HSelected
                          _ | any (flip inRange tPos) sL          -> HSearch
                          _ | fromMaybe maxBound comAt <= tPos    -> HComment
                          _ | any (flip inRange tPos) strL        -> HString
                          _ | any (flip inRange tPos) kwL         -> HKeyword
                          _ | otherwise                           -> HNone
                    ) (lNo, tPos) vPos c

            return (i:im, tPos + 1, vPos + length i)

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
txtToVisPos txt n =  (+1)
                 <$> ( stringWidth 1
                     $ take (n - 1)
                       txt
                     )



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
              .  B.curr
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
        currLn       = snd $ B.curr $ edLines s
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
        $ ( string (dColNoFormat d)
             $ (replicate (lNoWidth + 3) ' ')
            ++ ( take (txtCols + 1)
               $ drop scrollCols
               $ (' ':)
               $ concat
               $ map ( padRight (dColNoInterval d) ' '
                     . show
                     )
                 [1, dColNoInterval d + 1 ..]
               )
          )
       <-> string (dFrameFormat d)
            ( replicate (lNoWidth + 2) ' '
           ++ "+"
           ++ ( take (txtCols + 1)
              $ drop scrollCols
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
           $ map (\n -> char (dLineNoFormat d) ' '
                    <|> ( string (if r == n && not (readOnly s)
                                     then dCurrLnMod d $ dLineNoFormat d
                                     else if n `mod` dLineNoInterv d == 0
                                             then dLineNoFormat d
                                             else dFrameFormat  d
                                 )
                        $ padLeft lNoWidth ' '
                        $ if n <= B.length (edLines s)
                             then if n `mod` dLineNoInterv d == 0
                                    || r == n
                                     then show n
                                     else "·"
                             else ""
                        )
                    <|> string (dFrameFormat d) " |"
                 )
             [scrollRows + 1, scrollRows + 2 .. scrollRows + txtRows]



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
               ++ "+-------+"
               ++ replicate (txtCols - 7) '-'
               ++ "+-"
                )
          <-> (  string (dFrameFormat d)
                (replicate lNoWidth ' ' ++ "  | ")
             <|> string (dFrameFormat d)
                ( (if replaceTabs s
                      then "SPC "
                      else "TAB "
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
                                       else (<|> char ( (if l == cR
                                                            then dCurrLnMod d
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

    (nRows, nCols     ) <- getViewportDimensions
    (_    , scrollCols) <- getOffset

    cursor   <- getCursor >>= toCursorDispPos
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
           $ zip [2..]
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
        nLns    = B.length $ edLines s

        -- Relative position of the viewport's upper edge
        stProg  = floor $ (fromIntegral (nRows - 1     ))
                        * (fromIntegral  sRows          )
                        / (fromIntegral nLns :: Rational)

        -- Position of the viewport's lower edge
        endProg = floor $ 1
                        + (fromIntegral (nRows - 1     ))
                        * (fromIntegral (sRows + nRows ))
                        / (fromIntegral nLns :: Rational)

        -- Position of the cursor
        cProg   = floor $ (fromIntegral (nRows - 1     ))
                        * (fromIntegral  curR           )
                        / (fromIntegral nLns :: Rational)

    return $ pad (lNoWidth + 4 + nCols) 2 0 0
           $ cropBottom nRows
           $ vertCat
           $ map (\(n, c) -> char   (dFrameFormat d) '|'
                         <|> if n /= cProg || readOnly s
                                then char (               dFrameFormat  d) c
                                else char (dCurrLnMod d $ dLineNoFormat d) '<'
                 )
           $ zip [(0 :: Int)..]
           $ replicate  stProg                 ' '
          ++                                  ['-']
          ++ replicate (endProg - stProg - 2 ) '|'
          ++                                  ['-']
          ++ replicate (nRows - endProg      ) ' '



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
                { picCursor     = if (readOnly s || ru + rd + cl + cr > 0)
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
