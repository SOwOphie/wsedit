{-# LANGUAGE DeriveFunctor
           , LambdaCase
           #-}

module WSEdit.Util
    ( withPair
    , withFst
    , withSnd
    , withPairM
    , withFstM
    , withSndM
    , padLeft
    , padRight
    , withN
    , insertBefore
    , delN
    , rotateL
    , rotateR
    , chunk
    , chunkWords
#ifdef dev
    , dump
    , timed
#endif
    , mayReadFile
    , listDirectoryDeep
    , CharClass
        (..)
    , charClass
    , isIdentifierChar
    , isIdentifier
    , wordsPlus
    , linesPlus
    , unlinesPlus
    , getExt
    , getKeywordAtCursor
    , longestCommonPrefix
    , checkClipboardSupport
    , findInStr
    , findIsolated
    , findDelimBy
    , lookupBy
    , readEncFile
    , combineAttrs
    , iff
    , matchGlob
    ) where



#ifdef dev
import Control.DeepSeq
    ( NFData
    , force
    )
#endif
import Codec.Text.Detect
    ( detectEncodingName
    )
import Control.Exception
    ( SomeException
#ifdef dev
    , evaluate
#endif
    , try
    )
import Data.Char
    ( chr
    , isAlphaNum
    , isControl
    , isMark
    , isPrint
    )
import Data.List
    ( inits
    , intercalate
    , intersect
    , tails
    )
import Data.Maybe
    ( isJust
    )
#ifdef dev
import Data.Time.Clock
    ( diffTimeToPicoseconds
    , diffUTCTime
    , getCurrentTime
    )
#endif
import Graphics.Vty
    ( Attr
        ( Attr
        , attrStyle
        , attrForeColor
        , attrBackColor
        )
    , MaybeDefault
        ( Default
        , KeepCurrent
        , SetTo
        )
    )
import Safe
    ( foldl1Note
    , headMay
    , lastDef
    , lastNote
    )
import System.Directory
    ( doesDirectoryExist
    , doesFileExist
#ifdef dev
    , getHomeDirectory
#endif
    , listDirectory
    )
import System.Environment
    ( lookupEnv
    )
import System.Exit
    ( ExitCode
        ( ExitSuccess
        )
    )
import System.Info
    ( os
    )
import System.IO
    ( IOMode
        ( ReadMode
        )
    , hSetEncoding
    , hSetNewlineMode
    , mkTextEncoding
    , universalNewlineMode
    , char8
    , withFile
    )
import System.IO.Strict
    ( hGetContents
    )
#ifdef dev
import System.IO.Unsafe
    ( unsafePerformIO
    )
#endif
import System.Process
    ( readProcessWithExitCode
    )
#ifdef dev
import Text.Show.Pretty
    ( ppShow
    )
#endif

import qualified Data.ByteString.Lazy as S



fqn :: String -> String
fqn = ("WSEdit.Util." ++)





-- | Applies a separate function to each member of a pair.
withPair :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
withPair fa fb (a, b) = (fa a, fb b)


-- | Applies a function to the first member of a pair.
withFst :: (a -> b) -> (a, c) -> (b, c)
withFst f = withPair f id


-- | Applies a function to the second member of a pair.
withSnd :: (b -> c) -> (a, b) -> (a, c)
withSnd = withPair id



-- | Applies a separate monadic function to each member of a pair.
withPairM :: (Monad m) => (a -> m b) -> (c -> m d) -> (a, c) -> m (b, d)
withPairM fa fb (a, b) = do
    ra <- fa a
    rb <- fb b
    return (ra, rb)


-- | Applies a monadic function to the first member of a pair.
withFstM :: (Monad m) => (a -> m b) -> (a, c) -> m (b, c)
withFstM f = withPairM f return


-- | Applies a monadic function to the second member of a pair.
withSndM :: (Monad m) => (b -> m c) -> (a, b) -> m (a, c)
withSndM = withPairM return



-- | Pads a list to the left, using the given element.
padLeft :: Int -> a -> [a] -> [a]
padLeft n c s = replicate (n - length s) c ++ s


-- | Pads a list to the right, using the given element.
padRight :: Int -> a -> [a] -> [a]
padRight n c s = s ++ replicate (n - length s) c


-- | Applies a function to the n-th element of a list.
withN :: Int -> (a -> a) -> [a] -> [a]
withN n f (x:xs) | n > 0 = x : withN (n - 1) f xs
withN 0 f (x:xs)         = f x : xs
withN _ _ _              = error "withN: index out of bounds."


-- | Inserts an element before the specified index.
insertBefore :: Int -> a -> [a] -> [a]
insertBefore n el (x:xs) | n >  0 = x : insertBefore (n - 1) el xs
insertBefore 0 el (x:xs)          = el : x : xs
insertBefore n el []     | n >= 0 = [el]
insertBefore _ _  _               = error "insertBefore: index out of bounds."


-- | Deletes the n-th element of a list,
delN :: Int -> [a] -> [a]
delN n (x:xs) | n > 0 = x : delN (n-1) xs
delN 0 (_:xs)         = xs
delN n arr            = error $ "delN: index out of bounds (n = "
                             ++ show n
                             ++ ", length arr = "
                             ++ show (length arr)
                             ++ ")"


-- | Rotate the given list one position to the left. Note that
--  `rotateL _|_ == tail _|_`.
rotateL :: [a] -> [a]
rotateL []     = []
rotateL (x:xs) = xs ++ [x]


-- | Rotate the given list one position to the right. Note that
--   `head (rotateR _|_) == _|_`.
rotateR :: [a] -> [a]
rotateR [] = []
rotateR l  = lastNote (fqn "rotateR") l : init l


-- | Breaks a list into chunks of the given size, with the last one possibly
--   being smaller.
chunk :: Int -> [a] -> [[a]]
chunk n l | length l < n = [l]
          | otherwise    = take n l : chunk n (drop n l)


-- | Similar to `chunk`, but only breaks a string on word borders.
chunkWords :: Int -> String -> [String]
chunkWords _ [] = []
chunkWords n s  =
    let
        w  = words s
        x  = unwords
           $ snd
           $ foldl (\(len, ls) el -> if len + length el + 1 <= n
                                        then (len + length el + 1, ls ++ [el])
                                        else (len, ls)
                   ) (0, []) w

        xs = chunkWords n $ drop (length x + 1) s
    in
        x:xs


#ifdef dev
-- | Appends the given string as well as a pretty-printed 'show' of the second
--   parameter to the file @dmp@ in the current working directory. Occasionally
--   crashes on weird I/O race conditions. Use for debugging purposes only.
dump :: (Show a) => String -> a -> a
dump s x = unsafePerformIO $ do
    sh <- evaluate $ force $ ppShow x
    h  <- getHomeDirectory

    appendFile (h ++ "/dmp")
        $ s
       ++ ":\n"
       ++ unlines (map ("\t"++) $ lines sh)
       ++ "\n\n"

    return x

-- | Forces execution and `dump`s the elapsed time.
timed :: (NFData a) => String -> a -> a
timed s a = unsafePerformIO $ do
    t1 <- getCurrentTime
    a' <- evaluate $ force a
    t2 <- getCurrentTime
    h <- getHomeDirectory
    appendFile (h ++ "/dmp")
        $ s
       ++ ": "
       ++ show ( ( diffTimeToPicoseconds
                 $ realToFrac
                 $ diffUTCTime t2 t1)
               `div` 1000000000
               )
       ++ "ms\n\n"

    return a'

#endif


-- | Attempt to read a file, or return 'Nothing' if it fails.
mayReadFile :: FilePath -> IO (Maybe String)
mayReadFile f = do
    b <- doesFileExist f
    if b
       then try (readFile f)
            >>= \case
                Right s -> return $ Just s
                Left  e -> const (return Nothing) (e :: SomeException)
       else return Nothing



-- | Lists all files in a given directory recursively, following symlinks.
listDirectoryDeep :: FilePath -> IO [FilePath]
listDirectoryDeep p = listDirectory p >>= fmap concat . mapM (\s -> do
        let
            name = p ++ "/" ++ s

        dir  <- doesDirectoryExist name
        file <- doesFileExist      name

        case (dir , file) of
             (True, _   ) -> listDirectoryDeep name
             (_   , True) -> return [name]
             (_   , _   ) -> return []
    )




-- | Character classes.
data CharClass = Whitesp     -- ^ Whitespace (@"\\t\\n\\r "@)
               | Digit       -- ^ Digit (@['0'..'9']@)
               | Lower       -- ^ Lowercase letter (@'_':['a'..'z']@)
               | Upper       -- ^ Uppercase letter (@['A'..'Z']@)
               | Bracket     -- ^ Bracket (@"()[]{}"@)
               | Operator    -- ^ Operator (@"+-*/\\$!'\"%&^=?´`~#.:,;<>|\@"@)
               | Unprintable -- ^ Unprintable or unicode mark characters.
               | Special     -- ^ Everything else
    deriving (Eq, Read, Show)



-- | Returns the character class a char belongs into.
charClass :: Char -> CharClass
charClass c | '0' <= c && c <= '9' = Digit
charClass c | 'a' <= c && c <= 'z' = Lower
charClass c | 'A' <= c && c <= 'Z' = Upper

charClass c |       isControl c = Unprintable
charClass c |       isMark    c = Unprintable
charClass c | not $ isPrint   c = Unprintable

charClass '_'  = Lower

charClass '\t' = Whitesp
charClass '\n' = Whitesp
charClass '\r' = Whitesp
charClass ' '  = Whitesp

charClass '('  = Bracket
charClass ')'  = Bracket
charClass '['  = Bracket
charClass ']'  = Bracket
charClass '{'  = Bracket
charClass '}'  = Bracket

charClass '+'  = Operator
charClass '-'  = Operator
charClass '*'  = Operator
charClass '/'  = Operator
charClass '\\' = Operator
charClass '$'  = Operator
charClass '!'  = Operator
charClass '\'' = Operator
charClass '"'  = Operator
charClass '%'  = Operator
charClass '&'  = Operator
charClass '^'  = Operator
charClass '='  = Operator
charClass '?'  = Operator
charClass '´'  = Operator
charClass '`'  = Operator
charClass '~'  = Operator
charClass '#'  = Operator
charClass '.'  = Operator
charClass ':'  = Operator
charClass ','  = Operator
charClass ';'  = Operator
charClass '<'  = Operator
charClass '>'  = Operator
charClass '|'  = Operator
charClass '@'  = Operator

charClass  _   = Special


-- | Returns whether the character is probably part of a identifier.
isIdentifierChar :: Char -> Bool
isIdentifierChar c = case charClass c of
                          Whitesp     -> False
                          Digit       -> True
                          Lower       -> True
                          Upper       -> True
                          Bracket     -> False
                          Operator    -> False
                          Unprintable -> False
                          Special     -> True


-- | Returns whether the string is probably an identifier.
isIdentifier :: String -> Bool
isIdentifier s = all isIdentifierChar s
              && not (all (\c -> charClass c == Digit) s)
              && length s >= 2



-- | Prelude 'words' on steroids, uses 'isIdentifier' and 'isIdentifierChar' to
--   pick out identifiers from a possibly quite cryptic source code snippet.
wordsPlus :: String -> [String]
wordsPlus s =
    let
        s'       = dropWhile (\c -> not (isIdentifierChar c) || charClass c == Digit) s
        (k, s'') = span isIdentifierChar s'
    in
        if null k
           then []
           else if isIdentifier k
                   then k : wordsPlus s''
                   else     wordsPlus s''



-- | Like `lines`, but with @\n@ as a separator, not terminator.
linesPlus :: String -> [String]
linesPlus = lp ""
    where
        lp :: String -> String -> [String]
        lp accum []        = [accum]
        lp accum ('\n':xs) = accum : lp []             xs
        lp accum ( x  :xs) =         lp (accum ++ [x]) xs



-- | Like `unlines`, but with @\n@ as a separator, not terminator.
unlinesPlus :: [String] -> String
unlinesPlus = intercalate "\n"



-- | Returns the file extension of a given path. Undefined output for files
--   without extensions.
getExt :: FilePath -> String
getExt = reverse
       . takeWhile isAlphaNum
       . reverse



-- | Given a 1-based cursor position and the current line, returns the keyword
--   at the cursor position, if there is one.
getKeywordAtCursor :: Int -> String -> Maybe String
getKeywordAtCursor c s = headMay $ filter isIdentifier $ tails $ take (c - 1) s



-- | Returns the longest common prefix of all list elements.
longestCommonPrefix :: (Eq a) => [[a]] -> [a]
longestCommonPrefix [] = []
longestCommonPrefix s  = lastDef []
                       $ foldl1Note (fqn "longestCommonPrefix") intersect
                       $ map inits s



-- | Checks whether either `xclip` or `xsel` is ready for action.  Always
--   returns `True` on Darwin, since HClip uses built-in features there.
checkClipboardSupport :: IO Bool
checkClipboardSupport =
    if os == "darwin"
       then return True
       else do
            r1   <- try $ readProcessWithExitCode "xclip" ["-version"] ""
            r2   <- try $ readProcessWithExitCode "xsel"  []           ""
            disp <- lookupEnv "DISPLAY"
            return $ (ok r1 || ok r2) && isJust disp

    where
        ok :: Either SomeException (ExitCode, String, String) -> Bool
        ok (Right (ExitSuccess, _, _)) = True
        ok _                           = False



-- | Find the position of the first string within the second one.
findInStr :: (Eq a) => [a] -> [a] -> [Int]
findInStr []      _      = error "findInStr: empty pattern"
findInStr _       []     = []
findInStr pat str@(_:xs)
    | match pat str = 0 : map (+1) (findInStr pat xs)
    | otherwise     =     map (+1) (findInStr pat xs)

    where
        match :: (Eq a) => [a] -> [a] -> Bool
        match (p:ps) (y:ys) | p == y = match ps ys
        match []     _               = True
        match _      _               = False



-- | Find the position of the first string as a whole word within the second one.
findIsolated :: String -> String -> [Int]
findIsolated []  _  = error "findIsolated: empty pattern"
findIsolated _   [] = []
findIsolated pa str
    | match pa str = 0 : findIs pa str
    | otherwise    =     findIs pa str

    where
        findIs :: String -> String -> [Int]
        findIs []  _      = error "findIsolated: empty pattern"
        findIs _   []     = []
        findIs pat (x:xs)
            | isIdentifierChar x =     map (+1) $ findIs pat xs
            | match pat xs       = 1 : map (+1) ( findIs pat xs)
            | otherwise          =     map (+1) $ findIs pat xs

        match :: String -> String -> Bool
        match (p:ps) (y:ys) | p == y = match ps ys
        match []     []              = True
        match []     (y:_ )          = not $ isIdentifierChar y
        match _      _               = False



-- | Returns a list of subranges of a string which are delimited by one of the
--   given pairs, except after the given escape character.
findDelimBy :: Maybe Char -> [(Char, Char)] -> String -> [(Int, Int)]
findDelimBy _        _     []                = []
findDelimBy (Just c) delim (x:_:xs) | c == x = map (withPair (+2) (+2))
                                             $ findDelimBy (Just c) delim xs

findDelimBy mC       delim (x:  xs)          =
    case lookup x delim of
         Nothing -> map (withPair (+1) (+1)) $ findDelimBy mC delim xs
         Just  c ->
            case find mC c xs of
                 Nothing -> (0, 0) : map (withPair (+1) (+1))
                                         (findDelimBy mC delim xs)

                 Just  p -> (0, p+1) : map (withPair (+(p+2)) (+(p+2)))
                                        ( findDelimBy mC delim
                                        $ drop (p + 1) xs
                                        )
    where
        find :: Maybe Char -> Char -> String -> Maybe Int
        find _        _ []                   = Nothing
        find (Just e) c (y:_:ys) | e == y    = (+2) <$> find (Just e) c ys
        find mE       c (y  :ys) | c == y    = Just 0
                                 | otherwise = (+1) <$> find mE       c ys





-- | Generic version of `lookup`.
lookupBy :: (a -> Bool) -> [(a, b)] -> Maybe b
lookupBy _ []          = Nothing
lookupBy f ((k, v):xs) = if f k then Just v else lookupBy f xs





-- | Reads a file strictly and returns its text encoding, if applicable,
--   alongside its contents.
readEncFile :: FilePath -> IO (Maybe String, String)
readEncFile f = do
    raw <- S.readFile f

    withFile f ReadMode $ \h -> do
        hSetNewlineMode h universalNewlineMode

        case detectEncodingName raw of
            Nothing -> do
                hSetEncoding h char8
                s <- hGetContents h
                return (Nothing, s)

            Just en -> do
                e <- mkTextEncoding en
                hSetEncoding h e
                s <- hGetContents h
                return (Just en, dropWhile (`elem` [chr 0xFFFE, chr 0xFEFF]) s)





-- | Combines two vty attributes. Conflicts are resolved in a left-biased way.
combineAttrs :: Attr -> Attr -> Attr
combineAttrs a b = Attr
    { attrStyle     = combineMayDef (attrStyle     a) (attrStyle     b)
    , attrForeColor = combineMayDef (attrForeColor a) (attrForeColor b)
    , attrBackColor = combineMayDef (attrBackColor a) (attrBackColor b)
    }
    where
        combineMayDef :: MaybeDefault a -> MaybeDefault a -> MaybeDefault a
        combineMayDef (SetTo x)   _           = SetTo x
        combineMayDef _           (SetTo x)   = SetTo x
        combineMayDef KeepCurrent _           = KeepCurrent
        combineMayDef _           KeepCurrent = KeepCurrent
        combineMayDef _           _           = Default





-- | Applies the function if the first parameter is true, or 'id's otherwise.
iff :: Bool -> (a -> a) -> a -> a
iff True  f = f
iff False _ = id





-- | Test if a glob matches a string.
matchGlob :: String -> String -> Bool
matchGlob    []          []                 = True
matchGlob    ('\\':p:ps) (s:ss) | p == s    = matchGlob ps ss
matchGlob    ('?'   :ps) (_:ss)             = matchGlob ps ss
matchGlob    ['*']       _                  = True
matchGlob px@('*' :p:ps) (s:ss) | p == s    = matchGlob ps ss
                                           || matchGlob px ss
                                | otherwise = matchGlob px ss
matchGlob    (     p:ps) (s:ss) | p == s    = matchGlob ps ss
matchGlob    _           _                  = False
