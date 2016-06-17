module WSEdit.Util
    ( withPair
    , withFst
    , withSnd
    , padLeft
    , padRight
    , withN
    , insertBefore
    , delN
    , dump
    , mayReadFile
    , CharClass (..)
    , charClass
    , isIdentifierChar
    , isIdentifier
    , wordsPlus
    , getExt
    , getKeywordAtCursor
    , longestCommonPrefix
    , checkClipboardSupport
    , findInStr
    ) where

import Control.Exception (SomeException, try)
import Data.Char         (isAlphaNum, isControl, isMark, isPrint)
import Data.List         (inits, intersect, tails)
import Safe              (headMay, lastDef)
import System.Directory  (doesFileExist)
import System.Exit       (ExitCode (ExitSuccess))
import System.IO.Unsafe  (unsafePerformIO)
import System.Process    (readProcessWithExitCode)
import Text.Show.Pretty  (ppShow)



-- | Applies a separate function to each member of a pair.
withPair :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
withPair fa fb (a, b) = (fa a, fb b)


-- | Applies a function to the first member of a pair.
withFst :: (a -> b) -> (a, c) -> (b, c)
withFst f = withPair f id


-- | Applies a function to the second member of a pair.
withSnd :: (b -> c) -> (a, b) -> (a, c)
withSnd = withPair id



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


-- | Appends the given string as well as a pretty-printed 'show' of the second
--   parameter to the file @dmp@ in the current working directory. Occasionally
--   crashes on weird I/O race conditions. Use for debugging purposes only.
dump :: (Show a) => String -> a -> a
dump s x = x
     `seq` unsafePerformIO (appendFile "dmp" $ s
                                            ++ ":\n"
                                            ++ ( unlines
                                               $ map ("\t"++)
                                               $ lines
                                               $ ppShow x
                                               )
                                            ++ "\n\n"
                           )
     `seq` x



-- | Attempt to read a file, or return 'Nothing' if it fails.
mayReadFile :: FilePath -> IO (Maybe String)
mayReadFile f = do
    b <- doesFileExist f
    if b
       then Just <$> readFile f
       else return Nothing





-- | Character classes.
data CharClass = Whitesp     -- ^ Whitespace (@"\t\n\r "@)
               | Digit       -- ^ Digit (@['0'..'9']@)
               | Lower       -- ^ Lowercase letter (@'_':['a'..'z']@)
               | Upper       -- ^ Uppercase letter (@['A'..'Z']@)
               | Bracket     -- ^ Bracket (@"()[]{}"@)
               | Operator    -- ^ Operator (@"+-*/\\$!'\"%&^=?´`~#.:,;<>|@"@)
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
                       $ foldl1 intersect
                       $ map inits s



-- | Checks whether either `xclip` or `xsel` is ready for action.
checkClipboardSupport :: IO Bool
checkClipboardSupport = do
    r1 <- try $ readProcessWithExitCode "xclip" ["-o"] ""
    r2 <- try $ readProcessWithExitCode "xsel"  []     ""
    return $ ok r1 || ok r2

    where
        ok :: Either SomeException (ExitCode, String, String) -> Bool
        ok (Right (ExitSuccess, _, _)) = True
        ok _                           = False



-- | Find the position of the first string within the second one.
findInStr :: (Eq a) => [a] -> [a] -> Maybe Int
findInStr []   _                          = Just 0
findInStr _    []                         = Nothing
findInStr pat  str@(_:xs) | match pat str = Just 0
                          | otherwise     = (+1) <$> findInStr pat xs
    where
        match :: (Eq a) => [a] -> [a] -> Bool
        match (p:ps) (y:ys) | p == y = match ps ys
        match []     _               = True
        match _      _               = False
