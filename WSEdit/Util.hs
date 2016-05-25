module WSEdit.Util where

import Data.Char        (isAlphaNum)
import Data.List        (inits, intersect, tails)
import Safe             (headMay, lastDef)
import System.Directory (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)
import Text.Show.Pretty (ppShow)



withPair :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
withPair fa fb (a, b) = (fa a, fb b)


withFst :: (a -> b) -> (a, c) -> (b, c)
withFst f = withPair f id


withSnd :: (b -> c) -> (a, b) -> (a, c)
withSnd = withPair id



padLeft :: Int -> a -> [a] -> [a]
padLeft n c s = replicate (n - length s) c ++ s


padRight :: Int -> a -> [a] -> [a]
padRight n c s = s ++ replicate (n - length s) c


withN :: Int -> (a -> a) -> [a] -> [a]
withN n f (x:xs) | n > 0 = x : withN (n - 1) f xs
withN 0 f (x:xs)         = f x : xs
withN _ _ _              = error "withN: index out of bounds."


insertBefore :: Int -> a -> [a] -> [a]
insertBefore n el (x:xs) | n >  0 = x : insertBefore (n - 1) el xs
insertBefore 0 el (x:xs)          = el : x : xs
insertBefore n el []     | n >= 0 = [el]
insertBefore _ _  _               = error "insertBefore: index out of bounds."


delN :: Int -> [a] -> [a]
delN n (x:xs) | n > 0 = x : delN (n-1) xs
delN 0 (_:xs)         = xs
delN n arr            = error $ "delN: index out of bounds (n = "
                             ++ show n
                             ++ ", length arr = "
                             ++ show (length arr)
                             ++ ")"


{-
mergeUpN :: Int -> [[a]] -> [[a]]
mergeUpN n (x:  xs) | n > 1 = x : mergeUpN (n-1) xs
mergeUpN 1 (x:y:xs)         = (x ++ y) : xs
mergeUpN n arr              = error $ "mergeUpN: index out of bounds (n = "
                                   ++ show n
                                   ++ ", length arr = "
                                   ++ show (length arr)
                                   ++ ")"
-}


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



mayReadFile :: FilePath -> IO (Maybe String)
mayReadFile f = do
    b <- doesFileExist f
    if b
       then Just <$> readFile f
       else return Nothing





data CharClass = Whitesp
               | Digit
               | Lower
               | Upper
               | Bracket
               | Operator
               | Special
    deriving (Eq)



charClass :: Char -> CharClass
charClass c | '0' <= c && c <= '9' = Digit
charClass c | 'a' <= c && c <= 'z' = Lower
charClass c | 'A' <= c && c <= 'Z' = Upper

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


isIdentifierChar :: Char -> Bool
isIdentifierChar c = case charClass c of
                          Whitesp  -> False
                          Digit    -> True
                          Lower    -> True
                          Upper    -> True
                          Bracket  -> False
                          Operator -> False
                          Special  -> True


isIdentifier :: String -> Bool
isIdentifier s = all isIdentifierChar s
              && not (all (\c -> charClass c == Digit) s)
              && length s >= 3



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



getExt :: String -> String
getExt = reverse
       . takeWhile isAlphaNum
       . reverse



getKeywordAtCursor :: Int -> String -> Maybe String
getKeywordAtCursor c s = headMay $ filter isIdentifier $ tails $ take (c - 1) s



longestCommonPrefix :: [String] -> String
longestCommonPrefix [] = ""
longestCommonPrefix s  = lastDef ""
                       $ foldl1 intersect
                       $ map inits s
