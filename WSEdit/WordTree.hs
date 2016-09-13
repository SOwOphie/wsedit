module WSEdit.WordTree
    ( WordTree (..)
    , empty
    , addWord
    , fromList
    , toList
    , complete
    ) where


import Data.List  (sort)
import Data.Maybe (catMaybes)



-- | Autocomplete dictionary. 'Nothing' denotes the end of a valid word.
data WordTree = Node [Maybe (Char, WordTree)]
    deriving (Eq, Ord, Read, Show)



-- | Create an empty tree.
empty :: WordTree
empty = Node []



-- | Add a word to a tree. Adding a word multiple times is safe.
addWord :: String -> WordTree -> WordTree
addWord []     (Node l) = if Nothing `elem` l
                             then Node             l
                             else Node $ Nothing : l
addWord (x:xs) (Node l) =
    case lookup x $ catMaybes l of
        Nothing -> Node $ sort $ Just (x, addWord xs empty) : l
        Just n  -> Node $ sort $ Just (x, addWord xs n    )
                               : filter (/= Just (x, n)) l



-- | Create a tree from a list of words.
fromList :: [String] -> WordTree
fromList = foldl (flip addWord) empty


-- | Generate a sorted list of words from a tree.
toList :: WordTree -> [String]
toList (Node l) = concatMap majik l
    where
        majik :: Maybe (Char, WordTree) -> [String]
        majik Nothing       = [""]
        majik (Just (c, t)) = map (c:) $ toList t



-- | Get a list of autocomplete suggestions for a given prefix.
complete :: String -> WordTree -> [String]
complete []     t        = toList t
complete (x:xs) (Node l) =
    case lookup x $ catMaybes l of
            Nothing -> []
            Just t  -> map (x:) $ complete xs t
