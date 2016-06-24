module WSEdit.Buffer
    ( Buffer (..)
    , singleton
    , fromList
    , toList
    , length
    , sub
    , left
    , right
    , atMay
    , atDef
    , first
    , last
    , resembles
    , currPos
    , move
    , moveTo
    , forward
    , backward
    , toFirst
    , toLast
    , insertLeft
    , insertRight
    , dropLeft
    , deleteLeft
    , dropRight
    , deleteRight
    , withLeft
    , withLeftDef
    , withNLeft
    , withPos
    , withRight
    , withRightDef
    , withNRight
    , append
    , prepend
    , map
    ) where


import Data.Maybe (fromJust, fromMaybe)

import Prelude hiding (last, length, map)

import qualified Data.List as L
import qualified Prelude   as P
import qualified Safe      as S





-- | One-dimensional nonempty buffer with a current position.  Most operations
--   scale only with distance from the position, not with total buffer size.
data Buffer a = Buffer
    { prefix  :: [a]    -- ^ Elements in front of the position, in reversed order
    , prefLen :: Int    -- ^ Amount of elements in front of the current position
    , curr    :: a      -- ^ Currently focused element
    , suffix  :: [a]    -- ^ Elements after the position
    , sufLen  :: Int    -- ^ Amount of elements after the position
    }
    deriving ( Eq
             , Read
             , Show
             )



-- | Buffer containing a single element.
singleton :: a -> Buffer a
singleton x = Buffer
    { prefix  = []
    , prefLen = 0
    , curr    = x
    , suffix  = []
    , sufLen  = 0
    }


-- | Create a buffer from a list.  Starts out pointed at the start.
fromList :: [a] -> Maybe (Buffer a)
fromList []     = Nothing
fromList (l:ls) = Just $ (singleton l)
                    { suffix = ls
                    , sufLen = L.length ls
                    }


-- | Convert a buffer to a list.
toList :: Buffer a -> [a]
toList b = reverse (prefix b) ++ [curr b] ++ suffix b


-- | Retrieve the amount of elements stored.
length :: Buffer a -> Int
length b = prefLen b + sufLen b + 1


-- | Retrieve a sublist from the buffer. Indices are absolute and zero-based.
sub :: Int -> Int -> Buffer a -> [a]
sub from to b = ( L.reverse
                $ L.drop (prefLen b - to - 1)
                $ L.take (prefLen b - from)
                $ prefix b
                )
             ++ (if from <= currPos b && currPos b <= to
                    then [curr b]
                    else []
                )
             ++ ( L.drop (from - prefLen b - 1)
                $ L.take (to - prefLen b)
                $ suffix b
                )


-- | Retrieve the element left of the current position.
left :: Buffer a -> Maybe a
left b | L.null $ prefix b = Nothing
       | otherwise         = Just $ head $ prefix b


-- | Retrieve the element right of the current position.
right :: Buffer a -> Maybe a
right b | L.null $ suffix b = Nothing
        | otherwise         = Just $ head $ suffix b


-- | Retrieve the element at an absolute, zero-based index, if it exists.
atMay :: Buffer a -> Int -> Maybe a
atMay b n | n <  prefLen b = S.atMay (prefix b) $ prefLen b - n - 1
          | n == prefLen b = Just $ curr b
          | otherwise      = S.atMay (suffix b) $ n - prefLen b - 1


-- | Retrieve the element at an absolute, zero-based index, or a default in
--   case the element at the index is not defined.
atDef :: a -> Buffer a -> Int -> a
atDef d b n = fromMaybe d $ atMay b n


-- | Retrieve the first element in the buffer.
first :: Buffer a -> a
first b | prefLen b == 0 = curr b
        | otherwise      = L.last $ prefix b


-- | Retrieve the last element in the buffer.
last :: Buffer a -> a
last b | sufLen b == 0 = curr b
       | otherwise     = L.last $ suffix b





-- | Rapid, but weak equality test. Compares length, position and the elements
--   up to @n@ distance from the position.
resembles :: (Eq a) => Int -> Buffer a -> Buffer a -> Bool
resembles n a b =         prefLen a  ==         prefLen b
               &&         sufLen  a  ==         sufLen  b
               &&         curr    a  ==         curr    b
               && take n (prefix  a) == take n (prefix  b)
               && take n (suffix  a) == take n (suffix  b)





-- | Retrieve the current position in the buffer, defined as the amount of
--   elements before the current position.
currPos :: Buffer a -> Int
currPos = prefLen


-- | Move the buffer position relatively. Will silently stop at the front or
--   back of the buffer.
move :: Int -> Buffer a -> Buffer a
move n b | n > 0     = move (n-1) $ fromMaybe b $ forward  b
         | n < 0     = move (n+1) $ fromMaybe b $ backward b
         | otherwise = b


-- | Move the position absolutely. Will silently stop at the front or back of
--   the buffer.
moveTo :: Int -> Buffer a -> Buffer a
moveTo n b = move (n - prefLen b) b


-- | Advance the buffer position by 1.
forward :: Buffer a -> Maybe (Buffer a)
forward b | sufLen b <= 0 = Nothing
          | otherwise     = Just Buffer
                                { prefix  = curr b : prefix b
                                , prefLen = prefLen b + 1
                                , curr    = head $ suffix b
                                , suffix  = tail $ suffix b
                                , sufLen  = sufLen b - 1
                                }


-- | Rewind the buffer position by 1.
backward :: Buffer a -> Maybe (Buffer a)
backward b | prefLen b <= 0 = Nothing
           | otherwise      = Just Buffer
                                { prefix  = tail $ prefix b
                                , prefLen = prefLen b - 1
                                , curr    = head $ prefix b
                                , suffix  = curr b : suffix b
                                , sufLen  = sufLen b + 1
                                }


-- | Rewind the buffer position to the front.
toFirst :: Buffer a -> Buffer a
toFirst b | prefLen b <= 0 = b
          | otherwise      = toFirst
                           $ fromJust
                           $ backward b


-- | Advance the buffer position to the back.
toLast :: Buffer a -> Buffer a
toLast b | sufLen b <= 0 = b
         | otherwise     = toLast
                         $ fromJust
                         $ forward b





-- | Insert a new element, pushing the focused element to the left.
insertLeft :: a -> Buffer a -> Buffer a
insertLeft x b = b { prefix  = curr b : prefix b
                   , prefLen = prefLen b + 1
                   , curr    = x
                   }


-- | Insert a new element, pushing the focused element to the right.
insertRight :: a -> Buffer a -> Buffer a
insertRight x b = b { suffix = curr b : suffix b
                    , sufLen = sufLen b + 1
                    , curr   = x
                    }



-- | Drop at most n elements before the buffer position.
dropLeft :: Int -> Buffer a -> Buffer a
dropLeft n b = b { prefix  = drop n $ prefix b
                 , prefLen = max  0 $ prefLen b - max 0 n
                 }


-- | Drop n elements after the buffer position.
dropRight :: Int -> Buffer a -> Buffer a
dropRight n b = b { suffix = drop n $ suffix b
                  , sufLen = max  0 $ sufLen b - max 0 n
                  }

-- | Drop the currently focused element, filling the void from the left.
deleteLeft :: Buffer a -> Maybe (Buffer a)
deleteLeft b | prefLen b <= 0 = Nothing
             | otherwise      = b { curr    = head $ prefix b
                                  , prefix  = tail $ prefix b
                                  , prefLen = prefLen b - 1
                                  }

-- | Drop the currently focused element, filling the void from the right.
deleteRight :: Buffer a -> Maybe (Buffer a)
deleteRight b | sufLen b <= 0 = Nothing
              | otherwise     = b { curr    = head $ suffix b
                                  , prefix  = tail $ suffix b
                                  , prefLen = sufLen b - 1
                                  }


-- | Apply a function to the element left of the buffer position.
withLeft :: (a -> a) -> Buffer a -> Maybe (Buffer a)
withLeft f b | L.null $ prefix b = Nothing
             | otherwise         = Just $ b { prefix = f (head $ prefix b)
                                                     : drop 1 (prefix b)
                                            }


-- | Apply a function to the element left of the position. Will use a default
--   element as function argument if there is no left element, effectively
--   increasing the buffer size by 1.
withLeftDef :: a -> (a -> a) -> Buffer a -> Buffer a
withLeftDef d f b | L.null $ prefix b = b { prefix  = [f d]
                                          , prefLen = 1
                                          }

                  | otherwise         = b { prefix = f (head $ prefix b)
                                                   : drop 1 (prefix b)
                                          }


-- | Apply a function to a part of the buffer, starting at most n elements in
--   front of the position, and ending with the last element before the
--   position.
withNLeft :: Int -> (a -> a) -> Buffer a -> Buffer a
withNLeft n f b =
    let
        l = (P.map f $ take n $ prefix b)
         ++ (          drop n $ prefix b)
    in
        b { prefix  = l
          , prefLen = P.length l
          }


-- | Apply a function to the focused element.
withPos :: (a -> a) -> Buffer a -> Buffer a
withPos f b = b { curr = f $ curr b }


-- | Apply a function to the element right of the buffer position.
withRight :: (a -> a) -> Buffer a -> Maybe (Buffer a)
withRight f b | L.null $ suffix b = Nothing
              | otherwise         = Just $ b { suffix = f (head $ suffix b)
                                                      : drop 1 (suffix b)
                                             }


-- | Apply a function to the element right of the position. Will use
--   a default element as function argument if there is no right element,
--   effectively increasing the buffer size by 1.
withRightDef :: a -> (a -> a) -> Buffer a -> Buffer a
withRightDef d f b | L.null $ suffix b = b { suffix = [f d]
                                           , sufLen = 1
                                           }

                   | otherwise         = b { suffix = f (head $ suffix b)
                                                    : drop 1 (suffix b)
                                           }


-- | Apply a function to a part of the buffer, starting directly after the
--   position and ending n elements afterwards.
withNRight :: Int -> (a -> a) -> Buffer a -> Buffer a
withNRight n f b =
    let
        l = (P.map f $ take n $ suffix b)
         ++ (          drop n $ suffix b)
    in
        b { suffix = l
          , sufLen = P.length l
          }



-- | Append an element at the end of the buffer.
append :: a -> Buffer a -> Buffer a
append x b = b { suffix = suffix b ++ [x]
               , sufLen = sufLen b + 1
               }


-- | Prepend an element at the front of the buffer.
prepend :: a -> Buffer a -> Buffer a
prepend x b = b { prefix  = prefix b ++ [x]
                , prefLen = prefLen b + 1
                }



-- | Map a function over all elements in the buffer.
map :: (a -> b) -> Buffer a -> Buffer b
map f b = b { prefix = L.map f $ prefix b
            , curr   =       f $ curr   b
            , suffix = L.map f $ suffix b
            }
