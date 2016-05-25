module WSEdit.Buffer
    ( Buffer (..)
    , empty
    , singleton
    , fromList
    , toList
    , length
    , sub
    , left
    , right
    , atDef
    , firstDef
    , lastDef
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
    , withRight
    , withRightDef
    , withNRight
    , append
    , prepend
    , map
    ) where


import Data.Maybe (fromJust)

import Prelude hiding (length, map)

import qualified Data.List as L
import qualified Prelude   as P
import qualified Safe      as S





-- | One-dimensional buffer with a current position between two elements.
--   Most operations' cost scales only with distance from the position, not
--   with total buffer size.
data Buffer a = Buffer
    { prefix  :: [a]
    , prefLen :: Int
    , suffix  :: [a]
    , sufLen  :: Int
    }
    deriving ( Eq
             , Read
             , Show
             )





-- | An empty buffer.
empty :: Buffer a
empty = Buffer
    { prefix  = []
    , prefLen = 0
    , suffix  = []
    , sufLen  = 0
    }


-- | Buffer containing a single element. Current position is at the front.
singleton :: a -> Buffer a
singleton x = Buffer
    { prefix  = []
    , prefLen = 0
    , suffix  = [x]
    , sufLen  = 1
    }


-- | Create a buffer from a list. Current position is at the front.
fromList :: [a] -> Buffer a
fromList l = empty
    { suffix = l
    , sufLen = L.length l
    }


-- | Convert a buffer to a list.
toList :: Buffer a -> [a]
toList b = reverse (prefix b) ++ suffix b


-- | Retrieve the amount of elements stored.
length :: Buffer a -> Int
length b = prefLen b + sufLen b


-- | Retrieve a sublist from the buffer. Indices are absolute and zero-based.
sub :: Int -> Int -> Buffer a -> [a]
sub from to b = ( L.reverse
                $ L.drop (prefLen b - to - 1)
                $ L.take (prefLen b - from)
                $ prefix b
                )
             ++ ( L.drop (from - prefLen b)
                $ L.take (to - prefLen b + 1)
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


-- | Retrieve the element at an absolute, zero-based index, or a default in
--   case the element at the index is not defined.
atDef :: a -> Buffer a -> Int -> a
atDef d b n | n < prefLen b = S.atDef d (prefix b) $ prefLen b - n - 1
            | otherwise     = S.atDef d (suffix b) $ n - prefLen b


-- | Retrieve the first element in the buffer, or a default if the buffer is
--   empty.
firstDef :: a -> Buffer a -> a
firstDef d b = atDef d b 0


-- | Retrieve the last element in the buffer, or a default if the buffer is
--   empty.
lastDef :: a -> Buffer a -> a
lastDef d b = atDef d b $ WSEdit.Buffer.length b - 1





-- | Weak equality test. Compares length, position and the elements up to @n@
--   distance from the buffer position.
resembles :: (Eq a) => Int -> Buffer a -> Buffer a -> Bool
resembles n a b =         prefLen a  ==         prefLen b
               &&         sufLen  a  ==         sufLen  b
               && take n (prefix  a) == take n (prefix  b)
               && take n (suffix  a) == take n (suffix  b)





-- | Retrieve the current position in the buffer, defined as the amount of
--   elements before the current position.
currPos :: Buffer a -> Int
currPos = prefLen


-- | Move the buffer position relatively. Will silently stop at the front or
--   back of the buffer.
move :: Int -> Buffer a -> Buffer a
move n b | n > 0 &&  sufLen b > 0 = move (n-1) $ fromJust $ forward  b
         | n < 0 && prefLen b > 0 = move (n+1) $ fromJust $ backward b
         | n == 0                 = b
         | otherwise              = b


-- | Move the position absolutely. Will silently stop at the front or back of
--   the buffer.
moveTo :: Int -> Buffer a -> Buffer a
moveTo n b = move (n - prefLen b) b


-- | Advance the buffer position by 1.
forward :: Buffer a -> Maybe (Buffer a)
forward b | sufLen b <= 0 = Nothing
          | otherwise     = Just Buffer
                                { prefix  = head (suffix b) : prefix b
                                , prefLen = prefLen b + 1
                                , suffix  = tail $ suffix b
                                , sufLen  = sufLen b - 1
                                }


-- | Rewind the buffer position by 1.
backward :: Buffer a -> Maybe (Buffer a)
backward b | prefLen b <= 0 = Nothing
           | otherwise      = Just Buffer
                                { prefix  = tail $ prefix b
                                , prefLen = prefLen b - 1
                                , suffix  = head (prefix b) : suffix b
                                , sufLen  = sufLen b + 1
                                }


-- | Rewind the buffer position to the front.
toFirst :: Buffer a -> Buffer a
toFirst b = Buffer
    { prefix  = []
    , prefLen = 0
    , suffix  = reverse (prefix b) ++ suffix b
    , sufLen  = prefLen b + sufLen b
    }


-- | Advance the buffer position to the back.
toLast :: Buffer a -> Buffer a
toLast b = Buffer
    { prefix  = reverse (suffix b) ++ prefix b
    , prefLen = prefLen b + sufLen b
    , suffix  = []
    , sufLen  = 0
    }





-- | Insert an element left of the buffer position.
insertLeft :: a -> Buffer a -> Buffer a
insertLeft x b = b { prefix  = x : prefix b
                   , prefLen = prefLen b + 1
                   }


-- | Insert an element right of the buffer position.
insertRight :: a -> Buffer a -> Buffer a
insertRight x b = b { suffix = x : suffix b
                    , sufLen = sufLen b + 1
                    }



-- | Drop n elements before the buffer position.
dropLeft :: Int -> Buffer a -> Buffer a
dropLeft n b = b { prefix  = drop n $ prefix b
                 , prefLen = max  0 $ prefLen b - max 0 n
                 }

-- | Drop the element left of the buffer position.
deleteLeft :: Buffer a -> Buffer a
deleteLeft = dropLeft 1


-- | Drop n elements after the buffer position.
dropRight :: Int -> Buffer a -> Buffer a
dropRight n b = b { suffix = drop n $ suffix b
                  , sufLen = max  0 $ sufLen b - max 0 n
                  }

-- | Drop the element right of the buffer position.
deleteRight :: Buffer a -> Buffer a
deleteRight = dropRight 1


-- | Apply a function to the element left of the buffer position.
withLeft :: (a -> a) -> Buffer a -> Maybe (Buffer a)
withLeft f b | L.null $ prefix b = Nothing
             | otherwise         = Just $ b { prefix = f (head $ prefix b)
                                                     : drop 1 (prefix b)
                                            }


-- | Apply a function to the element left of the buffer position. Will use
--   a default element as function argument if there is no left element,
--   effectively increasing the buffer size by 1.
withLeftDef :: a -> (a -> a) -> Buffer a -> Buffer a
withLeftDef d f b | L.null $ prefix b = b { prefix  = [f d]
                                          , prefLen = 1
                                          }

                  | otherwise         = b { prefix = f (head $ prefix b)
                                                   : drop 1 (prefix b)
                                          }


-- | Apply a function to a part of the buffer, starting at most n elements in
--   front of the buffer position, and ending there.
withNLeft :: Int -> (a -> a) -> Buffer a -> Buffer a
withNLeft n f b =
    let
        l = (P.map f $ take n $ prefix b)
         ++ (          drop n $ prefix b)
    in
        b { prefix  = l
          , prefLen = P.length l
          }



-- | Apply a function to the element right of the buffer position.
withRight :: (a -> a) -> Buffer a -> Maybe (Buffer a)
withRight f b | L.null $ suffix b = Nothing
              | otherwise         = Just $ b { suffix = f (head $ suffix b)
                                                      : drop 1 (suffix b)
                                             }


-- | Apply a function to the element right of the buffer position. Will use
--   a default element as function argument if there is no right element,
--   effectively increasing the buffer size by 1.
withRightDef :: a -> (a -> a) -> Buffer a -> Buffer a
withRightDef d f b | L.null $ suffix b = b { suffix = [f d]
                                           , sufLen = 1
                                           }

                   | otherwise         = b { suffix = f (head $ suffix b)
                                                    : drop 1 (suffix b)
                                           }


-- | Apply a function to a part of the buffer, starting at the buffer position
--   and ending n elements afterwards.
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
            , suffix = L.map f $ suffix b
            }
