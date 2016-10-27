{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# LANGUAGE StandaloneDeriving #-}

module WSEdit.Buffer
    ( Buffer
    , pos
    , setPos
    , prefLength
    , sufLength
    , diffZone
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
    , insertBefore
    , insertAfter
    , deleteLeft
    , deleteRight
    , dropLeft
    , dropRight
    , dropPrefix
    , dropSuffix
    , withLeft
    , withLeftDef
    , withNLeft
    , withCurr
    , withRight
    , withRightDef
    , withNRight
    , append
    , prepend
    , map
    , mapM
    , mapM_
    ) where


import Data.Hashable (Hashable, hash, hashWithSalt)
import Data.Maybe    (fromMaybe)
import Safe          (fromJustNote, headMay, headNote, lastMay, tailNote)

import Prelude hiding (last, length, map, mapM, mapM_)

import qualified Data.List as L
import qualified Prelude   as P
import qualified Safe      as S



fqn :: String -> String
fqn = ("WSEdit.Buffer." ++)





-- | Finite list of elements, alongside their hash, salted with the hash of the
--   previous element. Allows for fast difference checking.
type HashList a = [(Int, a)]



-- | Create a new hash list from a regular one.
newHashList :: (Hashable a) => [a] -> HashList a
newHashList = flip concatHashList []



-- | Concat a normal and a hash list.
concatHashList :: (Hashable a) => [a] -> HashList a -> HashList a
concatHashList l1 l2 = cnc (L.reverse l1) l2
    where
        cnc :: (Hashable a) => [a] -> HashList a -> HashList a
        cnc []     b           = b
        cnc (x:xs) []          = cnc xs [(                 hash x, x)]
        cnc (x:xs) b@((h,_):_) = cnc xs ((hashWithSalt h $ hash x, x) : b)



-- | Appends a single element in front of a hash list.
consHashList :: (Hashable a) => a -> HashList a -> HashList a
consHashList el h = concatHashList [el] h






-- | One-dimensional nonempty buffer with a current position.  Most operations
--   scale only with distance from the position, not with total buffer size.
--   Next to every element, a hash over itself as well as all elements farther
--   away from the buffer position is stored to allow for fast difference
--   checks.
data Buffer a = Buffer
    { prefix  :: HashList a -- ^ Elements in front of the position, in reversed order
    , prefLen :: Int        -- ^ Amount of elements in front of the current position
    , curr    :: a          -- ^ Currently focused element
    , suffix  :: HashList a -- ^ Elements after the position
    , sufLen  :: Int        -- ^ Amount of elements after the position
    }
    deriving ( Eq
             , Read
             , Show
             )



-- | Returns the currently focused element.
pos :: Buffer a -> a
pos = curr



-- | Set the element at the current position.
setPos :: a -> Buffer a -> Buffer a
setPos el b = b { curr = el }



-- | Amount of elements in front of the current position.
prefLength :: Buffer a -> Int
prefLength = prefLen



-- | Amount of elements after the position.
sufLength :: Buffer a -> Int
sufLength = sufLen



-- | Calculate the difference between two 'Buffer's. Cutting the returned number
--   of elements from the prefix / suffix of the first passed buffer yields
--   their common elements.
diffZone :: Buffer a -> Buffer a -> (Int, Int)
diffZone a b =
    let
        pLen = min (prefLen a) $ prefLen b
        sLen = min ( sufLen a) $  sufLen b

        ovPA = L.length (prefix a) - pLen
        ovPB = L.length (prefix b) - pLen
        ovSA = L.length (suffix a) - sLen
        ovSB = L.length (suffix b) - sLen

        ps = zip (drop ovPA $ prefix a)
                 (drop ovPB $ prefix b)

        ss = zip (drop ovSA $ suffix a)
                 (drop ovSB $ suffix b)
    in
        (ovPA + L.length (takeWhile df ps), ovSA + L.length (takeWhile df ss))
    where
        df :: ((Int, a), (Int, a)) -> Bool
        df ((x,_),(y,_)) = x /= y



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
fromList :: (Hashable a) => [a] -> Maybe (Buffer a)
fromList []     = Nothing
fromList (l:ls) = Just $ (singleton l)
                    { suffix = newHashList ls
                    , sufLen = L.length ls
                    }


-- | Convert a buffer to a list.
toList :: Buffer a -> [a]
toList b = reverse (P.map snd $ prefix b) ++ [curr b] ++ P.map snd (suffix b)


-- | Retrieve the amount of elements stored.
length :: Buffer a -> Int
length b = prefLen b + sufLen b + 1


-- | Retrieve a sublist from the buffer. Indices are absolute and zero-based.
sub :: Int -> Int -> Buffer a -> [a]
sub from to b = P.map snd
                  ( L.reverse
                  $ L.drop (prefLen b - to - 1)
                  $ L.take (prefLen b - from)
                  $ prefix b
                  )
             ++ (if from <= currPos b && currPos b <= to
                    then [curr b]
                    else []
                )
             ++ P.map snd
                  ( L.drop (from - prefLen b - 1)
                  $ L.take (to - prefLen b)
                  $ suffix b
                  )


-- | Retrieve the element left of the current position.
left :: Buffer a -> Maybe a
left b = fmap snd $ headMay $ prefix b


-- | Retrieve the element right of the current position.
right :: Buffer a -> Maybe a
right b = fmap snd $ headMay $ suffix b


-- | Retrieve the element at an absolute, zero-based index, if it exists.
atMay :: Buffer a -> Int -> Maybe a
atMay b n | n <  prefLen b = fmap snd $ S.atMay (prefix b) $ prefLen b - n - 1
          | n == prefLen b = Just $ curr b
          | otherwise      = fmap snd $ S.atMay (suffix b) $ n - prefLen b - 1


-- | Retrieve the element at an absolute, zero-based index, or a default in
--   case the element at the index is not defined.
atDef :: a -> Buffer a -> Int -> a
atDef d b n = fromMaybe d $ atMay b n


-- | Retrieve the first element in the buffer.
first :: Buffer a -> a
first b = fromMaybe (curr b)
        $ fmap snd
        $ lastMay
        $ prefix b


-- | Retrieve the last element in the buffer.
last :: Buffer a -> a
last b = fromMaybe (curr b)
       $ fmap snd
       $ lastMay
       $ suffix b





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
move :: (Hashable a) => Int -> Buffer a -> Buffer a
move n b | n > 0     = move (n-1) $ fromMaybe b $ forward  b
         | n < 0     = move (n+1) $ fromMaybe b $ backward b
         | otherwise = b


-- | Move the position absolutely. Will silently stop at the front or back of
--   the buffer.
moveTo :: (Hashable a) => Int -> Buffer a -> Buffer a
moveTo n b = move (n - prefLen b) b


-- | Advance the buffer position by 1.
forward :: (Hashable a) => Buffer a -> Maybe (Buffer a)
forward b | sufLen b <= 0 = Nothing
          | otherwise     = Just Buffer
                                { prefix  = curr b `consHashList` prefix b
                                , prefLen = prefLen b + 1
                                , curr    = snd $ headNote (fqn "forward") $ suffix b
                                , suffix  =       tailNote (fqn "forward") $ suffix b
                                , sufLen  = sufLen b - 1
                                }


-- | Rewind the buffer position by 1.
backward :: (Hashable a) => Buffer a -> Maybe (Buffer a)
backward b | prefLen b <= 0 = Nothing
           | otherwise      = Just Buffer
                                { prefix  =       tailNote (fqn "backward") $ prefix b
                                , prefLen = prefLen b - 1
                                , curr    = snd $ headNote (fqn "backward") $ prefix b

                                , suffix  = curr b `consHashList` suffix b
                                , sufLen  = sufLen b + 1
                                }


-- | Rewind the buffer position to the front.
toFirst :: (Hashable a) => Buffer a -> Buffer a
toFirst b | prefLen b <= 0 = b
          | otherwise      = toFirst
                           $ fromJustNote (fqn "toFirst")
                           $ backward b


-- | Advance the buffer position to the back.
toLast :: (Hashable a) => Buffer a -> Buffer a
toLast b | sufLen b <= 0 = b
         | otherwise     = toLast
                         $ fromJustNote (fqn "toLast")
                         $ forward b





-- | Insert a new element, pushing the focused element to the left.
insertLeft :: (Hashable a) => a -> Buffer a -> Buffer a
insertLeft x b = b { prefix  = curr b `consHashList` prefix b
                   , prefLen = prefLen b + 1
                   , curr    = x
                   }


-- | Insert a new element, pushing the focused element to the right.
insertRight :: (Hashable a) => a -> Buffer a -> Buffer a
insertRight x b = b { suffix = curr b `consHashList` suffix b
                    , sufLen = sufLen b + 1
                    , curr   = x
                    }



-- | Insert a new element in front of the current position.
insertBefore :: (Hashable a) => a -> Buffer a -> Buffer a
insertBefore x b = b { prefix  = x `consHashList` prefix b
                     , prefLen = prefLen b + 1
                     }

-- | Insert a new element after the current position.
insertAfter :: (Hashable a) => a -> Buffer a -> Buffer a
insertAfter x b = b { suffix = x `consHashList` suffix b
                    , sufLen = sufLen b + 1
                    }



-- | Drop the currently focused element, filling the void from the left.
deleteLeft :: Buffer a -> Maybe (Buffer a)
deleteLeft b | prefLen b <= 0 = Nothing
             | otherwise      = Just b
                              { curr    = snd
                                        $ headNote (fqn "deleteLeft")
                                        $ prefix b

                              , prefix  = tailNote (fqn "deleteLeft")
                                        $ prefix b

                              , prefLen = prefLen b - 1
                              }

-- | Drop the currently focused element, filling the void from the right.
deleteRight :: Buffer a -> Maybe (Buffer a)
deleteRight b | sufLen b <= 0 = Nothing
              | otherwise     = Just b
                              { curr   = snd
                                       $ headNote (fqn "deleteRight")
                                       $ suffix b

                              , suffix = tailNote (fqn "deleteRight")
                                       $ suffix b

                              , sufLen = sufLen b - 1
                              }


-- | Apply `deleteLeft` at most n times.
dropLeft :: Int -> Buffer a -> Buffer a
dropLeft n b | n <= 0    = b
             | otherwise = dropLeft (n-1)
                         $ fromMaybe b
                         $ deleteLeft b


-- | Apply `deleteRight` at most n times.
dropRight :: Int -> Buffer a -> Buffer a
dropRight n b | n <= 0 = b
              | otherwise = dropRight (n-1)
                          $ fromMaybe b
                          $ deleteRight b

-- | Drop n elements off the (reversed) prefix.
dropPrefix :: Int -> Buffer a -> Buffer a
dropPrefix n b = b { prefix  = drop n $ prefix  b
                   , prefLen = max 0  $ prefLen b - n
                   }

-- | Drop n elements off the suffix.
dropSuffix :: Int -> Buffer a -> Buffer a
dropSuffix n b = b { suffix = drop n $ suffix b
                   , sufLen = max 0  $ sufLen b - n
                   }



-- | Apply a function to the element left of the buffer position.
withLeft :: (Hashable a) => (a -> a) -> Buffer a -> Maybe (Buffer a)
withLeft f b | L.null $ prefix b = Nothing
             | otherwise         = Just
                                 $ b { prefix = f ( snd
                                                  $ headNote (fqn "withLeft")
                                                  $ prefix b
                                                  )
                                              `consHashList` drop 1 (prefix b)
                                     }


-- | Apply a function to the element left of the position. Will use a default
--   element as function argument if there is no left element, effectively
--   increasing the buffer size by 1.
withLeftDef :: (Hashable a) => a -> (a -> a) -> Buffer a -> Buffer a
withLeftDef d f b = fromMaybe (b { prefix = newHashList [f d], prefLen = 1 })
                  $ withLeft f b


-- | Apply a function to a part of the buffer, starting at most n elements in
--   front of the position, and ending with the last element before the
--   position.
withNLeft :: (Hashable a) => Int -> (a -> a) -> Buffer a -> Buffer a
withNLeft n f b = b { prefix = P.map (f . snd) (take n $ prefix b)
                             `concatHashList`   drop n ( prefix b)
                    }


-- | Apply a function to the focused element.
withCurr :: (a -> a) -> Buffer a -> Buffer a
withCurr f b = b { curr = f $ curr b }


-- | Apply a function to the element right of the buffer position.
withRight :: (Hashable a) => (a -> a) -> Buffer a -> Maybe (Buffer a)
withRight f b | L.null $ suffix b = Nothing
              | otherwise         = Just
                                  $ b { suffix = f ( snd
                                                   $ headNote (fqn "withRight")
                                                   $ suffix b
                                                   )
                                               `consHashList` drop 1 (suffix b)
                                      }


-- | Apply a function to the element right of the position. Will use
--   a default element as function argument if there is no right element,
--   effectively increasing the buffer size by 1.
withRightDef :: (Hashable a) => a -> (a -> a) -> Buffer a -> Buffer a
withRightDef d f b = fromMaybe (b { suffix = newHashList [(f d)], sufLen = 1 })
                   $ withRight f b


-- | Apply a function to a part of the buffer, starting directly after the
--   position and ending n elements afterwards.
withNRight :: (Hashable a) => Int -> (a -> a) -> Buffer a -> Buffer a
withNRight n f b = b { suffix = P.map (f . snd) (take n $ suffix b)
                              `concatHashList`   drop n ( suffix b)
                     }



-- | Append an element at the end of the buffer.
append :: (Hashable a) => a -> Buffer a -> Buffer a
append x b = b { suffix = newHashList $ P.map snd (suffix b) ++ [x]
               , sufLen = sufLen b + 1
               }


-- | Prepend an element at the front of the buffer.
prepend :: (Hashable a) => a -> Buffer a -> Buffer a
prepend x b = b { prefix  = newHashList $ P.map snd (prefix b) ++ [x]
                , prefLen = prefLen b + 1
                }



-- | Map a function over all elements in the buffer.
map :: (Hashable b) => (a -> b) -> Buffer a -> Buffer b
map f b = b { prefix = newHashList $ L.map (f . snd) $ prefix b
            , curr   =                      f        $ curr   b
            , suffix = newHashList $ L.map (f . snd) $ suffix b
            }



mapM :: (Monad m, Hashable b) => (a -> m b) -> Buffer a -> m (Buffer b)
mapM f b = do
    p <- fmap (newHashList . reverse)
       $ P.mapM (f . snd)
       $ reverse
       $ prefix b

    c <- f $ curr b

    s <- fmap (newHashList)
       $ P.mapM (f . snd)
       $ suffix b

    return b { prefix = p
             , curr   = c
             , suffix = s
             }



mapM_ :: (Monad m, Hashable b) => (a -> m b) -> Buffer a -> m ()
mapM_ f b = mapM f b >> return ()

