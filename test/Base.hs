module Base where

import Test.Hspec



todo :: String -> SpecWith ()
todo s = it s $ pendingWith "Test still to be implemented"

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight = not . isLeft
