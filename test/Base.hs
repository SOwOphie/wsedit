module Base where

import Test.Hspec



todo :: String -> SpecWith ()
todo s = it s $ pendingWith "Test still to be implemented"
