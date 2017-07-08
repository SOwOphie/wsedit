module WSEdit.BufferSpec (spec) where


import Data.Hashable
import Data.Maybe
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Property

import qualified WSEdit.Buffer as B

import Base



instance (Arbitrary a, Hashable a) => Arbitrary (B.Buffer a) where
    arbitrary = do
        x   <- arbitrary
        xs  <- arbitrary
        pos <- choose (0, length (x:xs))
        return $ B.moveTo pos $ fromJust $ B.fromList (x:xs)



spec :: Spec
spec = do
    describe "Buffer" $ do
        describe "List conversion" $ do
            it "can be created from a list" $
                property $ \l ->
                    not (null l) ==>
                        fmap B.toList (B.fromList l) == Just (l :: [Int])

            it "creates a list with the same length" $
                property $ \l ->
                    not (null l) ==>
                        B.length (fromJust $ B.fromList l) == length (l :: [Int])

            it "can be converted into a list" $
                property $ \b ->
                    B.fromList (B.toList b) == Just (B.toFirst b :: B.Buffer Int)

        describe "Creation" $ do
            describe "singleton" $ it "creates a singleton element" $
                property $ \x ->
                    B.length (B.singleton (x :: Int)) == 1

        describe "Accessors" $ do
            describe "pos" $ it "returns the currently focused element" $
                property $ \l ->
                    not (null l) ==> withListPos (l :: [Int]) $
                        \pos -> property $
                            B.pos (B.moveTo pos $ fromJust $ B.fromList l)
                               == (l !! pos)

            describe "currPos" $ it "returns the number of elements before the current position" $
                property $ \b ->
                    withBufPos (b :: B.Buffer Int) $ \pos -> property $
                        B.currPos (B.moveTo pos b) == pos

            describe "prefLength" $ it "is an alias for currPos" $
                property $ \b ->
                    B.currPos b == B.prefLength (b :: B.Buffer Int)

            describe "sufLength" $ it "returns the number of items after the current position" $
                property $ \b -> property $
                    B.length b - B.prefLength b - 1
                        == B.sufLength (b :: B.Buffer Int)

            todo "left"
            todo "right"
            todo "atMay"
            todo "atDef"
            todo "first"
            todo "last"

        describe "Movement" $ do
            todo "move"
            todo "moveTo"
            todo "forward"
            todo "backward"
            todo "toFirst"
            todo "toLast"

        describe "Modifiers" $ do
            describe "Insert" $ do
                todo "insertLeft"
                todo "insertRight"
                todo "insertBefore"
                todo "insertAfter"

            describe "Remove" $ do
                todo "deleteLeft"
                todo "deleteRight"
                todo "dropLeft"
                todo "dropRight"
                todo "dropPrefix"
                todo "dropSuffix"

            describe "Maps" $ do
                todo "withLeft"
                todo "withLeftDef"
                todo "withNLeft"
                todo" withCurr"
                todo "withRight"
                todo "withRightDef"
                todo "withNRight"
                todo "map"
                todo "mapM"

            describe "Concatenation" $ do
                todo "append"
                todo "prepend"

        describe "Misc" $ do
            describe "sub" $ it "extracts a sublist" $
                property     $ \b ->
                withBufPos b $ \x ->
                withBufPos b $ \y ->
                    x <= y ==>
                        drop x (take (y + 1) $ B.toList (b :: B.Buffer Int))
                            == B.sub x y b

            todo "diffZone"
            todo "resembles"





withListPos :: [a] -> (Int -> Property) -> Property
withListPos l f = MkProperty $ do
    pos <- choose (0, length l - 1)
    unProperty
        $ counterexample ("Index " ++ show pos)
        $ f pos



withBufPos :: B.Buffer a -> (Int -> Property) -> Property
withBufPos b f = MkProperty $ do
    pos <- choose (0, B.length b - 1)
    unProperty
        $ counterexample ("Index " ++ show pos)
        $ f pos
