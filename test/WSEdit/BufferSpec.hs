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
                property $ \l ->
                    B.fromList (B.toList l) == Just (B.toFirst l :: B.Buffer Int)

        describe "Creation" $ do
            describe "singleton" $ it "creates a singleton element" $
                property $ \x ->
                    B.length (B.singleton (x :: Int)) == 1

        describe "Accessors" $ do
            todo "pos"
            todo "currPos"
            todo "prefLength"
            todo "sufLength"
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
                property $ \l -> MkProperty $ do
                    a <- choose (0, B.length l)
                    b <- choose (0, B.length l)
                    unProperty $ and [0 <= a, a <= b, b <= B.length l] ==>
                        B.sub a b l == drop a (take (b + 1) $ B.toList (l :: B.Buffer Int))

            todo "diffZone"
            todo "resembles"
