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



type B = B.Buffer Int
type L = [Int]



spec :: Spec
spec = do
    describe "Buffer" $ do
        describe "List conversion" $ do
            it "can be created from a list" $
                property $ \l ->
                    not (null l) ==>
                        fmap B.toList (B.fromList l) == Just (l :: L)

            it "creates a list with the same length" $
                property $ \l ->
                    not (null l) ==>
                        B.length (fromJust $ B.fromList l) == length (l :: L)

            it "can be converted into a list" $
                property $ \b ->
                    B.fromList (B.toList b) == Just (B.toFirst b :: B)

        describe "Creation" $ do
            describe "singleton" $ it "creates a singleton element" $
                property $ \x ->
                    B.length (B.singleton (x :: Int)) == 1

        describe "Accessors" $ do
            describe "pos" $ it "returns the currently focused element" $
                property $ \l ->
                    not (null l) ==> withListPos (l :: L) $
                        \pos -> property $
                            B.pos (B.moveTo pos $ fromJust $ B.fromList l)
                               == (l !! pos)

            describe "currPos" $ it "returns the number of elements before the current position" $
                property $ \b ->
                    withBufPos (b :: B) $ \pos -> property $
                        B.currPos (B.moveTo pos b) == pos

            describe "prefLength" $ it "is an alias for currPos" $
                property $ \b ->
                    B.currPos b == B.prefLength (b :: B)

            describe "sufLength" $ it "returns the number of items after the current position" $
                property $ \b -> property $
                    B.length b - B.prefLength b - 1
                        == B.sufLength (b :: B)

            describe "left" $ do
                it "returns the element left of the current position if it exists" $
                    property $ \b ->
                        B.currPos b > 0 ==>
                            B.left (b :: B) == Just (B.toList b !! (B.currPos b - 1))

                it "returns Nothing if there is no left element" $
                    property $ \b ->
                        B.left (B.toFirst b :: B) == Nothing

            describe "right" $ do
                it "returns the element right of the current position if it exists" $
                    property $ \b ->
                        B.currPos b < (B.length b - 1) ==>
                            B.right (b :: B) == Just (B.toList b !! (B.currPos b + 1))

                it "returns Nothing if there is no right element" $
                    property $ \b ->
                        B.right (B.toLast b :: B) == Nothing

            describe "atMay" $ do
                it "returns the element at the given index if it exists" $
                    property $ \b ->
                        withBufPos (b :: B) $ \pos -> property $
                            B.atMay b pos == Just (B.toList b !! pos)

                it "returns Nothing if the index is invalid" $
                    property $ \b ->
                        withInvalidBufPos (b :: B) $ \pos -> property $
                            B.atMay b pos == Nothing

            describe "atDef" $ do
                it "returns the element at the given index if it exists" $
                    property $ \b d ->
                        withBufPos (b :: B) $ \pos -> property $
                            B.atDef d b pos == (B.toList b !! pos)

                it "returns a default if the index is invalid" $
                    property $ \b d ->
                        withInvalidBufPos (b :: B) $ \pos -> property $
                            B.atDef d b pos == d

            describe "first" $ do
                it "returns the first value in the buffer" $
                    property $ \b ->
                        B.first (b :: B) == head (B.toList b)

            describe "last" $ do
                it "returns the last value in the buffer" $
                    property $ \b ->
                        B.last (b :: B) == last (B.toList b)


        describe "Movement" $ do
            describe "move" $ do
                it "moves the buffer position relatively" $
                    property $ \b ->
                        withBufPos (b :: B) $ \p -> property $
                            B.currPos (B.move (p - B.currPos b) b) == p

                it "silently stops at the end of the buffer" $
                    property $ \b ->
                        withInvalidBufPos (b :: B) $ \p -> property $
                            B.currPos (B.move (p - B.currPos b) b)
                                == (if (p > 0) then B.length b - 1 else 0)

                it "does not alter the buffer contents in any way" $
                    property $ \b ->
                        withMaybeValidBufPos (b :: B) $ \p -> property $
                            B.toList (B.move (p - B.currPos b) b) == B.toList b

            describe "moveTo" $ do
                it "moves the buffer position absolutely" $
                    property $ \b ->
                        withBufPos (b :: B) $ \p -> property $
                            B.currPos (B.moveTo p b) == p

                it "silently stops at the end of the buffer" $
                    property $ \b ->
                        withInvalidBufPos (b :: B) $ \p -> property $
                            B.currPos (B.moveTo p b)
                                == (if (p > 0) then B.length b - 1 else 0)

                it "does not alter the buffer contents in any way" $
                    property $ \b ->
                        withMaybeValidBufPos (b :: B) $ \p -> property $
                            B.toList (B.moveTo p b) == B.toList b

                it "directly corresponds to a move command" $
                    property $ \b p -> B.move   (p - B.currPos b) (b :: B)
                                    == B.moveTo  p                 b

            describe "forward" $ do
                it "advances to the next buffer position if it exists" $
                    property $ \b ->
                        B.sufLength (b :: B) > 0 ==> property $
                            B.forward b == Just (B.move 1 b)

                it "returns Nothing if the buffer is at the last position" $
                    property $ \b ->
                        B.forward (B.moveTo (B.length b - 1) (b :: B))
                            == Nothing

            describe "backward" $ do
                it "rewinds to the previous buffer position if it exists" $
                    property $ \b ->
                        B.prefLength (b :: B) > 0 ==> property $
                            B.backward b == Just (B.move (-1) b)

                it "returns Nothing if the buffer is at the first position" $
                    property $ \b ->
                        B.backward (B.moveTo 0 (b :: B))
                            == Nothing

            describe "toFirst" $ do
                it "rewinds the buffer to the first position" $
                    property $ \b ->
                        B.toFirst b == B.moveTo 0 (b :: B)

            describe "toLast" $ do
                it "advances the buffer to the last position" $
                    property $ \b ->
                        B.toLast b == B.moveTo (B.length b - 1) (b :: B)

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



withInvalidBufPos :: B.Buffer a -> (Int -> Property) -> Property
withInvalidBufPos b f = MkProperty $ do
    pos <- oneof [ choose (-1 * B.length b, -1            )
                 , choose (     B.length b, B.length b * 2)
                 ]
    unProperty
        $ counterexample ("Index " ++ show pos)
        $ f pos



withMaybeValidBufPos :: B.Buffer a -> (Int -> Property) -> Property
withMaybeValidBufPos b f = MkProperty $ oneof $ map unProperty
    [ withBufPos        b f
    , withInvalidBufPos b f
    ]
