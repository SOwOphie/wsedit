module WSEdit.Arguments.ParserSpec (spec) where


import Test.Hspec
import Test.QuickCheck
import Text.Parsec

import WSEdit.Data
import WSEdit.Arguments.Data
import WSEdit.Arguments.Parser



testBase :: CanonicalPath
testBase = CanonicalPath "/usr"

testParser :: WSParser a -> String -> Either ParseError a
testParser p = runP p testBase "Test string"


spec :: SpecWith ()
spec = do
    describe "qualifier" $ do
        it "recognizes a file name" $
            property $
                testParser qualifier "test.file"
                    `shouldBe` Right (FileQualifier "test.file")

        it "recognizes a relative path" $
            property $
                testParser qualifier "local/test.file"
                    `shouldBe` Right (PathQualifier testBase "local/test.file")

        it "recognizes an absolute path" $
            property $
                testParser qualifier "/usr/local/test.file"
                    `shouldBe` Right (PathQualifier testBase "/usr/local/test.file")
