import System.Exit

import Control.Monad
import Tests.Base

import Tests.WSEdit
import Tests.WSEdit.Buffer



-- | Collection of all tests.
allTests :: Test
allTests = TestList
    [ testWSEdit
    , testWSEditBuffer
    ]



-- | Main function
main :: IO ()
main = do
    r <- runTestTT allTests
    when (errors r + failures r > 0) $ exitFailure
