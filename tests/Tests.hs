import System.Exit

import Control.Monad
import Tests.Base
import Tests.WSEdit



-- | Collection of all tests.
allTests :: Test
allTests = TestList
    [ testWSEdit
    ]



-- | Main function
main :: IO ()
main = do
    r <- runTestTT allTests
    when (errors r + failures r > 0) $ exitFailure
