import Prelude

import System.Environment (setEnv)

import Test.Tasty
import Test.Tasty.Ingredients.ConsoleReporter (UseColor(..))

import Tests.SPICommands (tests)
import Tests.Types (tests)

main :: IO ()
main = do
  setEnv "TASTY_COLOR" "always"
  defaultMain $ localOption @UseColor Always $ testGroup "."
    [ Tests.Types.tests
    , Tests.SPICommands.tests
    ]
