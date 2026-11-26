-- | Minimal assertions for exitcode-style test suites.
module Workshop.Test (assert, equal) where

import Control.Monad (unless)
import System.Exit (die)

-- | Stop the suite with a message unless the condition holds.
assert :: Bool -> String -> IO ()
assert condition message = unless condition (die message)

-- | Stop the suite when two values differ, showing both.
equal :: (Eq a, Show a) => String -> a -> a -> IO ()
equal label expected actual =
    assert (expected == actual) (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)
