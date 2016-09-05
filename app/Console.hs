{-# LANGUAGE OverloadedStrings #-}

module Console
  (
    markovConsole
  ) where

import System.Console.Haskeline
import Utilities
import Lib
import Control.Monad.State
import System.Random (newStdGen)
import qualified Data.Text as Text (pack, words, unpack)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import System.Exit

data Action = File FilePath |
              Order Int     |
              Gen Int Int   |
              Add String    |
              Read          |
              Help          |
              Quit
  deriving (Show, Read)

help :: String
help =  "File \"<path>\" -- give file path to read input text\n"     ++
        "Order <n>     -- set markov chain order\n"                  ++
        "Gen <x> <y>   -- generate x pieces of text, y words long\n" ++
        "Add <text>    -- add new text to the current model\n"       ++
        "Read          -- using the current settings, make a model\n"++
        "Quit          -- exit\n"                                    ++
        "Help          -- display this help\n"

data ConsoleState = ConsoleState
  {
    currentTree :: MarkovTree
  , order       :: Int
  , currentPath :: FilePath
  }

emptyState = ConsoleState empty 1 ""

type Console = StateT ConsoleState IO

readFromFile :: Console ()
readFromFile = do
  fp    <- currentPath   <$> get
  order <- order         <$> get
  tree  <- genFromNGrams <$> liftIO (getNGramsFromFile fp order)
  modify' (\s -> s {currentTree = tree})

addToTree :: String -> Console ()
addToTree str = do
  tree <- currentTree <$> get
  modify (\s -> s {currentTree = insert tree . Text.words $ Text.pack str})

runAction :: Action -> Console ()
runAction (File fp) = modify (\s -> s {currentPath = fp})
runAction (Order n) = modify (\s -> s {order = n})
runAction (Add l)   = addToTree l
runAction (Gen x y) = do
  markovTree <- currentTree <$> get
  liftIO . replicateM_ x $
    putStrLn . ("- " ++) . Text.unpack . prettify . getChainFrom markovTree y
    =<< newStdGen
runAction Read      = readFromFile >> liftIO (putStrLn "...Done")
runAction Help      = liftIO $ putStr help
runAction Quit      = liftIO exitSuccess

markovConsole :: IO ()
markovConsole =
  evalStateT runLoop emptyState
  where
    runLoop :: Console ()
    runLoop = do
      input <- liftIO . runInputT defaultSettings $ getInputLine ">"
      runAction $ fromMaybe Help (readMaybe =<< input)
      runLoop
