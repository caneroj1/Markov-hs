module Utilities
  (
    getNGramsFromFile
  , prettify
  ) where

import Data.Text (Text)
import qualified Data.Text as Text (words, split, null,
                                    toLower, empty, toTitle,
                                    unwords)
import Data.Char (isPunctuation)
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Resource (runResourceT)
import Data.List (partition)

getNGramsFromFile :: FilePath -> Int -> IO [[Text]]
getNGramsFromFile fp ngrams =
  fixUnbalancedNGrams ngrams <$>
    runResourceT (
      CC.sourceFile fp                        =$=
      CT.lines                                =$=
      CC.concatMap Text.words                 =$=
      CC.filter (not . Text.null)             =$=
      CC.map Text.toLower                     =$=
      CC.slidingWindow ngrams                 $$
      CL.consume)

fixUnbalancedNGrams :: Int -> [[Text]] -> [[Text]]
fixUnbalancedNGrams ngrams ns =
  let (good, bad) = partition ((==) ngrams . length) ns
      allGood     = concat good
      fixedBad    = map (fixList allGood) bad
    in good ++ fixedBad
  where
    fixList pool list = list ++ take (ngrams - length list) pool

prettify :: [Text] -> Text
prettify []       = Text.empty
prettify l@(x:xs) = Text.unwords $ Text.toTitle x : xs
