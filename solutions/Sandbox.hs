
{-# OPTIONS_GHC -Wno-missing-fields #-}
module Sandbox where

import Provided

import Data.List ( intersperse, sort )
import Data.Char (isLetter, toUpper)
import Control.Monad.State


----------
type Count = Int
reverseWithCount :: [a] -> State Count [a]
reverseWithCount list = state (\count ->  (reverse list, count+1 ))

ex :: (String, Count)
ex = runState (reverseWithCount "aye" >>= reverseWithCount >>= reverseWithCount ) 1
