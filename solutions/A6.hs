{-# OPTIONS_GHC -Wno-missing-fields #-}
module A6 where

import Provided

import Data.List ( intersperse, sort )
import Data.Char (isLetter, toUpper)
import Control.Monad.State


-- *** A6-0: WARM-UP *** --

-- Q#01
type Chances = Int
type Guess = String
type Move = Char
type Secret = String
type Dictionary = [String]



-- Q#02
data GameException = InvalidWord | InvalidMove | RepeatMove | GameOver

-- Q#03

lengthInRange :: Secret -> Bool
lengthInRange sc =  length sc > a && length sc < b
                    where (a,b) = _LENGTH_

-- Q#04

invalidMove :: Move -> Bool
invalidMove move = isLetter move

-- Q#05

revealLetters :: Move -> Secret -> Guess -> Guess
revealLetters m sc g = zipWith (\x y -> if invalidMove m && m ==x then x else y ) sc g

-- Q#06

updateChances :: Move -> Secret -> Chances-> Chances
updateChances m sc c = if invalidMove m && elem m sc
                          then c
                          else c-1

-- Q#07

setSecret :: IO Secret
setSecret = do
  putStr "Enter a secret word:\t"
  showInput False
  sc <- getLine
  showInput True
  _SPACE_
  return sc



-- *** A6-1: Records & Instances *** --

-- Q#08
data Game = Game {getSecret :: Secret
                  ,getCurrentGuess :: Guess
                  ,getListMove :: [Move]
                  ,getChances :: Chances}
                  -- deriving (Eq,Ord,Show)


-- Q#09

repeatedMove :: Move -> Game -> Bool
repeatedMove m g = m `elem` getCurrentGuess g

-- Q#10

makeGame :: Secret -> Game
makeGame sc = Game {
              getSecret = map toUpper sc
              , getCurrentGuess = map (const '_') sc
              , getListMove = []
              , getChances = _CHANCES_
}

-- Q#11

updateGame :: Move -> Game -> Game
updateGame m g = Game {getCurrentGuess = revealLetters m sc cg
                      ,getListMove = m : lm
                      ,getChances = updateChances m sc ch }
          where sc = getSecret g
                cg = getCurrentGuess g
                ch = getChances g
                lm = getListMove g

-- Q#12

showGameHelper :: String -> [Char] -> Int -> String
showGameHelper game moves chances = unlines [
      _STARS_
    , "\tSecret Word:\t" ++ intersperse ' ' game ++ "\n"
    , "\tGuessed:\t" ++ intersperse ' ' (sort moves) ++ "\n"
    , "\tChances:\t" ++ show chances
    , _STARS_
    ]

instance Show Game where
  show (Game sc g ml ch) = showGameHelper sc ml ch

-- Q#13
instance Show GameException where
  show InvalidWord = show (concat ["invalid input"," with lower bound: ",lb," and higher bound: ",ub])
    where
      lb = show $ fst _LENGTH_
      ub = show $ snd _LENGTH_
  show InvalidMove = show "invalid Move"
  show RepeatMove  = show "repeat Move"
  show GameOver    = show "repeat Move"


-- *** A6-2: Exception Contexts *** --

-- Q#14

toMaybe :: Bool -> a -> Maybe a
-- toMaybe b a
--   | not b   = Nothing
--   | otherwise = Just a
toMaybe b a = if b then Just a else Nothing

-- Q#15

validateSecret :: (Secret -> Bool) -> Secret -> Either GameException Secret
validateSecret f sc  = if f sc then Right sc 
                        else Left InvalidWord

-- Q#16

-- hasValidChars sc = validateSecret (\sc -> foldr (\s b -> isLetter s && b) True sc) 


isValidLength = undefined


isInDict = undefined

-- Q#17

validateNoDict = undefined

validateWithDict = undefined

-- Q#18

processTurn = undefined

