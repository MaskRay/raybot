{-# LANGUAGE TypeSynonymInstances #-}

module Brainfuck (executeProgram) where

import Control.Applicative
import Control.Arrow
import Control.Monad.State
import Control.Monad.Writer
import Data.Bits
import Data.Word
import Text.ParserCombinators.Parsec hiding ((<|>), many)
import System.IO

data Token = Plus | Minus | Shl | Shr | Leftward | Rightward | Input | Output | Loop [Token]
           deriving (Show)

type Zipper = (([Word8],[Word8]), Int)

program :: CharParser st [Token]
program = many $ Plus <$ char '+' <|> Minus <$ char '-' <|>
          Shl <$ char ';' <|> Shr <$ char '\'' <|>
          Leftward <$ char '<' <|> Rightward <$ char '>' <|>
          Output <$ char '.' <|>
          Loop <$> between (char '[') (char ']') program

execute :: [Token] -> StateT Zipper (Writer String) ()
execute = mapM_ eval

eval :: Token -> StateT Zipper (Writer String) ()
eval Minus = modify $ \((ys,x:xs),p) -> ((ys,x-shiftL 1 p:xs),p)
eval Plus = modify $ \((ys,x:xs),p) -> ((ys,x+shiftL 1 p:xs),p)
eval Shl = modify . second $ \x -> (x+1)`mod`8
eval Shr = modify . second $ \x -> (x-1)`mod`8
eval Leftward = modify $ \((x:xs,ys),_) -> ((xs,x:ys),0)
eval Rightward = modify $ \((xs,y:ys),_) -> ((y:xs,ys),0)
-- eval Input = lift (lift getChar) >>= \y -> modify (first . second $ \(_:ys) -> toEnum (fromEnum y):ys)
eval Output = get >>= tell . return . toEnum . fromEnum . head . snd . fst
eval (Loop p) = get >>= \((_,c:_),_) -> when (c /= 0) (execute p >> eval (Loop p))

executeProgram :: String -> String
executeProgram s = do
  case parse program "" s of
    Left e -> show e
    Right p -> snd . runWriter $ evalStateT (execute p) ((repeat 0,repeat 0),0)