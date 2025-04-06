{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task1 where

import Text.Read (readMaybe)
import Data.Maybe()

-- * Expression data type

-- | Representation of integer arithmetic expressions comprising
-- - Literals of type 'a'
-- - Binary operations 'Add' and 'Mul'
data IExpr =
    Lit Integer
  | Add IExpr IExpr
  | Mul IExpr IExpr
  deriving (Show, Eq)

-- * Evaluation

-- | Evaluates given 'IExpr'
--
-- Usage example:
--
-- >>> evalIExpr (Lit 2)
-- 2
-- >>> evalIExpr (Add (Lit 2) (Lit 3))
-- 5
-- >>> evalIExpr (Add (Mul (Lit 3) (Lit 2)) (Lit 3))
-- 9
--
evalIExpr :: IExpr -> Integer
evalIExpr (Lit n)   = n
evalIExpr (Add e1 e2) = evalIExpr e1 + evalIExpr e2
evalIExpr (Mul e1 e2) = evalIExpr e1 * evalIExpr e2

-- * Parsing

-- | Class of parseable types
class Parse a where
  -- | Parses value 'a' from given string
  -- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
  parse :: String -> Maybe a

-- | Helper function to process RPN tokens using a stack
processTokenIExpr :: Maybe [IExpr] -> String -> Maybe [IExpr]
processTokenIExpr Nothing _ = Nothing
processTokenIExpr (Just stack) token =
  case token of
    "+" -> case stack of
             (e1:e2:rest) -> Just (Add e2 e1 : rest) 
             _            -> Nothing 
    "*" -> case stack of
             (e1:e2:rest) -> Just (Mul e2 e1 : rest) 
             _            -> Nothing
    _   -> case readMaybe token :: Maybe Integer of
             Just n  -> Just (Lit n : stack)
             Nothing -> Nothing

-- | Parses given expression in Reverse Polish Notation
-- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
--
-- Usage example:
--
-- >>> parse "2" :: Maybe IExpr
-- Just (Lit 2)
-- >>> parse "2 3 +" :: Maybe IExpr
-- Just (Add (Lit 2) (Lit 3))
-- >>> parse "3 2 * 3 +" :: Maybe IExpr
-- Just (Add (Mul (Lit 3) (Lit 2)) (Lit 3))
-- >>> parse "2 +" :: Maybe IExpr
-- Nothing
-- >>> parse "2 3" :: Maybe IExpr
-- Nothing
--
instance Parse IExpr where
  parse s = case foldl processTokenIExpr (Just []) (words s) of
              Just [result] -> Just result
              _             -> Nothing

instance Parse Integer where
  parse = readMaybe

-- * Evaluation with parsing

-- | Parses given expression in Reverse Polish Notation and evaluates it
--
-- Returns 'Nothing' in case the expression could not be parsed.
--
-- Usage example:
--
-- >>> evaluateIExpr "2"
-- Just 2
-- >>> evaluateIExpr "2 3 +"
-- Just 5
-- >>> evaluateIExpr "3 2 * 3 +"
-- Just 9
-- >>> evaluateIExpr "2 +"
-- Nothing
-- >>> evaluateIExpr "2 3"
-- Nothing
--
evaluateIExpr :: String -> Maybe Integer
evaluateIExpr s = evalIExpr <$> (parse s :: Maybe IExpr)
