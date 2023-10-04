{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Maybe
import Text.ParserCombinators.Parsec
import Text.Read


data CombinationOperator
  = Addition
  | Subtraction

data Term = 
  Term 
    { coefficient_ :: Int
    , indeterminate_ :: Maybe Char
    , exponent_ :: Int 
    }

instance Show Term where
  show (Term {coefficient_, indeterminate_, exponent_}) =
    case indeterminate_ of
      Just variable ->
        if coefficient_ /= 1 then
          show coefficient_ ++ [variable] ++ (if exponent_ /= 1 then "^" ++ show exponent_ else "")
        else
          variable : (if exponent_ /= 1 then "^" ++ show exponent_ else "")
      Nothing ->
        show coefficient_

-- Right now we assume:
--      all terms have positive exponents, 
--      spaces separate each term and operand
terms :: String -> Either ParseError [Term]
terms "" = Right []
terms s = traverse (parse term "") (negateTerms $ words s)

term :: Parser Term
--term "" = Term { coefficient_ = 0, indeterminate_ = Just 'x', exponent_ = 1 }
term = do
  -- parse coefficient, indeterminate and exponent
  coefficient_' <- coefficient <?> "Honk honk"
  indeterminateCharacter <- optionMaybe letter <?> "Hork honk"
  expo <- option 1 expon <?> "Honk hork"
  return 
    Term 
      { coefficient_ = coefficient_'
      , indeterminate_ = indeterminateCharacter
      , exponent_ = expo 
      }

coefficient :: Parser Int
coefficient = do
  negation <- optionMaybe (char '-')
  coefficientCharacter <- option "1" (many digit)
  let coefficient' = readMaybe coefficientCharacter :: Maybe Int
  case (negation, coefficient') of
    (Just _, Just c) -> return (-1 * c)
    (Nothing, Just c) -> return c
    (_, Nothing) -> return 1

  
expon :: Parser Int
expon = do
   skipMany1 (char '^')
   exponent' <- many digit
   return (read exponent')

negateTerms :: [String] -> [String]
negateTerms [] = []
negateTerms [justOne] = [justOne]
negateTerms terms' =
  reverse $
  snd $
  foldl
    (\(mbOperand, result) el ->
      case (mbOperand, el) of
        (Subtraction, e) -> (Addition, ("-" ++ e) : result)
        (Addition, "-") -> (Subtraction, result)
        (_, "+") -> (Addition, result)
        (_, _) -> (Addition, el : result)
    )
    (Addition, [])
    terms'

derivative :: Int -> Term -> Maybe Term
derivative 0 t = Just t
derivative degree (Term {coefficient_, indeterminate_, exponent_})
  | degree > 0 && isJust indeterminate_ = 
      case exponent_ of
        1 -> derivative (degree - 1) Term { coefficient_ = coefficient_, indeterminate_ = Nothing, exponent_ = 1 }
        x -> derivative (degree - 1) Term { coefficient_ = coefficient_ * x, indeterminate_ = indeterminate_, exponent_ = x - 1 }
  | otherwise = Nothing
      
intercalatePlus :: [Term] -> String
intercalatePlus =
  unwords . reverse . go [] 
  where
    go state [] = state
    go [] (t:ts) =
      go [show t] ts
    go state (t@Term {coefficient_, indeterminate_, exponent_}:ts) =
      if coefficient_ < 0 then
        go (show (Term {coefficient_ = -1 * coefficient_, indeterminate_ = indeterminate_, exponent_ = exponent_}) : "-" : state) ts
      else
        go (show t : "+" : state) ts

main :: IO ()
main = do
  putStrLn "Polynomial: "
  l <- getLine
  case terms l of
    Right terms' ->
      print $ intercalatePlus $ mapMaybe (derivative 1) terms'
    Left _ ->
      putStrLn "Could not parse"
