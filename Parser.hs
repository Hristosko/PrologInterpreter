{-
  Module Parser
  Implements a parser for prolog.
-}

module Parser
    (
    module ParserAbs,
    module SyntaxTree,
    parseProgram, parseGoal, renameGoal
    ) where

import SyntaxTree
import ParserAbs
import Control.Applicative

--
-- special symbols in prolog
--
specialSymbols :: String
specialSymbols = ",._(): \t\n"

parseAndSkipNL :: Parser a -> Parser a
parseAndSkipNL par = parseAndDropS par (many $ parseChar '\n')

--
-- parses a lowercase letter
--
parseLowercase :: Parser Char
parseLowercase = oneOf ['a'..'z'] "Expected lowercase letter"

--
-- parses a uppercase letter
--
parseUppercase :: Parser Char
parseUppercase = oneOf ['A'..'Z'] "Expected uppercase letter"

--
-- parses a symbol that isn't marked as special ie. isn't reserved nor empty
--
parseNotSpecialSymbol :: Parser Char
parseNotSpecialSymbol = satisfy $ \c -> not (elem c specialSymbols)

--
-- Parses a '.' - the last symbol in facts, clauses, goals
--
parseEnd :: Parser Char
parseEnd = parseChar '.'

--
-- Parses an expression and consumes '.' after it.
--
parseWithEnd :: Parser a -> Parser a
parseWithEnd par = parseAndDropS par parseEnd

--
-- Parses an expression in parenthesis
--
parseWithParens :: Parser a -> Parser a
parseWithParens par = parseAndDropA (parseChar '(') par (parseChar ')')

--
-- Pareses all cheractes until a special symbol is reached.
--
parseStringNoSpecial :: Parser String
parseStringNoSpecial = many parseNotSpecialSymbol

--
-- parses an aton ie identifier
--
parseAtom :: Parser Atom
parseAtom = (:) <$> parseLowercase <*> parseStringNoSpecial

--
-- parses a variable
--
parseVariable :: Parser Variable
parseVariable = (:) <$> parseUppercase <*> parseStringNoSpecial

--
-- parses a wildcard
--
parseWildcard :: Parser Char
parseWildcard = parseChar '_'

--
-- parses a term
--
parseTerm :: Parser Term
parseTerm = parseWildcardTerm <|> parseVariableTerm <|> parseCompTerm
  where
    parseWildcardTerm = wildcard <$> parseWildcard
    parseVariableTerm = Var <$> parseVariable
    parseList = (parseWithParens parseList') <|> pure []
    parseList' = (:) <$> (parseAndSkipNL parseTerm) <*> many (parseAndDropF (parseChar ',') parseTerm)
    parseCompTerm = Compose <$> parseAtom <*> parseList
    wildcard _ = Wildcard

--
-- parses a clause (or fact)
--
parseClause :: Parser Clause
parseClause = (,) <$> parseTerm <*> (parseAndSkipNL $ parseWithEnd parseTerms)
  where
    parseTerms = (parseAndDropF (parseString ":-") parseTerms') <|> pure []
    parseTerms' = (:) <$> parseTerm <*> many (parseAndDropF (parseChar ',') parseTerm)

--
-- parses a program
--
parseProgram :: Parser Program
parseProgram = some parseClause

--
-- renameTerm and renameGoal enable us to rename the variable in a Term (Goal)
-- in order to mark them as unique.
-- for exaple: f(X,Y) f(a, X)
-- should unify with X = a and all Y
-- However thie unifier will apply the substitution X = a, Y = X
-- and conclude that X = Y = a since it might consider the X's for one variable.
-- renameXXX might be used to mark one of them as unique.
-- In this implementation such error might occure if the variables in the initial goal
-- are used in a clause. Thus renaming the variables in the initial goal prevents this issue.
--
renameTerm :: Term -> Term
renameTerm Wildcard       = Wildcard
renameTerm (Var v)        = Var $ "_" ++ v
renameTerm (Compose f ts) = Compose f (map renameTerm ts)

renameGoal:: Goal -> Goal
renameGoal = map renameTerm

--
-- parses a goal
--
parseGoal :: Parser Goal
parseGoal = some $ parseWithEnd (parseAndDropF (parseString "?-") parseTerm)
