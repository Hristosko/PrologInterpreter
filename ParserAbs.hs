{-
  Module ParseAbs.
  Implements abstractive parser combinator.
-}

module ParserAbs
    (
    module Control.Applicative,
    Parser (Parser),
    satisfy, satisfyE,
    oneOf,
    parseStream, parseStreamErr,
    parseAndDropA, parseAndDropF, parseAndDropS,
    parseChar, parseString
    ) where

import Prelude
import Control.Applicative

type Error = String
newtype Parser a = Parser { parse :: String -> (String, Either Error a) }



--
-- parseStream invokes the parser and returns the result or the error.
-- parseStreamErr will throw if there is an error.
-- Both functions will indicate the place where the error occured.
--
parseStream :: Parser a -> String ->  Either Error a
parseStream par str = case (parse par) str of
  (rest, Left err) -> Left $ err ++ ": " ++ (takeWhile (/= '\n') rest)
  ([], Right a) -> Right a
  (rest, _) -> Left $ "Parsing error: " ++ (takeWhile (/= '\n') rest)

parseStreamErr :: Parser a -> String -> a
parseStreamErr par str = case (parse par) str of
  (rest, Left err) -> error $ err ++ ": " ++ (takeWhile (/= '\n') rest)
  ([], Right a) -> a
  (rest, _) -> error $ "Parsing error: " ++ (takeWhile (/= '\n') rest)


--
-- The parser is instace of Functor, Applicative and Alternative
-- Thus we can parse multiple items and combine them into one and
-- continue with second parser if the first one fails.
--
instance Functor Parser where
  fmap f (Parser st) = Parser $ \stream -> case st stream of
    (res, Left err) -> (res, Left err)
    (res, Right a)  -> (res, Right (f a))

instance Applicative Parser where
  pure a = Parser $ \stream -> (stream, Right a)
  Parser ff <*> Parser xx = Parser $ \stream -> case ff stream of
    (stream', Left err) -> (stream', Left err)
    (stream', Right f)  -> case xx stream' of
      (stream'', Left err) -> (stream'', Left err)
      (stream'', Right x)  -> (stream'', Right (f x))

instance Alternative Parser where
  empty = Parser $ \stream -> (stream, Left "Empty")
  (<|>) = orElse
  many = parseMany
  some = parseSome

--
-- parses a character that satisfies the predicate
--
satisfy :: (Char->Bool) -> Parser Char
satisfy p = satisfyE p "Satisfy::Predicate not satisfied"

--
-- satisfy with custom error message
--
satisfyE :: (Char->Bool) -> String -> Parser Char
satisfyE p errMsg = Parser $ \stream -> case stream of
  []                  -> ([], Left "Satisfy::End of Stream")
  (c:cs) | p c        -> (cs, Right c)
         | otherwise  -> (cs, Left errMsg)

--
-- parses a character that is an element of the list
--
oneOf :: [Char] -> String -> Parser Char
oneOf cs errMsg = satisfyE (`elem` cs) errMsg

--
-- If the first parser fails, returns the result of the second one.
--
orElse :: Parser a -> Parser a -> Parser a
orElse (Parser f) (Parser g) = Parser $ \stream -> case f stream of
  (_,       Left err) -> g stream
  (stream', Right a) -> (stream', Right a)


--
-- Parses the expresion as many times as possible.
-- 0 is also valid result ([]) thus it never returns an error
--
parseMany :: Parser a -> Parser [a]
parseMany (Parser f) = Parser loop
  where
    loop stream = case f stream of
      (_      , Left err) -> (stream, Right [])
      (stream', Right a) -> case loop stream' of
        (stream'', Left err) -> (stream'', Left err)
        (stream'', Right as) -> (stream'', Right $ a : as)

--
-- Similar to parseMany parses the expression as many times as possible.
-- However also requires that it can parsed at least one time.
-- If it can't - returns the respective error.
--
parseSome :: Parser a -> Parser [a]
parseSome (Parser f) = Parser $ \stream  -> case f stream of
  (stream', Left err) -> (stream', Left err)
  (stream', Right a)  ->
    let (Parser manyParser) = parseMany (Parser f)
    in case manyParser stream' of
      (stream'', Left err) -> (stream'', Left err)
      (stream'', Right as) -> (stream'', Right $ a : as)

--
-- parseAndDrop* functions cmbine two/three parsers into one by ignorring
-- the result of:
-- parseAndDropF - the first of the two parsers
-- parseAndDropS - the second of the two parsers
-- parseAndDropA - the first and the third
--
parseAndDropS :: Parser a -> Parser b -> Parser a
parseAndDropS pa pb = dropSecond <$> pa <*> pb
  where
    dropSecond x _ = x

parseAndDropF :: Parser a -> Parser b -> Parser b
parseAndDropF pa pb = dropFirst <$> pa <*> pb
  where
    dropFirst _ x = x

parseAndDropA :: Parser a -> Parser b -> Parser c -> Parser b
parseAndDropA pa pb pc = dropFirstAndLast <$> pa <*> pb <*> pc
  where
    dropFirstAndLast _ x _ = x

--
-- Parses a character.
--
parseChar :: Char -> Parser Char
parseChar c = satisfyE (== c) ("Expected " ++ [c])

--
-- Parses a whole string
--
parseString :: String -> Parser String
parseString []      = pure []
parseString (c:cs)  = (:) <$> parseChar c <*> parseString cs
