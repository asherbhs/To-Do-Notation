{-# LANGUAGE OverloadedStrings #-}

module Parser where

-- imports ---------------------------------------------------------------------

-- internal
import qualified Types

-- megaparsec
import Text.Megaparsec as Parsec
import qualified Text.Megaparsec.Char as CharParsec
import Text.Megaparsec.Char.Lexer as Lexer

-- misc
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Data.Char (isSpace)

-- types -----------------------------------------------------------------------

type Parser = Parsec.Parsec Void Text

-- parsing functions -----------------------------------------------------------

splitCommand :: Text -> Either Text [Text]
splitCommand t = case Parsec.parse splitCommandP "" t of
    Left e  -> Left $ Text.pack $ Parsec.errorBundlePretty e
    Right l -> Right l

splitCommandP :: Parser [Text]
splitCommandP = CharParsec.space *> Parsec.manyTill wordLexer Parsec.eof

myLexer :: Parser a -> Parser a
myLexer = Lexer.lexeme spaceConsumer
  where
    spaceConsumer = Parsec.lookAhead Parsec.eof <|> CharParsec.space1

wordLexer :: Parser Text
wordLexer = myLexer $
    Parsec.try
        (Parsec.notFollowedBy quote *> takeWhileNot isSpace)
    <|> 
        quote *> takeWhileNot (== '\"') <* quote
        -- Parsec.between can also be used, but tbh I prefer how this looks
  where 
    quote = CharParsec.char '\"'
    takeWhileNot p = Parsec.takeWhileP Nothing (not . p)

intLexer :: Parser Int
intLexer = myLexer Lexer.decimal

boolLexer :: Parser Bool
boolLexer = myLexer $ Parsec.choice 
    [ True  <$ CharParsec.string' "true"
    , False <$ CharParsec.string' "false"
    ]

doneLexer :: Parser Bool
doneLexer = myLexer $ boolLexer <|> True  <$ CharParsec.string' "done"

parseCommand :: Text -> Either Text Types.Command
parseCommand t = case Parsec.parse commandParser "" t of
    Left e  -> Left $ Text.pack $ Parsec.errorBundlePretty e
    Right l -> Right l

commandParser :: Parser Types.Command
commandParser = do
    commandName <- commandNameParser
    case commandName of
        Types.NewTodoCommandName  -> parseNewTodo
        Types.MarkTodoCommandName -> parseMarkTodo

commandNameParser :: Parser Types.CommandName
commandNameParser = myLexer $ Parsec.choice
    [ Types.NewTodoCommandName  <$ CharParsec.string' "new todo"
    , Types.MarkTodoCommandName <$ CharParsec.string' "mark todo"
    ]

parseNewTodo :: Parser Types.Command
parseNewTodo = do
    n <- wordLexer
    p <- intLexer
    return $ Types.NewTodoCommand n p

parseMarkTodo :: Parser Types.Command
parseMarkTodo = do
    n <- wordLexer
    d <- doneLexer
    return $ Types.MarkTodoCommand n d