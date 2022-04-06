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
import qualified Data.Maybe as Maybe
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
wordLexer = myLexer
    (
        Parsec.try
            (Parsec.notFollowedBy quote *> takeWhileNot isSpace)
        <|>
            quote *> takeWhileNot (== '\"') <* quote
            -- Parsec.between can also be used, but tbh I prefer how this looks
    ) <?> "a word (optionally multiple in \"double quotes\")"
  where
    quote = CharParsec.char '\"'
    takeWhileNot p = Parsec.takeWhile1P Nothing (not . p)

intLexer :: Parser Int
intLexer = myLexer Lexer.decimal

priorityLexer :: Parser Types.Priority
priorityLexer = myLexer
    (
        Parsec.choice
            [ Types.UrgentPriority <$ Parsec.choice (map CharParsec.string'
                [ "urgent"
                , "u"
                , "4"
                ])
            , Types.HighPriority <$ Parsec.choice (map CharParsec.string'
                [ "high"
                , "hi"
                , "h"
                , "3"
                ])
            , Types.MediumPriority <$ Parsec.choice (map CharParsec.string'
                [ "medium"
                , "m"
                , "2"
                ])
            , Types.LowPriority <$ Parsec.choice (map CharParsec.string'
                [ "low"
                , "lo"
                , "l"
                , "1"
                ])
            , Types.NoPriority <$ Parsec.choice (map CharParsec.string'
                [ "none"
                , "no"
                , "n"
                , "0"
                ])
            ]
    ) <?> "a priority (e.g. \"high\")"

boolParser :: Parser Bool
boolParser = Parsec.choice
    [ True  <$ CharParsec.string' "true"
    , False <$ CharParsec.string' "false"
    ]

boolLexer :: Parser Bool
boolLexer = myLexer boolParser

doneLexer :: Parser Bool
doneLexer = myLexer $ boolParser <|> True <$ CharParsec.string' "done"

parseCommand :: Text -> Either Text Types.Command
parseCommand t = case Parsec.parse commandParser "" t of
    Left e  -> Left $ Text.pack $ Parsec.errorBundlePretty e
    Right l -> Right l

commandParser :: Parser Types.Command
commandParser = do
    commandName <- commandNameParser
    case commandName of
        Types.QuitCommandName     -> return Types.QuitCommand
        Types.HelpCommandName     -> return Types.HelpCommand
        Types.NewTodoCommandName  -> parseNewTodo
        Types.MarkTodoCommandName -> parseMarkTodo

commandNameParser :: Parser Types.CommandName
commandNameParser = myLexer
    (
        Parsec.choice
            [ Types.QuitCommandName
                <$ (CharParsec.string' "quit" <|> CharParsec.string' "q")
            , Types.HelpCommandName     <$ CharParsec.string' "help"
            , Types.NewTodoCommandName  <$ CharParsec.string' "new todo"
            , Types.MarkTodoCommandName <$ CharParsec.string' "mark todo"
            ]
    ) <?> "a valid command"

parseNewTodo :: Parser Types.Command
parseNewTodo = do
    n <- wordLexer
    p <- Parsec.optional priorityLexer
    Parsec.eof
    return $ Types.NewTodoCommand n $ Maybe.fromMaybe Types.NoPriority p

parseMarkTodo :: Parser Types.Command
parseMarkTodo = do
    n <- wordLexer
    d <- doneLexer
    Parsec.eof
    return $ Types.MarkTodoCommand n d