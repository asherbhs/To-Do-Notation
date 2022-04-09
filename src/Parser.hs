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



--------------------------------------------------------------------------------

-- tests

splitCommand :: Text -> Either Text [Text]
splitCommand t = case Parsec.parse splitCommandP "" t of
    Left e  -> Left $ Text.pack $ Parsec.errorBundlePretty e
    Right l -> Right l

splitCommandP :: Parser [Text]
splitCommandP = CharParsec.space *> Parsec.manyTill wordLexer Parsec.eof



-- helpers

myLexer :: Parser a -> Parser a
myLexer = Lexer.lexeme $ Parsec.lookAhead Parsec.eof <|> CharParsec.space1

stringChoice :: [Text] -> Parser Text
stringChoice = Parsec.choice . map CharParsec.string'



-- basic parsers

boolParser :: Parser Bool
boolParser = Parsec.choice
    [ True  <$ CharParsec.string' "true"
    , False <$ CharParsec.string' "false"
    ]

wordParser :: Parser Text
wordParser = Parsec.label "a word (optionally multiple in \"double quotes\")" 
    $ Parsec.try 
        (Parsec.notFollowedBy quote *> takeWhileNot isSpace)
    <|> Parsec.try
        (quote *> takeWhileNot (== '\"') <* quote)
    -- Parsec.between can also be used, but tbh I prefer how this looks
  where
    quote = CharParsec.char '\"'
    takeWhileNot p = Parsec.takeWhile1P Nothing (not . p)



-- basic lexers

wordLexer :: Parser Text
wordLexer = myLexer wordParser

intLexer :: Parser Int
intLexer = myLexer Lexer.decimal

boolLexer :: Parser Bool
boolLexer = myLexer boolParser



-- specified lexers

priorityLexer :: Parser Types.Priority
priorityLexer = Parsec.label "a valid priority (e.g. 'high', 'h', '3')"  
    $ myLexer $ Lexer.decimal <|> Parsec.choice
        [ Types.UrgentPriority <$ stringChoice
            [ "urgent"
            , "urg"
            , "u"
            , "*"
            ]
        , Types.HighPriority <$ stringChoice
            [ "high"
            , "hi"
            , "h"
            , "!"
            ]
        , Types.MediumPriority <$ stringChoice
            [ "medium"
            , "med"
            , "m"
            , ":"
            ]
        , Types.LowPriority <$ stringChoice
            [ "low"
            , "lo"
            , "l"
            , "."
            ]
        , Types.NoPriority <$ stringChoice
            [ "none"
            , "no"
            , "n"
            , "-"
            ]
        ]

habitRepeatLexer :: Parser Types.HabitRepeat
habitRepeatLexer = Parsec.label 
    "how the habit should recur (e.g 'daily', 'every monday')"
    $ myLexer 
    $ Parsec.choice
        [ Types.EveryDay       <$ stringChoice ["daily", "every day"]
        , Types.EveryOtherDay  <$ CharParsec.string' "\"every other day\""
        , Types.EveryWeekday   <$ CharParsec.string' "\"every weekday\""
        , Types.EveryMonday    <$ CharParsec.string' "\"every monday\""
        , Types.EveryTuesday   <$ CharParsec.string' "\"every tuesday\""
        , Types.EveryWednesday <$ CharParsec.string' "\"every wednesday\""
        , Types.EveryThursday  <$ CharParsec.string' "\"every thursday\""
        , Types.EveryFriday    <$ CharParsec.string' "\"every friday\""
        , Types.EverySaturday  <$ CharParsec.string' "\"every saturday\""
        , Types.EverySunday    <$ CharParsec.string' "\"every sunday\""
        ]

doneLexer :: Parser Bool
doneLexer = myLexer $ boolParser <|> True <$ CharParsec.string' "done"



-- command parsing

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
        Types.NewHabitCommandName -> parseNewHabit

commandNameParser :: Parser Types.CommandName
commandNameParser = myLexer (Parsec.choice
    [ Types.QuitCommandName     <$ stringChoice ["quit", "q"]
    , Types.HelpCommandName     <$ stringChoice ["help", "h"]
    , Types.NewTodoCommandName  <$ stringChoice ["new todo", "nt"]
    , Types.NewHabitCommandName <$ stringChoice ["new habit", "nh"]
    ]) <?> "a valid command"

parseNewTodo :: Parser Types.Command
parseNewTodo = do
    n <- wordLexer
    p <- Parsec.optional priorityLexer
    Parsec.eof
    return $ Types.NewTodoCommand n $ Maybe.fromMaybe Types.NoPriority p

parseNewHabit :: Parser Types.Command
parseNewHabit = do
    n <- wordLexer
    r <- Parsec.optional $ Parsec.some habitRepeatLexer
    Parsec.eof
    return $ Types.NewHabitCommand n $ Maybe.fromMaybe [Types.EveryDay] r