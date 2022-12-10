{-# OPTIONS_GHC -Wall #-}

import Text.Parsec (string, option, ParseError, parse, eof, (<|>), digit)
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (many1)
import Debug.Trace (trace)

data Instruction = Add Int | Noop
    deriving (Eq, Show)

type State = (Int, Int)


parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

newline :: Parser String
newline = (string "\n") <|> (string "\r\n")

parseInt :: Parser Int
parseInt = (\s n -> read (s ++ n)) <$> (option "" $ string "-") <*> many1 digit

parseAdd :: Parser [Instruction]
parseAdd = (\n -> [Noop, Add n]) <$> (string "addx " *> parseInt <* newline)

parseNoop :: Parser [Instruction]
parseNoop = [Noop] <$ (string "noop" *> newline)

parseFile :: String -> Either ParseError [Instruction]
parseFile file = parseWithEof (concat <$> (many1 (parseNoop <|> parseAdd))) file


executeInstruction :: State -> Instruction -> State
executeInstruction (ncycle, register) instruction = case instruction of
    Noop -> (ncycle + 1, register)
    Add n -> (ncycle + 1, register + n)

totalSignalStrength :: [Instruction] -> String
totalSignalStrength instructions = show $
    sum .
    map (uncurry (*)) .
    filter (\(ncycles, _) -> (ncycles - 20) `mod` 40 == 0) $ states
        where states = scanl executeInstruction (1, 1) instructions

insertNewlines :: [Char] -> String
insertNewlines [] = ""
insertNewlines pixels = "\n\t" ++ (take 40 pixels) ++ (insertNewlines $ drop 40 pixels)

renderPixel :: State -> Char
renderPixel (ncycle, register) = if abs (ncycle `mod` 40 - register) <= 1
    then '█'
    else '░'

render' :: State -> [Instruction] -> [Char]
render' _ [] = []
render' state (instruction:instructions) = renderPixel state : render' newState instructions
    where newState = executeInstruction state instruction

render :: [Instruction] -> [Char]
render = insertNewlines . render' (0, 1)

main :: IO ()
main = do
    file <- readFile "input"
    case parseFile file of
        Left e -> putStrLn $ "Error: " ++ show e
        Right instructions -> do
            putStrLn $ "Part 1: " ++ totalSignalStrength instructions
            putStrLn $ "Part 2: " ++ render instructions
