module Main where

import Text.Read
import System.Environment
import System.Exit
import Control.Monad
import Data.Bits

data Config = Config {
    rule :: Int,
    start :: Int,
    line :: Int,
    window :: Int,
    move :: Int
} deriving (Show)

defaultConf :: Config
defaultConf = Config {rule = -1, start = 0, line = -1, window = 80, move = 0}

initLine :: Config -> String
initLine conf@(Config r s l 0 m) = ""
initLine conf@(Config r s l w m) = ' ' : initLine (conf {window = w - 1})

generateNextLine :: String -> Int -> String
generateNextLine (' ':' ':' ':str) rule = case testBit rule 0 of
    True -> '*' : generateNextLine (' ':' ':str) rule
    False -> ' ' : generateNextLine (' ':' ':str) rule
generateNextLine (' ':' ':'*':str) rule = case testBit rule 1 of
    True -> '*' : generateNextLine (' ':'*':str) rule
    False -> ' ' : generateNextLine (' ':'*':str) rule
generateNextLine (' ':'*':' ':str) rule = case testBit rule 2 of
    True -> '*' : generateNextLine ('*':' ':str) rule
    False -> ' ' : generateNextLine ('*':' ':str) rule
generateNextLine (' ':'*':'*':str) rule = case testBit rule 3 of
    True -> '*' : generateNextLine ('*':'*':str) rule
    False -> ' ' : generateNextLine ('*':'*':str) rule
generateNextLine ('*':' ':' ':str) rule = case testBit rule 4 of
    True -> '*' : generateNextLine (' ':' ':str) rule
    False -> ' ' : generateNextLine (' ':' ':str) rule
generateNextLine ('*':' ':'*':str) rule = case testBit rule 5 of
    True -> '*' : generateNextLine (' ':'*':str) rule
    False -> ' ' : generateNextLine (' ':'*':str) rule
generateNextLine ('*':'*':' ':str) rule = case testBit rule 6 of
    True -> '*' : generateNextLine ('*':' ':str) rule
    False -> ' ' : generateNextLine ('*':' ':str) rule
generateNextLine ('*':'*':'*':str) rule = case testBit rule 7 of
    True -> '*' : generateNextLine ('*':'*':str) rule
    False -> ' ' : generateNextLine ('*':'*':str) rule
generateNextLine (a:b:[]) _ = ""

algorithm :: Config -> String -> IO ()
algorithm conf@(Config r s 0 w m) str = putStrLn str
algorithm conf@(Config r s (-1) w m) str =
    putStrLn str >>
    algorithm conf (generateNextLine (' ' : str ++ " ") r)
algorithm conf@(Config r 0 l w m) str =
    putStrLn str >>
    algorithm (conf {line = l - 1}) (generateNextLine (' ' : str ++ " ") r)
algorithm conf@(Config r s l w m) str =
    algorithm (conf {start = s - 1}) (generateNextLine (' ' : str ++ " ") r)

readPositiveInt :: String -> Maybe Int
readPositiveInt s = do
    i <- readMaybe s
    guard $ i >= 0
    Just i

parseArgs :: [String] -> Either String Config
parseArgs [] = Right defaultConf
parseArgs ("--rule":x:xs) = case readPositiveInt x of
    Just nb -> do
        conf <- parseArgs xs
        Right conf {rule = nb}
    Nothing -> Left ("Invalid Argument: " ++ x)
parseArgs ("--start":x:xs) = case readPositiveInt x of
    Just nb -> do
        conf <- parseArgs xs
        Right conf {start = nb}
    Nothing -> Left ("Invalid Argument: " ++ x)
parseArgs ("--lines":x:xs) = case readPositiveInt x of
    Just nb -> do
        conf <- parseArgs xs
        Right conf {line = nb}
    Nothing -> Left ("Invalid Argument: " ++ x)
parseArgs ("--window":x:xs) = case readPositiveInt x of
    Just nb -> do
        conf <- parseArgs xs
        Right conf {window = nb}
    Nothing -> Left ("Invalid Argument: " ++ x)
parseArgs ("--move":x:xs) = case readMaybe x of
    Just nb -> do
        conf <- parseArgs xs
        Right conf {move = nb}
    Nothing -> Left ("Invalid Argument: " ++ x)
parseArgs (x:xs) = Left ("Invalid Argument: " ++ x)

placeInitStar :: String -> Int -> String
placeInitStar (c:str) 0 = '*' : str
placeInitStar (c:str) i = c : placeInitStar str (i - 1)

checkConfigValues :: Config -> IO ()
checkConfigValues conf@(Config r s l w m) = if r < 0 || r > 255 then
    putStr "Invalid value for rule: " >> print r >> exitWith (ExitFailure 84)
    else putStr ""

setConf :: Config -> Config
setConf conf@(Config r s (-1) w m) = conf
setConf conf@(Config r 0 l w m) = conf {line = l - 1}
setConf conf@(Config r s l w m) = conf {start = s - 1}

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Right conf@(Config r s l w m) ->
            checkConfigValues conf >>
            algorithm (setConf conf) firstLine
            where
                firstLine = placeInitStar initialLine ((w `div` 2) + m)
                initialLine = initLine conf
        Left str -> putStrLn str >> exitWith (ExitFailure 84)
