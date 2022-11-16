module Main where
import System.Environment
import Data.List
import System.IO
import Data.Either
import Text.Regex.Posix

-- Three tuple helpers
first :: (a, b, c, d) -> a
first  (res, _, _, _)  = res

second :: (a, b, c, d) -> b
second (_, res, _, _)  = res

third :: (a, b, c, d) -> c
third  (_, _, res, _)  = res

fourth :: (a, b, c, d) -> d
fourth (_, _, _, d) = d

checkflag :: [String] -> String
checkflag flags
 | length flags > 1   = "ferror1"
 | null flags         = "grab"
 | head flags == "-h" = "help"
 | head flags == "-s" = "swap"
 | head flags == "-d" = "delete"
 | otherwise          = "ferror2"

-- Help
help = "-------------------------------------------------------------------------------------------\n\
\ReX Help: \n\
\To use stdin as input, leave <input> blank...\n\
\Help:   -h ->  \n\
\Default:   ->  rex    <expression> <input>, returns only text that matches expression.\n\
\Swap:   -s ->  rex -s <expression> <replacement> <input>, swaps first match with replacement.\n\
\Delete: -d ->  rex -d <expression> <input>, deletes text that matches expression.\n\
\--------------------------------------------------------------------------------------------\n"

-- Grab (default arg)
grab :: String -> String -> String
grab exp input = foldr (++) "" (input =~ exp :: [String])

grabIO :: [String] -> IO ()
grabIO params = do
    case params of
        [exp]        -> do input <- getLine
                           print $ grab exp input
        [exp, input] -> print $ grab exp input
        _            -> print "<expression> argument must be provided"

-- Swap
swap :: String -> String ->  String -> String
swap exp rplc input = first res ++ rplc ++ third res
    where res = input =~ exp :: (String, String, String, [String])

swapIO :: [String] -> IO ()
swapIO params = do
    case params of
        [exp, rplc]        -> do input <- getLine
                                 print $ swap exp rplc input
        [exp, rplc, input] -> print $ swap exp rplc input
        _                  -> print "<expression> <replacement> arguments must be provided."

-- Delete
del :: String -> String -> String
del exp input = first res ++ third res
    where res = input =~ exp :: (String, String, String, [String])

delIO :: [String] -> IO ()
delIO params = do
    case params of
        [exp]        -> do input <- getLine
                           print $ del exp input
        [exp, input] -> print $ del exp input  
        _            -> print "Invalid arguments to delete"

main :: IO ()
main = do
    args <- getArgs
    let (flags, params) = foldr splt ([], []) args 
                            where splt s (flags, params) = if '-' `elem` s && length s == 2 
                                                           then (s : flags, params)
                                                           else (flags, s : params)
    case checkflag flags of
        "help"    -> putStr help 
        "grab"    -> grabIO params
        "swap"    -> swapIO params
        "delete"  -> delIO params
        "ferror1" -> print "Too many flags provided"
        "ferror2" -> print "Invalid flag, use -h to see available flags"