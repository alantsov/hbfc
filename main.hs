module Main where
import System.Environment
import Control.Monad
import Model (transform)
import Parser (parseBF)
import Asm (compile)
import Data.Either

main = getArgs >>= mapM_ compileFile

compileFile fileName = do content <- readFile fileName
                          writeOut fileName (parseBF content >>= (\bf -> Right $ transform bf) >>= (\bf -> Right $ compile bf))

writeOut fileName (Right str) = writeFile (fileName ++ ".s") str
writeOut fileName (Left error_message) = putStrLn "smth went wrong" >> (putStrLn $ show error_message)
