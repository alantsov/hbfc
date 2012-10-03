module Parser (parseBF) where

import Text.ParserCombinators.Parsec
import Model

parseBF :: String -> Either ParseError [BF]
parseBF s = parse bf "unknown" s

bf = many (simple <|> loop)

simple = ( (char ',' >> return Read)
         <|> (char '.' >> return Write)
         <|> (char '+' >> return Inc)
         <|> (char '-' >> return Dec)
         <|> (char '<' >> return LeftShift)
         <|> (char '>' >> return RightShift)
         )

loop = do char '['
          items <- bf
          char ']'
          return $ Loop items
