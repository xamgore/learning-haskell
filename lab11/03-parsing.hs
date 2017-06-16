{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Attoparsec.Text         hiding (number)
import Data.Text            as T    hiding (map)
import Data.Text.IO         as TIO
import System.Environment

spaces = skipWhile isHorizontalSpace
number = (decimal  <* spaces) <?> "number expected"
comma  = (char ',' <* spaces) <?> "comma expected"
pair   = (,) <$> (number <* comma) <*> (number <* endOfLine)
pairs  = manyTill pair endOfInput

tshow = T.pack . show

sum' (a, b) = T.concat [ tshow a, "+", tshow b, "=", tshow $ a + b ]


-- runhaskell 03-parsing.hs '03-input.txt' '03-out.txt'

main = do
    [inf, outf] <- getArgs
    input <- TIO.readFile inf

    -- print $ parse pairs input

    case parseOnly pairs input of
        (Right res) -> do
            let sums = T.unlines $ map sum' res
            TIO.writeFile outf sums
        (Left err) -> print err
