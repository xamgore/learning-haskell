{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Attoparsec.Text         hiding (number)
import Data.Text            as T    hiding (map)
import Data.Text.IO         as TIO
import System.Environment

spaces = skipWhile isHorizontalSpace
number = decimal  <* spaces
comma  = char ',' <* spaces
pair   = (,) <$> (number <* comma) <*> (number <* endOfLine)
pairs  = many pair <* endOfInput

tshow = T.pack . show

sum' (a, b) = T.concat [ tshow a, "+", tshow b, "=", tshow $ a + b ]


-- runhaskell 03-parsing.hs '03-input.txt' '03-out.txt'

main = do
    [inf, outf] <- getArgs
    input <- TIO.readFile inf
    let (Right res) = parseOnly pairs input
    let sums = T.unlines $ map sum' res
    TIO.writeFile outf sums
