module Worder where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

wrds = QuasiQuoter
        { quoteExp = listE . map (listE . map stringE . words) . lines
        , quotePat = undefined
        , quoteType = undefined
        , quoteDec = undefined
        }
