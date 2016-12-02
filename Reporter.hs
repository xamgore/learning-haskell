{-# LANGUAGE TemplateHaskell #-}

module Reporter where


import Language.Haskell.TH
import Control.Monad


rpt :: Integer -> ExpQ
rpt n = [| replicateM_ n . putStrLn |]


replicateT n = [| \x -> $(tupE $ replicate n [|x|]) |]
