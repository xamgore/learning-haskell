{-# LANGUAGE TemplateHaskell #-}

module Reporter where

import Language.Haskell.TH
import Control.Monad


rpt :: Integer -> ExpQ
rpt n = [| replicateM_ n . putStrLn |]

replicateT :: Int -> ExpQ
replicateT n = [| \x -> $(tupE $ replicate n [|x|]) |]


applyT :: Int -> ExpQ
applyT n = do
    funs <- gen 'f'
    args <- gen 'a'

    let input  = (TupP . map VarP) <$> [funs, args]
    let output = TupE $ zipWith AppE (map VarE funs) (map VarE args)

    return $ LamE input output
    where gen ch  = sequence (newName <$> replicate n [ch])


countConstrs :: Name -> ExpQ
countConstrs name = do
    TyConI (DataD _ _ _  _ cons _) <- reify name
    let n = length cons
    [|n|]
