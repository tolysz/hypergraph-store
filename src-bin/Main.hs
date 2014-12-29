
{-# Language OverloadedStrings #-}

module Main where

import qualified Database.HypherGraph as HG

main = do
   putStrLn "Bam"
   -- b <- 
   HG.openNode "test/amba"
   --print =<< HG.runHGM HG.stats b
