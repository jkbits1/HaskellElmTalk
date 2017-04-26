module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ show $ fac 3.0 

fac :: Number -> Number
fac = go 1.0
  where 
  go :: Number -> Number -> Number
  go acc 1.0 = acc
  go acc n = go (acc * n) (n - 1.0)
