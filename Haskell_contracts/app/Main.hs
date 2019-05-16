module Main where

import Prelude hiding (and, or, truncate)
import Lib

-- Zero-coupon discount bond.
zcb :: Date -> Double -> Currency -> Contract
zcb t x k = scaleK x $ get $ truncate t $ one k

main :: IO ()
main = let 
    t1 = dateFromString "1 01 2010"
    t2 = dateFromString "1 02 2010"
    c1 = zcb t1 100 GBP  -- Receive 100GBP on 1st Jan 2010
    c2 = zcb t2 200 GBP

    c3 = c1 `and` c2
    c4 = c1 `and` give c2
    in
    print $ c4
