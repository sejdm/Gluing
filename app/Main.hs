module Main (main) where

import Gluing
import System.Environment


-- Examples of parametrizations
trefoil :: Parametrization
trefoil t = (t^3 - 3*t, t^4 - 4*t^2, t^5 - 10*t)

trefoil3 :: Parametrization
trefoil3 t = (t^5 - 10*t, t^3 - 3*t, t^4 - 4*t^2)

line :: Parametrization
line t = (0.01*t, t, 0)

line2 :: Parametrization
line2 t = (t, 0, 0)

--main = undefined
          


main :: IO ()
main = do [n, s, t] <- getArgs

          let standardRange = range 0.00001 (-90) 90

          let f = Glue {knot = trefoil3, glueAt = 4, scaleBy = read s}
          let g = Glue {knot = line2, glueAt = 4, scaleBy = read t}
          
          let file = "/home/shane/data3.dat" 
          
          let view  = Cube (0, 0.25, 0.05) 200
          
          let closeness = 0.0001

          writeChart file closeness view (changeChart (toChart n) . glue f g) standardRange
          
