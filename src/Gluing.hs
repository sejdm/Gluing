module Gluing 
    (
      GlueAt (..)
    , writeChart
    , View (..)
    , changeChart
    , toChart
    , glue
    , range
    , Parametrization
    ) where

import qualified Data.ByteString.Lazy.Char8 as C
import Data.Monoid


class ByteStringAble a where
    toByteString :: a -> C.ByteString

(#*) :: (ByteStringAble a, ByteStringAble b) => a -> b -> C.ByteString
a #* b  = toByteString a <> spc <> toByteString b
    where spc = C.pack " " 

instance ByteStringAble C.ByteString where toByteString = id
instance ByteStringAble Float where toByteString = C.pack . show

instance (ByteStringAble a, ByteStringAble b, ByteStringAble c) => 
        ByteStringAble (a, b, c) where
   toByteString (x, y, z) = x #* y #* z



type Affine = (Float, Float, Float)
type Projective = (Float, Float, Float, Float)
type Parametrization = Float -> Affine



data View = Cube Affine Float | Circle Affine Float | Rectangle Affine Affine
data GlueAt = Glue {knot :: Parametrization, glueAt :: Float, scaleBy :: Float}
data Chart = X | Y | Z | W -- Coordinate charts of the projective space




(#-) :: Affine -> Affine -> Affine
(x1, x2, x3) #- (y1, y2, y3) = ( x1 - y1, x2 - y2, x3 - y3)




stretch :: Chart -> Float -> Affine -> Affine
stretch X t (x, y, z) = (t*x, y, z)
stretch Y t (x, y, z) = (x, t*y, z)
stretch Z t (x, y, z) = (x, y, t*z)




-- Convert from homogeneous projective coordinates to affine
affine :: Chart -> Projective -> Affine
affine W (x0, x1, x2, x3) = (x1/x0, x2/x0, x3/x0)
affine X (x0, x1, x2, x3) = (x0/x1, x2/x1, x3/x1)
affine Y (x0, x1, x2, x3) = (x0/x2, x1/x2, x3/x2)
affine Z (x0, x1, x2, x3) = (x0/x3, x1/x3, x2/x3)



changeChart :: Chart -> Affine -> Affine
changeChart n (x1, x2, x3) = affine n (1, x1, x2, x3)





range :: Float -> Float -> Float -> [Float]
range h a b = [a, (a+h)..b]

shift :: Float -> Parametrization -> Parametrization
shift t' f = \t -> f (t + t')  #- f t'

scale :: Float -> Float -> Float
scale r t = r*t

invert :: Float -> Float
invert t = 1/t





-- The main gluing function to glue two algebraic knots
glue :: GlueAt -> GlueAt -> Parametrization
glue (Glue f t1 r1) (Glue g t2 r2) = preglue (shift t1 f . scale r1) (shift t2 g . invert . scale r2)
    where 
        preglue f' g' = \t -> glueForumula (f' t) (g' t)
        glueForumula (x1, x2, x3) (y1, y2, y3) = (y1+x1, y2+x2, y3+x3)





writeChart :: FilePath -> Float -> View -> Parametrization -> [Float] -> IO()
writeChart n c v f = C.writeFile n . C.unlines . map toByteString . removeIf (tooClose c) . filter (check v) . map f 
    where 
        check (Cube (x, y, z) r) (x', y', z') = abs (x - x') < r && abs (y - y') < r && abs (z - z') < r
        check (Rectangle (x, y, z) (r1, r2, r3)) (x', y', z') = abs (x - x') < r1 && abs (y - y') < r2 && abs (z-z') < r3
        check (Circle (x, y, z) r) (x', y', z') = (x-x')^2 + (y-y')^2 + (z-z')^2  < r^2






tooClose :: Float -> Affine -> Affine -> Bool
tooClose r (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) < r &&  abs (y1 - y2) < r  && abs  (z1 - z2) < r

removeIf :: (a -> a -> Bool) -> [a] -> [a]
removeIf eq ys = remove ys
  where
    remove []  = []
    remove [x] = [x]
    remove (x1:x2:xs)
      | eq x1 x2  = remove (x1:xs)
      | otherwise = x1 : remove (x2:xs)






toChart :: String -> Chart
toChart "W" = W          
toChart "X" = X          
toChart "Y" = Y          
toChart "Z" = Z          
toChart _ = error "Unrecognizable chart"

