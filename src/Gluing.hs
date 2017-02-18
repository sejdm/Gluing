{-# LANGUAGE FlexibleInstances, UnboxedTuples #-}
module Gluing (
               glue,
               Parametrization, Affine, Projective,
               View (..),
               GlueAt (..),
               Chart (..),
               Settings (..), def,
               writeChart, useList,
               stretch, affine, changeChart, shift, scale, invert, backward, toChart, permute,
               translate, translateOnly, translateX, translateY, translateZ, reflectO, reflect, inclineXZ, incline, inclineYZ,
               circle,
               opts,
               range, 
               twist, twistKnot, twist', twistKnot'
               ) where 

import System.Environment
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Monoid
import Control.Parallel.Strategies
import Data.List
import Data.Ratio
import Data.Map (fromList, findWithDefault)
import Data.Char



class ByteStringAble a where
    toByteString :: a -> C.ByteString




( #* ) :: (ByteStringAble a, ByteStringAble b) => a -> b -> C.ByteString
a #* b  = toByteString a <> spc <> toByteString b
    where spc = C.pack " " 

instance ByteStringAble C.ByteString where toByteString = id
instance ByteStringAble Float where toByteString = C.pack . show
instance ByteStringAble Double where toByteString = C.pack . show
instance ByteStringAble (Ratio Integer) where toByteString = C.pack . show . toFloat

toFloat :: Ratio Integer -> Float
toFloat = fromRational

instance (ByteStringAble a, ByteStringAble b, ByteStringAble c) => 
        ByteStringAble (a, b, c) where
   toByteString (x, y, z) = x #* y #* z

type RealNumber = Float
--toFloat (x, y, z) = (fromRational x, fromRational y, fromRational z)

type Affine = (RealNumber, RealNumber, RealNumber)
type Projective = (RealNumber, RealNumber, RealNumber, RealNumber)
type Parametrization = RealNumber -> Affine
type AffineTransformation = Affine -> Affine



data View = Cube Affine RealNumber | Sphere Affine RealNumber | Cuboid Affine Affine
data GlueAt = Glue {knot :: Parametrization, glueAt :: RealNumber, scaleBy :: RealNumber}
data Chart = X | Y | Z | W deriving (Eq) -- Coordinate charts of the projective space


( #- ) :: Affine -> Affine -> Affine
(x1, x2, x3) #- (y1, y2, y3) = ( x1 - y1, x2 - y2, x3 - y3)




stretch :: Chart -> Float -> AffineTransformation
stretch X t (x, y, z) = (t*x, y, z)
stretch Y t (x, y, z) = (x, t*y, z)
stretch Z t (x, y, z) = (x, y, t*z)




-- Convert from homogeneous projective coordinates to affine
affine :: Chart -> Projective -> Affine
affine W (x0, x1, x2, x3) = (x1/x0, x2/x0, x3/x0)
affine X (x0, x1, x2, x3) = (x0/x1, x2/x1, x3/x1)
affine Y (x0, x1, x2, x3) = (x0/x2, x1/x2, x3/x2)
affine Z (x0, x1, x2, x3) = (x0/x3, x1/x3, x2/x3)


toChart :: String -> Chart
toChart "W" = W          
toChart "X" = X          
toChart "Y" = Y          
toChart "Z" = Z          
toChart _ = error "Unrecognizable chart"

permute :: Chart -> Chart -> AffineTransformation
permute X Y (x, y, z) = (y , x, z)
permute X Z (x, y, z) = (z, y, x)
permute Y Z (x, y, z) = (x, z, y)
permute a b v | a == b = v
              | otherwise = permute b a v

changeChart :: Chart -> AffineTransformation
changeChart n (x1, x2, x3) = affine n (1, x1, x2, x3)





range :: RealNumber -> RealNumber -> RealNumber -> [RealNumber]
range h a b = [a, (a+h)..b]

shift :: Num a => a -> (a -> Affine) -> (a -> Affine)
shift t' f = \t -> f (t + t')  #- f t'

scale :: RealNumber -> RealNumber -> RealNumber
scale r t = r*t

invert :: RealNumber -> RealNumber
invert t = 1/t





-- The main gluing function to glue two algebraic knots
glue :: GlueAt -> GlueAt -> Parametrization
glue (Glue f t1 r1) (Glue g t2 r2) = preglue (shift t1 f . scale r1) (shift t2 g . scale r2 . invert)
    where
        preglue f' g' = \t -> glueForumula (f' t) (g' t)
        glueForumula (x1, x2, x3) (y1, y2, y3) = (y1+x1, y2+x2, y3+x3)


data Settings = Settings {file :: FilePath, closeness :: RealNumber, view :: View, stepSize :: RealNumber, initial :: RealNumber, final :: RealNumber}

writeChart :: Settings -> Parametrization -> IO ()
writeChart (Settings n c v h a b) f = (C.writeFile n . C.unlines . parBS . map toByteString . removeIf (tooClose c) . filter (check v) . map f) (range h a b)
    where 
        check (Cube (x, y, z) r) (x', y', z') = abs (x - x') < r && abs (y - y') < r && abs (z - z') < r
        check (Cuboid (x, y, z) (r1, r2, r3)) (x', y', z') = abs (x - x') < r1 && abs (y - y') < r2 && abs (z-z') < r3
        check (Sphere (x, y, z) r) (x', y', z') = (x-x')^2 + (y-y')^2 + (z-z')^2  < r^2


parBS :: [C.ByteString] -> [C.ByteString]
parBS = withStrategy (parBuffer (10^8) rdeepseq)


tooClose :: Float ->  Affine -> Affine -> Bool
tooClose r (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) < r &&  abs (y1 - y2) < r  && abs  (z1 - z2) < r

removeIf :: (a -> a -> Bool) -> [a] -> [a]
removeIf eq ys = remove ys
  where
    remove []  = []
    remove [x] = [x]
    remove (x1:x2:xs)
      | eq x1 x2  = remove (x1:xs)
      | otherwise = x1 : remove (x2:xs)

backward :: RealNumber -> RealNumber
backward t = -t

translate :: Affine -> AffineTransformation
translate (x, y, z) (x', y', z') = (x + x', y + y', z + z')

translateOnly :: Chart -> RealNumber -> AffineTransformation
translateOnly c r = onlyChart (r+) c

translateX :: RealNumber -> AffineTransformation
translateX x = translate (x, 0, 0)

translateY :: RealNumber -> AffineTransformation
translateY x = translate (0, x, 0)

translateZ :: RealNumber -> AffineTransformation
translateZ x = translate (0, 0, x)

reflectO :: AffineTransformation
reflectO (x, y, z) = (-x, -y, -z)

inclineXZ :: RealNumber -> Affine -> Affine
inclineXZ s (x, y, 0) = (x, y, s*x)

inclineYZ :: RealNumber -> Affine -> Affine
inclineYZ s (x, y, 0) = (x, y, s*y)

incline :: Chart -> RealNumber -> Affine -> Affine
incline c s = onlyChart (s*) c

reflect :: Chart -> AffineTransformation
reflect x = onlyChart negate x

-- Examples of parametrizations

printNumbered :: [String] -> IO ()
printNumbered = mapM_ putStrLn . zipWith (\x y -> show x ++ ". " ++  y) [1..]

opts :: [String] -> String -> IO String
opts xs x | all isDigit x = let a =  (xs !! (read x - 1)) in putStrLn a >> return a
          | otherwise = do let ys = filter (isInfixOf x) xs
                           case ys of 
                                [] -> error "Nothing"
                                [a] -> if isInfixOf x a then putStrLn a >> return a else error "nothing"
                                _ -> printNumbered ys >> getLine >>= opts ys

useList :: Settings -> [(String, Parametrization)] -> [(String, Int -> Parametrization)] -> IO ()
useList s op op' = do z:zs <- getArgs
                      case zs of 
                                 [] -> do z' <-  opts (map fst op) z
                                          writeChart s (findWithDefault (circle 1) z' (fromList op))
                                 [x] -> do z' <-  opts (map fst op') z
                                           writeChart s ((findWithDefault twist z' (fromList op') (read x)))

def :: Settings
def = Settings {
               file = "/home/shane/data3.dat", 
               closeness = 0.0001, 
               view = Cube (0, 0.25, 0.05) 20, 
               stepSize = 0.00001,
               initial = -200,
               final = 200
               }


onlyChart :: (RealNumber -> RealNumber) -> Chart -> AffineTransformation
onlyChart f X (x, y, z) = (f x, y, z)
onlyChart f Y (x, y, z) = (x, f y, z)
onlyChart f Z (x, y, z) = (x, y, f z)

--
-- The parametrizations
circle :: RealNumber -> RealNumber -> Affine
circle r t = (r*2*t/(1+t^2), r*(1-t^2)/(1+t^2), 0)

twist :: Int -> Parametrization
twist m = foldl twistF twist1 [1..(m-1)]
    where 
          twistF p n = glue (Glue p (back (n+1) $ 1/20) (0.1)) (Glue (inclineYZ ((slopeDivide m) * fromIntegral (n+1)) . circle 1 . back n) (back n $ -1) 0.1)  
          twist1 = glue (Glue (circle 1) 1 0.1) (Glue (inclineYZ (slopeDivide m) . circle 1) (-1) 0.1)

twist' :: RealNumber -> RealNumber ->  Int -> Parametrization
twist' r1 r2  m = foldl twistF twist1 [1..(m-1)]
    where twist1 = glue (Glue (circle 1) 1 r1) (Glue (inclineYZ (slopeDivide m) . circle 1) (-1) r2)
          twistF p n = glue (Glue p (back (n+1) $ 0.5 * r1) (0.1)) (Glue (inclineYZ ((slopeDivide m) * fromIntegral (n+1)) . circle 1 . back n) (back n $ -1) 0.1)  


back :: Int -> RealNumber -> RealNumber
back n t = (-1)^n * t

twistKnot :: Int -> Parametrization
twistKnot n  = glue (Glue (twist n) (back (n+1) $ 1/20) (0.1)) (Glue (inclineYZ (1) . circle (fromIntegral n + 0.5)) (1) 0.1)  

twistKnot' :: RealNumber -> Int -> Parametrization
twistKnot' r n  = glue (Glue (twist' r r n) (back (n+1) $ 0.5 * r) (0.1)) (Glue (inclineYZ (1) . circle (fromIntegral n + 0.5)) (1) 0.1)  


slopeDivide :: Int -> RealNumber
slopeDivide m = - tan (1.5 * pi/(4 * fromIntegral m))
