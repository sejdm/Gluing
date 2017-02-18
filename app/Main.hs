import Gluing


circle2 = inclineXZ 0.005 . translateY (-1) . circle 1
circle3 = inclineXZ (-0.005) . reflectO . translateY (-1) . circle 1
circle4 = translateY (-1) . circle 2

circ = inclineXZ 0.01 . circle 1
circ' = circle 2

--rotateZ :: Ratio Integer -> Affine -> Affine
--rotateZ r (x, y, z) = (cos r * x + sin r * y,  cos r * y - sin r * x, z) 

trefoil :: Parametrization
trefoil t = (t^3 - 3*t, t^4 - 4*t^2, t^5 - 10*t)

trefoil3 :: Parametrization
trefoil3 t = (t^5 - 10*t, t^3 - 3*t, t^4 - 4*t^2)

line :: Parametrization
line t = (0.01*t, t, 0)

line2 :: Parametrization
line2 t = (t, 0, 0)




twist3 = glue (Glue (twist 1) (1/20) (0.1)) (Glue (inclineYZ 0.2 . circle 1 . backward) (backward $ -1) 0.1)  
--twist = glue (Glue circle3 0 30) (Glue circle2 0 30)
notwist = glue (Glue (circle3 . backward) 0 1) (Glue circle2 0 100)
notwist2 = glue (Glue (circle3) 0 1) (Glue (circle2 . backward) 0 100)
trefoilAttempt = glue (Glue (twist 1) (-1) 0.1) (Glue (circle 1.5) 1 0.1)
nofigure8  = glue (Glue (twist 1) 3 1) (Glue (circle 1.5 . backward) 0 100)

threeCircles = glue (Glue (twist 1) 3 1) (Glue (reflectO . inclineXZ (-0.01) . circle 1) 0 100)
figure8attempt = glue (Glue (translateY (-2) . threeCircles) 0.1 1) (Glue (translateY 1.5 . inclineXZ (-1) . translateX (-1.5) . permute X Y . circle 1.5) 1.5 1000)


inneruntwist = glue (Glue circle4 0 1) (Glue circle2 0 1000)
innertwist = glue (Glue (circle4 . backward) 0 1) (Glue circle2 0 1000)


figure8  = glue (Glue (twist 2) (-1/20) (0.1)) (Glue (circle 2.5) (1) 0.1)  
op = [
     ("trefoil", twistKnot 1),
     ("twist1", twist 1),
     ("twist2", twist 2),
     ("twist3", twist 3),
     ("twist4", twist 4),
     ("figure8", twistKnot 2),
     ("notwist", notwist),
     ("notwist2", notwist2),
     ("threecircles", threeCircles),
     ("innertwist", innertwist),
     ("inneruntwist", inneruntwist),
     ("extra", twistKnot 3),
     ("nofigure8", nofigure8)
     ]
op' = [
      ("twist", twist' 0.1 0.1),
      ("twistknot", twistKnot' 0.5)
      ]
main = useList def {closeness = 0.001, stepSize = 0.001, initial = -10000, final = 10000}  op op'
