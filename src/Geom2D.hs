module Geom2D where
  type StandardReal = Double
  type Vector = Point
  type Axis = Point

  data Matrix = Matrix [[StandardReal]]
  identity = Matrix [[1, 0, 0], [0, 1, 0], [0, 0, 1]]
  at :: Matrix -> Int -> Int -> StandardReal
  at (Matrix mat) x y = (mat !! y) !! x

  createTranslation :: Point -> Transform
  createTranslation = Transform Translation 1.0 identity

  data TransformType = Identity | Translation | Scale | Mirror
  data Transform = Transform TransformType StandardReal Matrix Point
  matrix (Transform _ _ m _) = m

  class Transformable a where
    transform :: a -> Transform -> a
  class Curve a where
    value :: a -> StandardReal -> Point
    dN :: a -> StandardReal -> StandardReal -> (Point, [Point])

  data Conic = Circle Point StandardReal
             | Ellipse Point Point StandardReal
             | Hyperbola
             | Parabola

  data BoundedCurve = BezierCurve
                    | BSplineCurve
                    | TrimmedCurve

  data Point = Point StandardReal StandardReal
    deriving(Show)
  x (Point x _) = x
  y (Point y _) = y
  instance Transformable Point where
    transform p (Transform Identity _ _ _) = p
    transform p (Transform Translation _ _ l) = Point (x p + x l) (y p + y l)
    transform p (Transform _ _ mat _) = Point newX newY where
      oldX = x p
      oldY = y p
      newX = at mat 0 0 * oldX + at mat 0 1 * oldY
      newY = at mat 1 0 * oldX + at mat 1 1 * oldY

  data Line = Line Point Point
  lineDir (Line _ dir) = dir
  instance Curve Line where
    value (Line (Point posX posY) (Point dirX dirY)) u = Point (u * dirX + posX) (u * dirY + posY)
    dN l u 1 = (value l u, [lineDir l])
    dN l u n = (value l u, Point 0 0 : snd (dN l u (n - 1)))
  instance Transformable Line where
    transform (Line pos dir) t = Line (transform pos t) dir
