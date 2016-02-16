module Geom2D where
  import Data.Matrix
  import qualified Data.Vector as V

  type StandardReal = Double
  type Vector = Point

  data Point3D = Point3D StandardReal StandardReal StandardReal
    deriving(Show)
  data Dir3D = Dir3D StandardReal StandardReal StandardReal
    deriving(Show)
  data Axis3D = Axis3D Point3D Dir3D
    deriving(Show)

  data Point = Point StandardReal StandardReal
    deriving(Show)
  data Dir = Dir StandardReal StandardReal
    deriving(Show)
  data Axis = Axis Point Dir
    deriving(Show)

  createTranslation :: Point -> Matrix StandardReal
  createTranslation (Point pX pY) = fromLists [[1, 0, pX], [0, 1, pY], [0, 0, 1]]

  createScale :: StandardReal -> Matrix StandardReal
  createScale s = fromLists [[s, 0, 0], [0, s, 0], [0, 0, s]]

  class Transformable a where
    transform :: a -> Matrix StandardReal -> a
  class Curve a where
    value :: a -> StandardReal -> Point
    dN :: a -> StandardReal -> StandardReal -> (Point, [Dir])

  x (Point x _) = x
  y (Point y _) = y

  instance Transformable Point where
    transform (Point pX pY) mat = Point (head vals) (vals !! 1) where
      pointMat = fromLists [[pX], [pY], [1]]
      vals = Data.Matrix.toList (mat * pointMat)

  data Line = Line Point Dir

  instance Curve Line where
    value (Line (Point posX posY) (Dir dirX dirY)) u = Point (u * dirX + posX) (u * dirY + posY)
    dN (Line p d) u 1 = (value (Line p d) u, [d])
    dN l u n = (value l u, Dir 0 0 : snd (dN l u (n - 1)))
  instance Transformable Line where
    transform (Line pos dir) t = Line (transform pos t) dir

  data Circle = Circle Axis StandardReal
   deriving(Show)
  instance Curve Circle where
    value (Circle (Axis p d) r) u = Point pX pY where
      cX = x p
      cY = y p
      a1 = r * cos u
      a2 = r * sin u
      pX = a1 * cX + a2 * cY
      pY = 0

  transformSR :: StandardReal -> Matrix StandardReal -> StandardReal
  transformSR s m = head (toList (multStd (fromList 1 3 [s, s, s]) m))

  instance Transformable Circle where
    transform (Circle (Axis p d) r) t = Circle (Axis (transform p t) d) (transformSR r t)


  t = createTranslation (Point 1 1)
  p = Point 0 0
  c = Circle (Axis (Point 5 2) (Dir 1 0)) 2
  s = createScale 5
