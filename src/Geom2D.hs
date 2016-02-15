module Geom2D where
  type StandardReal = Double

  data Matrix = Matrix

  data Point = Point StandardReal StandardReal

  type Axis = Point

  data Transform = Transform StandardReal Matrix Point

  class Transformable a where
    transform :: a -> Transform -> a

  class Curve a where
    value :: a -> StandardReal -> Point

  data Conic = Circle Point StandardReal
             | Ellipse Point Point StandardReal
             | Hyperbola
             | Parabola

  data BoundedCurve = BezierCurve
                    | BSplineCurve
                    | TrimmedCurve


  --data Curve = Line Point Point
    --         | Conic
      --       | BoundedCurve
        --     | OffsetCurve

  data Vector = Direction
              | VectorWithMagnitude

  data Geometry = Geometry Transformable
