data Direction
  = Left
  | Right
  | Straight
  deriving Show
               
data Point = Point Int Int deriving Show
pointX (Point x _) = x
pointY (Point _ y) = y

-- turnDirection :: Point -> Point -> Point -> Direction
-- turnDirection a b c = ??? Fuck, this is hard