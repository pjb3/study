import Data.List(sort)
import Data.Map((!), intersection, fromList, keys)
import qualified Data.Map as M

type Preferences = M.Map String (M.Map String Double)
type Similarity = Preferences -> String -> String -> Double

critics :: Preferences
critics = fromList [
  ("Lisa Rose", fromList
    [("Lady in the Water", 2.5),
     ("Snakes on a Plane", 3.5),
     ("Just My Luck", 3.0),
     ("Superman Returns", 3.5),
     ("The Night Listener", 3.0),
     ("You, Me and Dupree", 2.5)
    ]),
  ("Gene Seymour", fromList
    [("Lady in the Water", 3.0),
     ("Snakes on a Plane", 3.5),
     ("Just My Luck", 1.5),
     ("Superman Returns", 5.0),
     ("The Night Listener", 3.0),
     ("You, Me and Dupree", 3.5)
    ]),
  ("Michael Phillips", fromList    
    [("Lady in the Water", 2.5),
     ("Snakes on a Plane", 3.0),
     ("Superman Returns", 3.5),
     ("The Night Listener", 4.0)
    ])]

sim_pearson :: Preferences -> String -> String -> Double
sim_pearson prefs p1 p2 =
  let si = keys $ intersection (prefs ! p1) (prefs ! p2) 
      n  = fromIntegral $ length si
  in case n of
      0 -> 0
      otherwise -> 
        let sum1 = sum [prefs ! p1 ! it | it <- si]
            sum2 = sum [prefs ! p2 ! it | it <- si]
            sum1Sq = sum [prefs ! p1 ! it ^ 2 | it <- si]
            sum2Sq = sum [prefs ! p2 ! it ^ 2 | it <- si]
            pSum = sum [(prefs ! p1 ! it) * (prefs ! p2 ! it) | it <- si]
            num = pSum - (sum1 * sum2 / n)
            den = sqrt ((sum1Sq - sum1 ^ 2 / n) *
                        (sum2Sq - sum2 ^ 2 / n))
        in case den of
          0 -> 0
          otherwise -> num / den

top_matches :: Preferences -> String -> Int -> Similarity -> [(Double, String)]
top_matches prefs person n sim =
  take n $ 
    reverse $ 
      sort [(sim prefs person other, other) 
        | other <- (keys prefs), other /= person ]