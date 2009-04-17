import Data.Map((!), intersection, fromList, keys)
import qualified Data.Map as M

type Preferences = M.Map String (M.Map String Double)

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

sim_distance :: Preferences -> String -> String -> Double
sim_distance prefs p1 p2 =
  let common_movies = keys $ intersection (prefs ! p1) (prefs ! p2) 
  in case common_movies of
      [] -> 0
      otherwise -> 
        1 / (1 + sqrt sum_of_squares)
        where sum_of_squares = 
                sum [(((prefs ! p1) ! movie) - 
                      ((prefs ! p2) ! movie)) ^ 2 
                     | movie <- common_movies]
