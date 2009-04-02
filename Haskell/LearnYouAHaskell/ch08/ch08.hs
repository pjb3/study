data Point = Point Float Float deriving (Show)  
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

data Vector a = Vector a a a deriving (Show)  
  
vplus :: (Num t) => Vector t -> Vector t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  
  
vectMult :: (Num t) => Vector t -> t -> Vector t  
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)  
  
scalarMult :: (Num t) => Vector t -> Vector t -> t  
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord) 
  
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  
  
treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right)  
    
treeElem :: (Ord a) => a -> Tree a -> Bool  
treeElem x EmptyTree = False  
treeElem x (Node a left right)  
   | x == a = True  
   | x < a  = treeElem x left  
   | x > a  = treeElem x right    
   
class YesNo a where  
  yesno :: a -> Bool   
    
instance YesNo Int where
  yesno 0 = False
  yesno _ = True
    
instance YesNo [a] where
  yesno [] = False
  yesno _ = True
    
instance YesNo Bool where
  yesno = id
  
instance YesNo (Maybe a) where  
  yesno (Just _) = True  
  yesno Nothing = False 

instance YesNo (Tree a) where  
  yesno EmptyTree = False  
  yesno _ = True   

yesnoIf :: (YesNo y) => y -> a -> a -> a  
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult  