data List a = Cons a (List a)
            | Nil
              deriving (Show)

toList :: List a -> [a]
toList Nil = []
toList (Cons h t) = h : toList t