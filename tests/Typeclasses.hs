module Typeclasses where

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Eq,Show)

instance Functor Tree where
    fmap f (Leaf a)   = Leaf $ f a
    fmap f (Node a b) = Node (fmap f a) (fmap f b)

instance Applicative Tree where
    pure = Leaf
    (Leaf f)   <*> x = _ f x
    (Node f g) <*> x = Node (f <*> x) (g <*> x)








--
--instance Monad Tree where
--    return = pure
--    (Node a b) >>= f = Node (a >>= f) (b >>= f)
--    (Leaf a)   >>= f = f a
--
--
--instance Foldable Tree where
--    foldMap f (Node a b) = foldMap f a <> foldMap f b
--    foldMap f (Leaf a)   = f a
--
--instance Traversable Tree where
--    traverse f (Leaf a) = pure Leaf <*> f a
--    traverse f (Node a b) = liftA2 Node (traverse f a) (traverse f b)