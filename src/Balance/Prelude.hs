module Balance.Prelude where


(<$$>) :: Functor f => Functor g => (a -> b) -> f (g a) -> f (g b)
f <$$> x = fmap (fmap f) x
