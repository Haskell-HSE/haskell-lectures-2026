







-- composition of side-effects!

example :: Maybe a
example = Nothing

example' :: s -> (Int, s)
example' x = (0, x)

newtype Compose f g a = MkCompose { runCompose :: f (g a) }
    deriving Functor

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure = MkCompose . pure @f . pure @g
    liftA2 h (MkCompose x) (MkCompose y) =
        MkCompose $ liftA2 @f (liftA2 @g h) x y

instance (Monad f, Monad g) => Monad (Compose f g) where
    x >>= k = let MkCompose r = fmap (runCompose . k) x in error "TODO"
