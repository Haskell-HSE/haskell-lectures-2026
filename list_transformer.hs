




{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
import Data.Monoid (Ap (..))

newtype Compose f g a = MkCompose { runCompose :: f (g a) }
    deriving Functor

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure = MkCompose . pure @f . pure @g
    liftA2 h (MkCompose x) (MkCompose y) =
        MkCompose $ liftA2 @f (liftA2 @g h) x y

newtype ListT0 m a = MkListT0 { runListT0 :: m [a] }
    deriving Functor
    deriving Applicative via (Compose m [])

instance Monad m => Monad (ListT0 m) where
    MkListT0 x >>= k = MkListT0 do
        xs <- x
        yss <- traverse (runListT0 . k) xs
        pure (concat yss)

newtype ListT1 m a = MkListT1 { runListT1 :: [m a] }
    deriving Functor
    deriving Applicative via (Compose [] m)

instance Monad m => Monad (ListT1 m) where
    MkListT1 x >>= k = MkListT1 [ error "INCORRECT" | mx <- x, let my = fmap (runListT1 . k) mx ]

newtype ListT2 m a = MkListT2 { runListT2 :: m [m a] }
    deriving Functor
    deriving Applicative via (Compose (Compose m []) m)

instance Monad m => Monad (ListT2 m) where
    MkListT2 x >>= k = MkListT2 do
        xs <- x
        yss <- traverse (>>= runListT2 . k) xs
        pure (concat yss)

newtype ListT m a = MkListT { runListT :: m (ListTData m a) }
    deriving Functor
    deriving Applicative via (Compose m (ListTData m))
    deriving Semigroup via (Ap m (ListTData m a))

data ListTData m a = Nil | Cons a (ListT m a)
    deriving Functor

instance Applicative m => Semigroup (ListTData m a) where
    Nil <> xs = xs
    Cons x xs <> ys = Cons x (xs <> MkListT (pure ys))

instance Applicative m => Applicative (ListTData m) where
    pure = (`Cons` MkListT (pure Nil))
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    Cons f fs <*> Cons x xs = Cons (f x) $ fmap f xs <> (fs <*> xs)

instance Monad m => Monad (ListT m) where
    MkListT x >>= k = MkListT $ x >>= \case
        Nil -> pure Nil
        Cons x xs -> runListT (k x <> (xs >>= k))
