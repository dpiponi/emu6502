{-# LANGUAGE BangPatterns #-}

import Control.Monad.Trans
import Control.Monad.State

data Action m = Atom Int (m (Action m)) | Stop

atom :: Monad m => Int -> m a -> Step m a
atom t m = Step $ \c -> Atom t (m >>= (return . c))

stop :: Monad m => Step m a -> Action m
stop = \c -> Stop

action :: Monad m => Step m a -> Action m
action (Step m) = m (\a -> Stop)

step :: Monad m => Int -> Action m -> m (Maybe (Action m))
step 0 m = return $ Just m
step n (Atom t a) | n < t = return $ Just $ Atom (t-n) a
step n (Atom t a) = a >>= step (n-t)
step n Stop = return Nothing

instance MonadTrans Step where
    lift a = atom 0 a

tick :: Monad m => Step m ()
tick = atom 1 (return ())

newtype Step m a = Step { unC :: ((a -> Action m) -> Action m)}

instance Monad m => Monad (Step m) where
    Step f >>= k = Step $ \c -> f (\a -> unC (k a) c)
    return x = Step $ \c -> c x

test :: Step (StateT Int IO) ()
test = do
    lift $ do
        !a <- get
        put (a+1)
    tick
    tick
    test

main = do
    let u0 = action test
    (u,v) <- runStateT (step 10 u0) 0
    print v
    case u of
        Nothing -> print "End"
        Just u' -> do
                    (u'',v'') <- runStateT (step 10000000 u') 0
                    print v''
