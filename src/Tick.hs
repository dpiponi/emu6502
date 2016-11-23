{-# LANGUAGE BangPatterns #-}

import Control.Monad.Trans
import Control.Monad.State

data Action m = Atom Int (m (Action m)) | Stop

atom :: Monad m => Int -> m a -> C m a
atom t m = C $ \c -> Atom t (m >>= (return . c))

stop :: Monad m => C m a -> Action m
stop = \c -> Stop

action :: Monad m => C m a -> Action m
action (C m) = m (\a -> Stop)

step :: Monad m => Int -> Action m -> m (Maybe (Action m))
step 0 m = return $ Just m
step n (Atom t a) | n < t = return $ Just $ Atom (t-n) a
step n (Atom t a) = a >>= step (n-t)
step n Stop = return Nothing

instance MonadTrans C where
    lift a = atom 0 a

tick :: Monad m => C m ()
tick = atom 1 (return ())

newtype C m a = C { unC :: ((a -> Action m) -> Action m)}

instance Monad m => Monad (C m) where
    C f >>= k = C $ \c -> f (\a -> unC (k a) c)
    return x = C $ \c -> c x

test :: C (StateT Int IO) ()
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
