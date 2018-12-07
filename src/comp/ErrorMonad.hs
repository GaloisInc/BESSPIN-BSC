{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module ErrorMonad(ErrorMonad(..), convErrorMonadToIO) where

#if !defined(__GLASGOW_HASKELL__) || (__GLASGOW_HASKELL__ < 710)
import Control.Applicative(Applicative(..))
#endif
import ErrorTCompat
import Control.Monad(ap)
import Error(EMsg, WMsg, ErrMsg(..), ErrorHandle, bsError, bsWarning)
import Position(noPosition)

data ErrorMonad v = EMError [EMsg]
		  | EMWarning [WMsg] v
		  | EMResult v

instance Monad ErrorMonad where
    (EMError es) >>= _     = EMError es  -- XXX could merge errors
    (EMWarning ws v) >>= f = case f v of
                               EMError es       -> EMError es  -- XXX ws
			       EMWarning ws' v' -> EMWarning (ws ++ ws') v'
			       EMResult v'      -> EMWarning ws v'
    (EMResult v) >>= f     = (f v)
    return v               = EMResult v
    fail s                 = EMError [(noPosition, EGeneric s)]

instance Functor ErrorMonad where
    fmap _ (EMError es)     = EMError es
    fmap f (EMWarning ws v) = EMWarning ws (f v)
    fmap f (EMResult v)     = EMResult (f v)

instance Applicative ErrorMonad where
  pure = return
  (<*>) = ap

instance MonadError [EMsg] ErrorMonad where
    throwError es    = EMError es
    m `catchError` h = case m of
                         EMError es -> h es
                         _          -> m

convErrorMonadToIO :: ErrorHandle -> ErrorMonad a -> IO a
convErrorMonadToIO errh r =
    case r of
      EMError emsgs      -> bsError errh emsgs
      EMWarning wmsgs m' -> bsWarning errh wmsgs >> return m'
      EMResult m'        -> return m'
