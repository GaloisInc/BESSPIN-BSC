module SMonad where

id_SMonad = " $Id: SMonad.hs,v 1.1 2001/01/16 15:29:14 augustss Exp $"

newtype SM s a = M (s -> (s, a))

instance Monad (SM s) where
    return a = M $ \ s -> (s, a)
    M a >>= f = M $ \ s ->
        case a s of
	(s', b) ->
	    let M f' = f b
	    in  f' s'

run :: s -> SM s a -> (s, a)
run s (M m) = m s

getState :: SM state_t state_t
getState = M (\state -> (state, state))

setState :: state_t -> SM state_t ()
setState new_state = M (\_ -> (new_state, ()))

