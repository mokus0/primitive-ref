module Data.STRef.Primitive
    ( Ref
    , STRef
    , IORef
    , newRef
    , readRef
    , writeRef
    , modifyRef
    ) where

import Control.Applicative
import Control.Monad.Primitive
import qualified Control.Monad.ST as ST
import Data.STRef (STRef)
import qualified Data.STRef       as ST

type Ref m = STRef (PrimState m)
type IORef = Ref IO

newRef :: PrimMonad m => a -> m (Ref m a)
newRef x = primToPrim (ST.newSTRef x)

readRef :: PrimMonad m => Ref m a -> m a
readRef r = primToPrim (ST.readSTRef r)

writeRef :: PrimMonad m => Ref m a -> a -> m ()
writeRef r x = primToPrim (ST.writeSTRef r x)

modifyRef :: PrimMonad m => Ref m a -> (a -> a) -> m ()
modifyRef r f = primToPrim (ST.modifySTRef r f)
