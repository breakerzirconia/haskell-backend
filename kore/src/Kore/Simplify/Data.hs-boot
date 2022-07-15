module Kore.Simplify.Data (Env, Simplifier) where

import Prelude.Kore
import Control.Monad.Catch (
    MonadCatch,
    MonadMask,
    MonadThrow,
 )

import Data.Kind (Type)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Kore.Simplify.Simplify (SimplifierCache, MonadSimplify)
import SMT (MSMT, MonadSMT)

data Env (simplifier :: Type -> Type)

newtype Simplifier a = Simplifier
    { runSimplifier' :: StateT SimplifierCache (ReaderT (Env Simplifier) MSMT) a
    }

instance Functor Simplifier
instance Applicative Simplifier
instance Monad Simplifier
instance MonadSMT Simplifier
instance MonadIO Simplifier
instance MonadCatch Simplifier
instance MonadThrow Simplifier
instance MonadMask Simplifier
instance MonadReader (Env Simplifier) Simplifier
instance MonadState SimplifierCache Simplifier
instance MonadSimplify Simplifier
