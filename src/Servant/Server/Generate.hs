{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Servant.Server.Generate
  ( -- * Using this library
    -- $using

    -- * The 'GenerateServer' class
    GenerateServer(..)
  , -- * Utilities
    unconstrained, constrained
  , handlers
  , Flatten, NonEmpty
  ) where

import Data.Proxy
import GHC.Exts
import GHC.TypeLits
import Servant.API
import Servant.API.Internal.Test.ComprehensiveAPI
import Servant.Server

-- | A class that'll define the generated server using the supplied
--   constraints and some function that takes a response type (through a 'Proxy')
--   and returns a computation in an arbitrary monad that returns a value of
--   the aforementionned response type.
--
--   What does this all mean?
--
--   Well, for example, we can use an empty list of
--   constraints (see 'unconstrained') and just systematically call 'throwError'
--   in all endpoints. The monad in that case would be 'Handler', and the result of
--   calling 'generateServer' on any API type would be a server implementation for that
--   API that just errors out in each handler.
--
--   Another example would be to use a constraint list of just one class (with
--   'constrained'), say `Arbitrary`, and pass a function to `generateServer` that
--   simply generates a random value in IO (something like
--   @\_ -> liftIO $ generate (arbitrary :: Gen a)@). The result of calling this on a
--   given API type would be a server implementation that can live in any `MonadIO m`,
--   where for each endpoint we simply generate a random response (of the right type!).
class GenerateServer (api :: *) (constraints :: [* -> Constraint]) where
  -- | Given:
  --
  --     * a particular API type @api@,
  --
  --     * some constraint(s) that all response types in the API must satisfy,
  --
  --     * a \"monad-like\" type constructor @m@, of kind @* -> *@,
  --
  --     * a function that, when given (a 'Proxy' to) any response type @a@ that
  --       implements the aforementionned constraints, can produce a value of
  --       that type in @m@,
  --
  --   'generateServer' gives you back an implementation of the given API
  --   in with handlers returning responses in @m@, that is, a value of type
  --   @'ServerT' api m@.
  --
  --   When @m@ is just 'Handler', you can directly 'serve' that implementation.
  --   If you're working with another monad, you need to use 'hoistServer' on the
  --   result of 'generateServer' before you can 'serve' it.
  generateServer :: forall (m :: * -> *).
                    Proxy api
                 -> Proxy constraints
                 -> Proxy m
                 -> (forall a. Flatten constraints a => Proxy a -> m a)
                 -> ServerT api m

instance (GenerateServer a c, GenerateServer b c) => GenerateServer (a :<|> b) c where
  generateServer _ c m f = generateServer (Proxy @ a) c m f
                      :<|> generateServer (Proxy @ b) c m f

instance (KnownSymbol path, GenerateServer api c) => GenerateServer (path :> api) c where
  generateServer _ c m f = generateServer (Proxy @ api) c m f

instance GenerateServer api c => GenerateServer (Summary s :> api) c where
  generateServer _ c m f = generateServer (Proxy @ api) c m f

instance GenerateServer api c => GenerateServer (Description s :> api) c where
  generateServer _ c m f = generateServer (Proxy @ api) c m f

instance GenerateServer EmptyAPI c where
  generateServer _ _ _ _ = emptyServer

instance (KnownSymbol s, GenerateServer api c) => GenerateServer (Capture' mods s a :> api) c where
  generateServer _ c m f = \_ -> generateServer (Proxy @ api) c m f

instance (KnownSymbol s, GenerateServer api c) => GenerateServer (CaptureAll s a :> api) c where
  generateServer _ c m f = \_ -> generateServer (Proxy @ api) c m f

instance GenerateServer api c => GenerateServer (ReqBody' mods cts a :> api) c where
  generateServer _ c m f = \_ -> generateServer (Proxy @ api) c m f

instance GenerateServer api c => GenerateServer (RemoteHost :> api) c where
  generateServer _ c m f = \_ -> generateServer (Proxy @ api) c m f

instance GenerateServer api c => GenerateServer (IsSecure :> api) c where
  generateServer _ c m f = \_ -> generateServer (Proxy @ api) c m f

instance GenerateServer api c => GenerateServer (Vault :> api) c where
  generateServer _ c m f = \_ -> generateServer (Proxy @ api) c m f

instance GenerateServer api c => GenerateServer (HttpVersion :> api) c where
  generateServer _ c m f = \_ -> generateServer (Proxy @ api) c m f

instance (KnownSymbol s, GenerateServer api c) => GenerateServer (QueryParam' mods s a :> api) c where
  generateServer _ c m f = \_ -> generateServer (Proxy @ api) c m f

instance (KnownSymbol s, GenerateServer api c) => GenerateServer (QueryFlag s :> api) c where
  generateServer _ c m f = \_ -> generateServer (Proxy @ api) c m f

instance (KnownSymbol s, GenerateServer api c) => GenerateServer (QueryParams s a :> api) c where
  generateServer _ c m f = \_ -> generateServer (Proxy @ api) c m f

instance (KnownSymbol h, GenerateServer api c) => GenerateServer (Header' mods h a :> api) c where
  generateServer _ c m f = \_ -> generateServer (Proxy @ api) c m f

instance GenerateServer api c => GenerateServer (WithNamedContext name subContext api) c where
  generateServer _ c m f = generateServer (Proxy @ api) c m f

-- | Requires all constraints in @cs@ to be satisfied by @a@.
instance (KnownNat status, Flatten cs a) => GenerateServer (Verb (method :: StdMethod) (status :: Nat) (cts :: [*]) (a :: *)) cs where
  generateServer _ _ _ f = f (Proxy @ a)

type family Flatten (cs :: [* -> Constraint]) (a :: *) :: Constraint where
  Flatten '[] a = ()
  Flatten (c ': cs) a = (c a, Flatten cs a)

-- * Utilities for specifying the arguments to 'generateServer'

-- | Meant to be used as the second argument to 'generateServer' when your
--   implementation function doesn't need any typeclass constraint and can
--   just work with any response type.
unconstrained :: Proxy ('[] :: [* -> Constraint])
unconstrained = Proxy

type family NonEmpty (cs :: [* -> Constraint]) :: Constraint where
  NonEmpty '[] = TypeError ('Text "empty list of constraints used with 'constrained'")
  NonEmpty cs = ()

-- | Meant to be used as the second argument to 'generateServer' when your
--   implementation function requires one or more constraints on the /all/
--   the response types of your API, e.g @'constrained' \@ '[Show, Random, Monoid]@.
constrained
  :: forall (cs :: [* -> Constraint]). NonEmpty cs => Proxy cs
constrained = Proxy

-- | Meant to be used as the third argument to 'generateServer'. It's a simple
--   more readable wrapper around 'Proxy'. Use a type application to specify
--   the monad, e.g @'handlers' \@ 'AppMonad'@.
handlers :: forall (m :: * -> *). Proxy m
handlers = Proxy

-- do we have all instances?

class A a where
  getA :: Proxy a -> a

instance A NoContent where
  getA _ = NoContent

instance A Int where
  getA _ = 0

instance A a => A (Headers '[] a) where
  getA _ = Headers (getA (Proxy :: Proxy a)) HNil

instance (A t, A (Headers hs a))
      => A (Headers (Header h t ': hs) a) where
  getA _ = case getA (Proxy @ (Headers hs a)) of
    Headers a hs -> Headers a $
      (Header $ getA (Proxy @ t)) `HCons` hs

s :: Server ComprehensiveAPIWithoutRaw
s = generateServer
  (Proxy @ ComprehensiveAPIWithoutRaw)
  (constrained @ '[A])
  (handlers @ Handler)
  (return . getA)

-- $using
-- Let's imagine we're working with the following
-- simple API and we would like to spin up a "silly"
-- server implementation that returns responses of the
-- right type (or errors out) for each endpoint.
--
-- > type API = Get '[JSON] ()
-- >       :<|> "int" :> Get '[JSON] Int
-- >
-- > api :: Proxy API
-- > api = Proxy
--
-- First, let's see how we can get a server implementation
-- where all the handlers throw a 404 error.
--
-- > -- x :: Handler () :<|> Handler Int
-- > -- inferred just fine
-- > x = generateServer api unconstrained (handlers @ Handler) (\_ -> throwError err404)
--
-- We could more generally take the monad as argument, simply
-- requiring that it is a @'MonadError' 'ServantErr'@, for
-- example.
--
-- > -- x' :: MonadError ServantErr m => Proxy m -> m () :<|> m Int
-- > -- inferred just fine
-- > x' mon = generateServer api unconstrained mon (\_ -> throwError err404)
--
-- Now, let's see an example where we need one constraint to be
-- satisfied for all response types present in the API.
--
-- > -- we will require all response types to provide
-- > -- an instance of this typeclass.
-- > class Default a where
-- >   def :: Proxy a -> a
-- >
-- > instance Default () where
-- >   def _ = ()
-- >
-- > instance Default Int where
-- >   def _ = 0
-- >
-- > -- y :: forall m. Applicative m => Proxy m -> m () :<|> m Int
-- > -- inferred just fine
-- > y mon = generateServer api (constrained @ '[Default]) mon $ \pa -> pure (def pa)
--
-- Finally, let's see an example where we require all response
-- types of an API to have instances for several type classes.
--
-- > -- we will require instances of Tweak in addition to
-- > -- Default.
-- > class Tweak a where
-- >   tweak :: a -> a
-- >
-- > instance Tweak () where
-- >   tweak () = ()
-- >
-- > instance Tweak Int where
-- >   tweak n = n^2
-- >
-- > -- z :: forall m. Monad m => Proxy m -> m () :<|> m Int
-- > z mon = generateServer api (constrained @ '[Default, Tweak]) mon $ \pa ->
-- >   return $ tweak (def pa)
-- >
-- > -- instantiated at a particular monad, say Handler:
-- > z' = z (handlers @ Handler)
