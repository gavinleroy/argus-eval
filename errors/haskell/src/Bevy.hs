{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE IncoherentInstances #-}

module Bevy ( App
            , Res
            , Resource
            , IntoSystem
            , System
            , newApp
            , injectResource
            , addSystem
            , runApp
            ) where

newtype Res a = Res a

data JustSystem
data IsFunctionSystem

class Resource a
class SystemParam a
class SystemParamFunction a
class System a

-- NOTE: modeling the `Marker` as a closed type family
-- makes it easier for GHC to produce better diagnostics.
type family Marker a where
  Marker (a -> b) = IsFunctionSystem
  Marker a = JustSystem

class IntoSystem a
instance (Marker a ~ marker, IntoSystem' a marker) => IntoSystem a

class IntoSystem' a marker where {}
instance (System a) => IntoSystem' a JustSystem
instance (SystemParamFunction a) => IntoSystem' a IsFunctionSystem

instance (SystemParam a) =>
  SystemParamFunction (a -> out)
 
instance (SystemParam a, SystemParam b) =>
  SystemParamFunction (a -> b -> out)

instance (Resource a) => SystemParam (Res a)

data App = App

newApp :: () -> App
newApp = undefined

injectResource :: (Resource a) => App -> a -> App
injectResource = undefined

addSystem :: IntoSystem a => App -> a -> App
addSystem = undefined

runApp :: App -> IO ()
runApp = undefined
