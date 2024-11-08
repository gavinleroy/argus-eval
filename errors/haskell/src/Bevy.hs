module Bevy ( App
            , Res
            , Resource
            , newApp
            , injectResource
            , addSystem
            , runApp
            ) where

data Res a = Res a

data IsFunctionSystem = IsFunctionSystem

class Resource a
class SystemParam a
class SystemParamFunction a
class System a
class IntoSystem a marker

instance (Resource a) => SystemParam (Res a)

instance (System a) => IntoSystem a ()

instance (SystemParamFunction a) => IntoSystem a IsFunctionSystem

instance (SystemParam a) =>
  SystemParamFunction (a -> out)
 
instance (SystemParam a, SystemParam b) =>
  SystemParamFunction (a -> b -> out)

instance (SystemParam a, SystemParam b, SystemParam c) =>
  SystemParamFunction (a -> b -> c -> out)

data App = App

newApp :: () -> App
newApp = undefined

injectResource :: (Resource a) => App -> a -> App
injectResource = undefined

addSystem :: IntoSystem a m => App -> a -> App
addSystem = undefined

runApp :: App -> IO ()
runApp = undefined
