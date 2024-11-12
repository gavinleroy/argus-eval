{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Bevy (App, Res, Resource, System, addSystem, injectResource, newApp, runApp)

newtype Timer = Timer Int deriving (Resource)

injectTimer :: App -> App
injectTimer app = injectResource app (Timer 0)

-- NOTE: a developer *might* expect this instance to be used at runtime.
-- However, it is not used because the signature matches the `IsFunctionSystem`
-- instance. Rust / Lean would disallow this because of the ambiguity.
instance System (Res Timer -> Int)

addIncoherentSystem :: App -> App
addIncoherentSystem app = addSystem app system
  where
    system :: Res Timer -> Int
    system _ = undefined

-- NOTE: Type Checks
-- Demonstrates that you can use custom systems that implement `System`
newtype CustomSystem a = CustomSystem a

instance System (CustomSystem a)

addCustomSystem :: App -> App
addCustomSystem app = addSystem app (CustomSystem ())

-- NOTE: Type Checks
-- Demonstrates that functions can have more than one argument
addRunTimersSystem :: App -> App
addRunTimersSystem app = addSystem app system
  where
    system :: Res Timer -> Res Timer -> Timer
    system _ _ = undefined

-- app/Main.hs:59:25: error: [GHC-39999]
--     • No instance for ‘Bevy.SystemParam Timer’
--         arising from a use of ‘addSystem’
--     • In the expression: addSystem app system
--       In an equation for ‘addRunTimerSystem’:
--           addRunTimerSystem app
--             = addSystem app system
--             where
--                 system :: Timer -> Timer
--                 system _ = undefined
--    |
-- 58 | addRunTimerSystem app = addSystem app system
--    |                         ^^^^^^^^^
addRunTimerSystem :: App -> App
addRunTimerSystem app = addSystem app system
  where
    system :: Timer -> Timer
    system _ = undefined

-- app/Main.hs:51:20: error: [GHC-39999]
--     • No instance for ‘System [Char]’ arising from a use of ‘addSystem’
--     • In the expression: addSystem app "NotASystem"
--       In an equation for ‘addBadSystem’:
--           addBadSystem app
--             = addSystem app "NotASystem"
--             where
--                 addCustomSystem :: App -> App
--                 addCustomSystem app = addSystem app (CustomSystem ())
--    |
-- 77 | addBadSystem app = addSystem app "NotASystem"
--    |                    ^^^^^^^^^
addBadSystem :: App -> App
addBadSystem app = addSystem app "NotASystem"

main :: IO ()
main =
  runApp
    . addRunTimerSystem
    . injectTimer
    . newApp
    $ ()
