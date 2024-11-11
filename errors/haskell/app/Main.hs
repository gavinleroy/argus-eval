module Main where

import Bevy (App, Resource, Res, System, newApp, injectResource, addSystem, runApp)

instance Resource Timer

newtype Timer = Timer Int

newtype CustomSystem a = CustomSystem a

instance System (CustomSystem a)


injectTimer :: App -> App
injectTimer app = injectResource app (Timer 0)


-- NOTE: Type Checks
-- Demonstrates that functions can have more than one argument
addRunTimersSystem :: App -> App
addRunTimersSystem app = addSystem app runMultipleTimers
  where
    runMultipleTimers :: Res Timer -> Res Timer -> Timer
    runMultipleTimers _ _ = undefined


-- app/Main.hs:36:25: error: [GHC-39999]
--     • No instance for ‘Bevy.SystemParam Timer’
--         arising from a use of ‘addSystem’
--     • In the expression: addSystem app runTimer
--       In an equation for ‘addRunTimerSystem’:
--           addRunTimerSystem app
--             = addSystem app runTimer
--             where
--                 runTimer :: Timer -> Timer
--                 runTimer _ = undefined
--    |
-- 36 | addRunTimerSystem app = addSystem app runTimer
--    |                         ^^^^^^^^^
addRunTimerSystem :: App -> App
addRunTimerSystem app = addSystem app runTimer
  where
    runTimer :: Timer -> Timer
    runTimer _ = undefined


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
-- 51 | addBadSystem app = addSystem app "NotASystem"
--    |                    ^^^^^^^^^
addBadSystem :: App -> App
addBadSystem app = addSystem app "NotASystem"
  where
    addCustomSystem :: App -> App
    addCustomSystem app = addSystem app (CustomSystem ())


main :: IO ()
main = runApp
  . addRunTimerSystem
  . injectTimer
  . newApp
  $ ()
