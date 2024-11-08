module Main where

import Bevy (App, Resource, newApp, injectResource, addSystem, runApp)

instance Resource Timer

data Timer = Timer Int

injectTimer :: App -> App
injectTimer app = injectResource app (Timer 0)

runTimer :: Timer -> Timer
runTimer (Timer n) = Timer (n + 1)

addRunTimerSystem :: App -> App
addRunTimerSystem app = addSystem app runTimer

main :: IO ()
main = runApp . addRunTimerSystem . injectTimer . newApp $ ()
