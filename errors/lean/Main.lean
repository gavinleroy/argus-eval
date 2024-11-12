import Leanbevy

structure Timer where
  n : Nat

instance : Resource Timer where

def injectTimer (app : App) : App :=
  injectResource (Timer.mk 0) app

def runTimer (timer : Timer) : Timer :=
  { timer with n := timer.n + 1 }

def addRunTimerSystem (app : App) : App :=
  addSystem app runTimer

def main : IO Unit :=
  newApp Unit.unit
  |> injectTimer
  |> addRunTimerSystem
  |> runApp
