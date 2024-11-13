import Leanbevy

structure CustomSystem (a: Type) where
  v : a

instance : System (CustomSystem Unit) where

structure Timer where
  n : Nat

instance : Resource Timer where


def injectTimer (app : App) : App :=
  injectResource (Timer.mk 0) app

-- NOTE: systems with multiple arguments type check
def addRunTimerSystems (app : App) : App :=
  addSystem app (fun (_ : Res Timer) (_ : Res Timer) => ())

-- NOTE: raw systems
def addCustomSystem (app : App) : App :=
  addSystem app (CustomSystem.mk ())

def addBadSystem (app : App) : App :=
  addSystem app "hello"

def runTimer (timer : Timer) : Timer :=
  { timer with n := timer.n + 1 }

def addRunTimerSystem (app : App) : App :=
  addSystem app runTimer

def main : IO Unit :=
  newApp Unit.unit
  |> injectTimer
  |> addRunTimerSystem
  |> runApp
