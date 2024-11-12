-- This module serves as the root of the `Leanbevy` library.
-- Import modules here that should be built as part of the library.
import Leanbevy.Basic

class Resource (r : Type)
class SystemParam (p : Type)
class SystemParamFunction (f: Type)
class System (s : Type)

structure JustSystem where
structure IsFunctionSystem where

structure Res (r : Type) where
   val : r

instance [Resource a] : SystemParam (Res a) where

class IntoSystem (s: Type) (m : outParam Type)

instance [System a]
  : IntoSystem a JustSystem where

instance [SystemParamFunction a]
  : IntoSystem a IsFunctionSystem where

instance {a : Type} {b : Type} [SystemParam a]
  : SystemParamFunction (a -> b) where

def App : Type := Unit

def newApp (_ : Unit) : App := Unit.unit

def injectResource [Resource r] (_ : r) (app : App) : App := app

def addSystem [IntoSystem a m] (app : App) (_ : a) : App := app

def runApp (_ : App) : IO Unit := IO.println "Running app"
