package bevy

import scala.util.TupledFunction

trait IntoSystem[A, M]
trait SystemParamFunction[A]
trait System[A]
trait SystemParam[A]
trait Resource[A]

// Marker types
case class JustSystem()
case class IsFunctionSystem()

// Parameter wrapper
case class Res[R: Resource](r: R)


given [R: Resource]: SystemParam[Res[R]] with {}

given [T1: SystemParam, T2: SystemParam]:
  SystemParam[(T1, T2)] with {}

given [A: System]: IntoSystem[A, JustSystem] with {}
given [A: SystemParamFunction]: IntoSystem[A, IsFunctionSystem] with {}

// NOTE: produces an "ambiguous instace" because of the plain `Tuple`
// given [F, Args <: Tuple : SystemParam, Res](using TupledFunction[F, Args => Res]):
//   SystemParamFunction[F] with {}

given [Args: SystemParam, Res]:
  SystemParamFunction[Args => Res] with {}

case class App():
  def addSystem[S, M](s: S)(using intoSystem: IntoSystem[S, M]): Unit = ()
  def injectResource[R: Resource](r: R): Unit = ()
  def run(): Unit = println("Application Ran")
