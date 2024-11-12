package bevy

trait Resource[A]
trait SystemParam[A]
trait SystemParamFunction[A]
trait System[A]

trait IntoSystem[A, M]:
  def print(): Unit

case class Res[R: Resource](r: R)

case class JustSystem()
case class IsFunctionSystem()

given [R: Resource]: SystemParam[Res[R]] with {}

given [T1: SystemParam, T2: SystemParam]:
  SystemParam[(T1, T2)] with {}

given [A: System]: IntoSystem[A, JustSystem] with {
  def print(): Unit = println("JustSystem")
}

given [A: SystemParamFunction]: IntoSystem[A, IsFunctionSystem] with {
  def print(): Unit = println("IsFunctionSystem")
}

given [P1: SystemParam, Out]:
  SystemParamFunction[P1 => Out] with {}

case class App():
  def injectResource[R: Resource](r: R): Unit = ()
  def addSystem[S, M](s: S)(using intoSystem: IntoSystem[S, M]): Unit =
      intoSystem.print()
  def run(): Unit = println("Application Ran")
