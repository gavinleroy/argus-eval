package bevy

case class Timer(n: Int)
given Resource[Timer] with {}


// NOTE: type checks
def addCustomSystem(app: App) =
  case class CustomSystem()
  given System[CustomSystem] with {}
  app.addSystem(CustomSystem())


// [error] -- [E172] Type Error: ./src/main/scala/Main.scala:15:29
// [error] 28 |  app.addSystem("NotASystem")
// [error]    |                             ^
// [error]    |No given instance of type bevy.IntoSystem[String, M] was found for parameter intoSystem of method addSystem in class App.
// [error]    |I found:
// [error]    |
// [error]    |    bevy.given_IntoSystem_A_JustSystem[String](
// [error]    |      /* missing */summon[bevy.System[String]])
// [error]    |
// [error]    |But no implicit values were found that match type bevy.System[String]
// [error]    |
// [error]    |where:    M is a type variable
// [error]    |.
def addBadSystem(app: App) =
  app.addSystem("NotASystem")


// [error] -- [E172] Type Error: ./src/main/scala/Main.scala:10:25
// [error] 39 |  app.addSystem(runTimer)
// [error]    |                         ^
// [error]    |Ambiguous given instances: both given instance given_IntoSystem_A_JustSystem in package bevy and given instance given_IntoSystem_A_IsFunctionSystem in package bevy match type bevy.IntoSystem[bevy.Res[bevy.Timer] => bevy.Timer, M] of parameter intoSystem of method addSystem in class App
def addIncoherentSystem(app: App) =
  given System[Res[Timer] => Timer] with {}
  def runTimer(t: Res[Timer]): Timer =
    Timer(t.r.n + 1)
  app.addSystem(runTimer)


// [error] -- [E172] Type Error: ./src/main/scala/Main.scala:11:25
// [error] 62 |  app.addSystem(runTimer)
// [error]    |                         ^
// [error]    |No given instance of type bevy.IntoSystem[bevy.Timer => bevy.Timer, M] was found for parameter intoSystem of method addSystem in class App.
// [error]    |I found:
// [error]    |
// [error]    |    bevy.given_IntoSystem_A_IsFunctionSystem[bevy.Timer => bevy.Timer](
// [error]    |      bevy.given_SystemParamFunction_Function[bevy.Timer, bevy.Timer](
// [error]    |        /* missing */summon[bevy.SystemParam[bevy.Timer]])
// [error]    |    )
// [error]    |
// [error]    |But no implicit values were found that match type bevy.SystemParam[bevy.Timer]
// [error]    |
// [error]    |where:    M is a type variable
// [error]    |.
// [error] one error found
def addRunTimerSystem(app: App) =
  def runTimer(t: Timer): Timer =
    Timer(t.n + 1)
  app.addSystem(runTimer)


// [error] -- [E172] Type Error: ./src/main/scala/Main.scala:66:26
// [error] 66 |  app.addSystem(runTimers)
// [error]    |                          ^
// [error]    |No given instance of type bevy.IntoSystem[(bevy.Res[bevy.Timer], bevy.Res[bevy.Timer]) => Unit, M] was found for parameter intoSystem of method addSystem in class Ap
// [error]    |I found:
// [error]    |
// [error]    |    bevy.given_IntoSystem_A_JustSystem[
// [error]    |      (bevy.Res[bevy.Timer], bevy.Res[bevy.Timer]) => Unit](
// [error]    |      /* missing */
// [error]    |        summon[bevy.System[(bevy.Res[bevy.Timer], bevy.Res[bevy.Timer]) => Unit]]
// [error]    |    )
// [error]    |
// [error]    |But no implicit values were found that match type bevy.System[(bevy.Res[bevy.Timer], bevy.Res[bevy.Timer]) => Unit]
// [error]    |
// [error]    |where:    M is a type variable
def addMultiParamSystem(app: App) =
  def runTimers(timer1: Res[Timer], timer2: Res[Timer]): Unit = ()
  app.addSystem(runTimers)


@main def hello(): Unit =
  var app = App()
  app.run()
