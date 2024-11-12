package bevy

case class Timer(n: Int)
given Resource[Timer] with {}

def runTimer(t: Timer): Timer =
  Timer(t.n + 1)

@main def hello(): Unit =
  var app = App()
  app.addSystem(runTimer)
  app.run()
