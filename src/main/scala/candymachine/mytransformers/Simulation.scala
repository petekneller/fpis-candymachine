package candymachine.mytransformers

import candymachine._

class Simulation[T[_]](stateMonad: StateMonad[CandyMachine, T], writerMonad: WriterMonad[Input, T]) {

  import stateMonad.monadImplicit

  private def transitionState(f: CandyMachine => CandyMachine): T[Unit] =
    stateMonad.modify(s => f(s))

  def create(inputs: Seq[Input]): T[(Int, Int)] = {

    val transitions: Seq[T[Unit]] = inputs.map{ input =>
      for {
        oldS <- stateMonad.get
        newS = CandyMachine.processInput(input, oldS)
        _ <- stateMonad.set(newS)
        _ <- writerMonad.write(input)
      } yield ()
    }

    for {
      _ <- stateMonad.sequence(transitions)
      summary <- stateMonad.get.map(s => (s.candies, s.coins))
    } yield summary
  }

}
