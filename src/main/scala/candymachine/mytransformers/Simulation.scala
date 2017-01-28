package candymachine.mytransformers

import candymachine._

case class SimulationState(machine: CandyMachine)

class Simulation[T[_]](stateMonad: StateMonad[SimulationState, T], writerMonad: WriterMonad[Input, T]) {

  import stateMonad.monadImplicit

  private def transitionState(f: CandyMachine => CandyMachine): T[Unit] =
    stateMonad.modify(s => s.copy(
      machine = f(s.machine)
    ))

  def runCandyMachine(initialCandyMachine: CandyMachine, inputs: Seq[Input]): T[Seq[Unit]] = {

    val transitions: Seq[T[Unit]] = inputs.map{ input =>
      for {
        _ <- writerMonad.write(input)
        _ <- transitionState(CandyMachine.processInput(input, _))
      } yield ()
    }

    val finalState = stateMonad.sequence(transitions)
    finalState
  }

  def summaryOfMachine(simulation: T[_]): T[(Int, Int)] = {
    for {
      _ <- simulation
      summary <- stateMonad.get.map(s => (s.machine.candies, s.machine.coins))
    } yield summary
  }

  def initialState(initialCandyMachine: CandyMachine): SimulationState = SimulationState(initialCandyMachine)
}
