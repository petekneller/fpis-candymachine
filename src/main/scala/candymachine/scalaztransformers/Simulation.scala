package candymachine.scalaztransformers

import scalaz._
// import scalaz.Id
// import scalaz.std.list._
// import scalaz.WriterT._
// import scalaz.StateT._
import candymachine._

case class Simulation(machine: CandyMachine, recordedInputs: List[Input])

object Simulation {

  type WriterTState[A] = WriterT[State[Simulation, ?], List[Input], A]

  def create(inputs: Seq[Input]): WriterTState[(Int, Int)] = {

    // I really cannot figure out how to get a MonadState of WriterT

    // val a = MonadState[State[Simulation, ?], Simulation]
    // val b = Applicative[State[Simulation, ?]]
    // val c = Applicative[Lambda[B => WriterT[Lambda[A => State[Simulation, A]], List[Input], B]]]
    // val d = Monoid[List[Input]]
    // val e = Applicative[WriterT[Id, List[Input], ?]]

    // val transitions = inputs.map{ input =>
    //   MonadState[Lambda[A => WriterT[Lambda[B => State[Simulation, B]], List[Input], A]], Simulation].modify(s => s.copy(
    //     machine = CandyMachine.processInput(input, s.machine),
    //     recordedInputs = s.recordedInputs :+ input)
    //   )
    // }

    // for {
    //   _ <- Traverse[List].sequence[WriterTState, Unit](transitions)
    //   summary <- MonadState[Lambda[A => WriterT[Lambda[B => State[Simulation, B]], List[Input], A]], Simulation].get.map(s => (s.machine.candies, s.machine.coins))
    // } yield summary
    ???
  }
}
