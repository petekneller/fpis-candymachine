package candymachine.catstransformers

import cats._
import cats.instances.list._
import cats.data._
import cats.data.WriterT._
import cats.data.StateT._
import candymachine._

object Simulation {

  type TheStack[A] = StateT[WriterT[Eval, List[Input], ?], Simulation, A]

  implicit val app = WriterT.catsDataApplicativeForWriterT[StateT[Eval, Simulation, ?], List[Input]]
  implicit val stackMonadState: MonadState[TheStack, Simulation] = StateT.catsDataMonadStateForStateT[WriterT[Eval, List[Input], ?], Simulation]

  case class Simulation(machine: CandyMachine, recordedInputs: List[Input])

  def create(inputs: Seq[Input]): TheStack[(Int, Int)] = {

    val transitions: List[TheStack[Unit]] = inputs.toList.map{ input =>
      MonadState[TheStack, Simulation].modify(s => s.copy(
        machine = CandyMachine.processInput(input, s.machine),
        recordedInputs = s.recordedInputs :+ input)
      )
    }

    for {
      _ <- Traverse[List].sequenceU(transitions)
      summary <- MonadState[TheStack, Simulation].get.map(s => (s.machine.candies, s.machine.coins))
    } yield summary
  }

}
