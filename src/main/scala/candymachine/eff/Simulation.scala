package candymachine.eff

import cats._, data._
import cats.syntax.traverse._
import cats.instances.list._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import candymachine._

object Simulation {

  type WriterInputs[A] = Writer[Input, A]
  type StateCandyMachine[A] = State[CandyMachine, A]
  type TheStack = Fx.fx2[StateCandyMachine, WriterInputs]

  def create(inputs: Seq[Input]): Eff[TheStack, (Int, Int)] = {

    implicitly[WriterInputs |= TheStack]

    val transitions: Seq[Eff[TheStack, Unit]] = inputs.map{ input =>
      for {
        oldS <- get[TheStack, CandyMachine]
        newS: CandyMachine = CandyMachine.processInput(input, oldS)
        _ <- put[TheStack, CandyMachine](newS)
        _ <- tell[TheStack, Input](input)
      } yield ()
    }

    for {
      _ <- transitions.toList.sequenceU
      summary <- get[TheStack, CandyMachine].map(s => (s.candies, s.coins))
    } yield summary
  }

}
