package candymachine.eff

import cats.data._
import cats.syntax.traverse._
import cats.instances.list._
import org.atnos.eff._
import org.atnos.eff.all._
import candymachine._

object ParameterizedSimulation {

  def create[R](inputs: Seq[Input])(implicit s: State[CandyMachine, ?] |= R, w: Writer[Input, ?] |= R): Eff[R, (Int, Int)] = {

    val transitions: Seq[Eff[R, Unit]] = inputs.map{ input =>
      for {
        oldS <- get[R, CandyMachine]
        newS: CandyMachine = CandyMachine.processInput(input, oldS)
        _ <- put[R, CandyMachine](newS)
        _ <- tell[R, Input](input)
      } yield ()
    }

    for {
      _ <- transitions.toList.sequence
      summary <- get[R, CandyMachine].map(s => (s.candies, s.coins))
    } yield summary
  }

}
