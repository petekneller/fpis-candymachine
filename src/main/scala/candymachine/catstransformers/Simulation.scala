package candymachine.catstransformers

import cats._
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.functor._
import candymachine._

class Simulation[M[_]: Monad](state: MonadState[M, CandyMachine], writer: MonadWriter[M, List[Input]]) {

  def create(inputs: Seq[Input]): M[(Int, Int)] = {

    val transitions: List[M[Unit]] = inputs.toList.map{ input =>
      for {
        oldS <- state.get
        newS = CandyMachine.processInput(input, oldS)
        _ <- state.set(newS)
        _ <- writer.tell(input :: Nil)
      } yield ()
    }

    for {
      _ <- Traverse[List].sequence(transitions)(state)
      summary <- state.get.map(s => (s.candies, s.coins))
    } yield summary
  }
}
