package candymachine.catstransformers

import cats._
import cats.instances.list._
import cats.data._
import cats.data.WriterT._
import cats.data.StateT._
import candymachine._

object Simulation {

  type TheStack[A] = StateT[WriterT[Eval, List[Input], ?], CandyMachine, A]

  implicit val stackMonadState: MonadState[TheStack, CandyMachine] = StateT.catsDataMonadStateForStateT[WriterT[Eval, List[Input], ?], CandyMachine]
  implicit val stackMonadWriter: MonadWriter[TheStack, List[Input]] = new MonadWriter[TheStack, List[Input]] {

    implicit val monadWriterForWriterT = MonadWriter[WriterT[Eval, List[Input], ?] , List[Input]]
    implicit val transForStateT: TransLift.Aux[StateT[?[_], CandyMachine, ?], Applicative] = StateT.catsDataLiftForStateT[CandyMachine]
    type F[A] = TheStack[A]
    type W = List[Input]

    /** Lift a writer action into the effect */
    def writer[A](aw: (W, A)): F[A] = transForStateT.liftT[WriterT[Eval, W, ?], A](monadWriterForWriterT.writer(aw))(monadWriterForWriterT)

    /** Run the effect and pair the accumulator with the result */
    def listen[A](fa: F[A]): F[(W, A)] = stackMonadState.flatMap(fa)(a =>
      transForStateT.liftT[WriterT[Eval, W, ?], (W, A)](monadWriterForWriterT.listen(monadWriterForWriterT.pure(a)))(monadWriterForWriterT))

    /** Apply the effectful function to the accumulator */
    def pass[A](fa: F[(W => W, A)]): F[A] = stackMonadState.flatMap(fa)(f =>
    transForStateT.liftT[WriterT[Eval, W, ?], A](monadWriterForWriterT.pass(monadWriterForWriterT.pure(f)))(monadWriterForWriterT))

    // Members declared in cats.Applicative
    def pure[A](x: A): F[A] = transForStateT.liftT[WriterT[Eval, W, ?], A](monadWriterForWriterT.pure(x))(monadWriterForWriterT)

    // Members declared in cats.FlatMap
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = stackMonadState.flatMap(fa)(f)

    def tailRecM[A, B](a: A)(f: A => F[Either[A,B]]): F[B] = stackMonadState.tailRecM(a)(f)
  }

  def create(inputs: Seq[Input]): TheStack[(Int, Int)] = {

    val transitions: List[TheStack[Unit]] = inputs.toList.map{ input =>
      for {
        oldS <- MonadState[TheStack, CandyMachine].get
        newS = CandyMachine.processInput(input, oldS)
        _ <- MonadState[TheStack, CandyMachine].set(newS)
        _ <- stackMonadWriter.tell(input :: Nil)
      } yield ()
    }

    for {
      _ <- Traverse[List].sequence(transitions)(stackMonadState)
      summary <- MonadState[TheStack, CandyMachine].get.map(s => (s.candies, s.coins))
    } yield summary
  }

}
