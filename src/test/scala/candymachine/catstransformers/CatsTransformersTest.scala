package candymachine.catstransformers

import cats._
import cats.instances.list._
import cats.data._
import cats.data.WriterT._
import candymachine._

class CatsTransformersTest extends CandyMachineTest {

  def runSimulation(inputs: Seq[Input], initialState: CandyMachine): (Int, Int, CandyMachine) = {

    /* State over Writer */
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


    val simulation = new Simulation[TheStack](stackMonadState, stackMonadWriter)(stackMonadState).create(inputs)
    val (recordedInputs, (endMachine, (candies, coins))) = simulation.run(initialState)(FlatMap[WriterT[Eval, List[Input], ?]]).run.value
    Predef.assert(recordedInputs != Nil)
    (candies, coins, endMachine)
  }

}
