package candymachine.ubermonad

import candymachine._

case class StateWriter[S, W, +A](run: (S, Seq[W]) => (A, S, Seq[W])) {

  def map[B](f: A => B): StateWriter[S, W, B] = StateWriter {
    (s, w) =>
      val (a, s2, w2) = run(s, w)
      val b = f(a)
      (b, s2, w2)
  }

  def flatMap[B](f: A => StateWriter[S, W, B]): StateWriter[S, W, B] = StateWriter {
    (s, w) =>
      val (a, s2, w2) = run(s, w)
      f(a).run(s2, w2)
  }
}

object StateWriter {
  def unit[S, W, A](a: A): StateWriter[S, W, A] = StateWriter((s, w) => (a, s, w))

  def get[S, W]: StateWriter[S, W, S] = StateWriter((s, w) => (s, s, w))

  def set[S, W](s: S): StateWriter[S, W, Unit] = StateWriter((_, w) => ((), s, w))

  def modify[S, W](f: S => S): StateWriter[S, W, Unit] = for {
    oldS <- StateWriter.get[S, W]
    newS = f(oldS)
    _ <- StateWriter.set(newS)
  } yield ()

  def write[S, W](entry: W): StateWriter[S, W, Unit] = StateWriter { (s, w) => ((), s, w :+ entry) }

  def map2[S, W, A, B, C](sa: StateWriter[S, W, A], sb: StateWriter[S, W, B])(f: (A, B) => C): StateWriter[S, W, C] = StateWriter {
    (s, w) =>
      val (a, s2, w2) = sa.run(s, w)
      val (b, s3, w3) = sb.run(s2, w2)
      (f(a, b), s3, w3)
  }

  def sequence[S, W, A](fs: Seq[StateWriter[S, W, A]]): StateWriter[S, W, Seq[A]] = {
    fs.foldLeft(unit[S, W, Seq[A]](Nil))((acc: StateWriter[S, W, Seq[A]], f: StateWriter[S, W, A]) => map2(acc, f)(_ :+ _))
  }
}

object Simulation {
  def transitionMachine(input: Input)(f: (Input, CandyMachine) => CandyMachine): StateWriter[CandyMachine, Input, Unit] = for {
    oldS <- StateWriter.get[CandyMachine, Input]
    newS = f(input, oldS)
    _ <- StateWriter.set(newS)
    _ <- StateWriter.write(input)
  } yield ()

  def processSingleInput(input: Input): StateWriter[CandyMachine, Input, Unit] =
    transitionMachine(input)(CandyMachine.processInput _)

  def create(inputs: Seq[Input]): StateWriter[CandyMachine, Input, (Int, Int)] = for {
    _ <- StateWriter.sequence(inputs.map(processSingleInput))
    endState <- StateWriter.get[CandyMachine, Input]
  } yield (endState.candies, endState.coins)

}
