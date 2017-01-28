package candymachine.ubermonad

import candymachine._

case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = State {
    s =>
      val (a, s2) = run(s)
      val b = f(a)
      (b, s2)
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State {
    s =>
      val (a, s2) = run(s)
      f(a).run(s2)
  }
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[A](s: A): State[A, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    oldS <- State.get[S]
    newS = f(oldS)
    _ <- State.set(newS)
  } yield ()

  def map2[S, A, B, C](sa: State[S, A], sb: State[S, B])(f: (A, B) => C): State[S, C] = State {
    (s: S) =>
      val (a, s2) = sa.run(s)
      val (b, s3) = sb.run(s2)
      (f(a, b), s3)
  }

  def sequence[S, A](fs: Seq[State[S, A]]): State[S, Seq[A]] = {
    fs.foldLeft(unit[S, Seq[A]](Nil))((acc: State[S, Seq[A]], f: State[S, A]) => map2(acc, f)(_ :+ _))
  }
}

object Simulation {
  def transitionMachine(input: Input)(f: CandyMachine => CandyMachine): State[Simulation, Unit] =
    State.modify(s => s.copy(
      machine = f(s.machine),
      recordedInputs = s.recordedInputs :+ input
    ))

  def summarizeMachine: State[Simulation, (Int, Int)] =
    State.get[Simulation].map(s => (s.machine.candies, s.machine.coins))

  def simulateMachine(inputs: Seq[Input]): State[Simulation, (Int, Int)] = {
    def singleTransition(input: Input): State[Simulation, Unit] = {
      input match {
        case Coin => Simulation.transitionMachine(input) {
          (m: CandyMachine) =>
            if (m.locked && m.candies > 0) m.copy(locked = false, coins = m.coins + 1)
            else m
        }
        case Turn => Simulation.transitionMachine(input) {
          (m: CandyMachine) =>
            if (m.locked) m
            else m.copy(candies = m.candies - 1, locked = true)
        }
      }
    }

    val endS = State.sequence(inputs.map(singleTransition))

    for {
      _ <- endS
      summary <- Simulation.summarizeMachine
    } yield summary
  }

}
