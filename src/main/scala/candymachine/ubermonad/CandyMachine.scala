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

case class SimulationState(machine: CandyMachine, recordedInputs: List[Input])

object Simulation {
  def transitionMachine(input: Input)(f: (Input, CandyMachine) => CandyMachine): State[SimulationState, Unit] =
    State.modify(oldS => SimulationState(
      f(input, oldS.machine),
      oldS.recordedInputs :+ input
    ))

  def processSingleInput(input: Input): State[SimulationState, Unit] =
    transitionMachine(input)(CandyMachine.processInput _)

  def create(inputs: Seq[Input]): State[SimulationState, (Int, Int)] = for {
    _ <- State.sequence(inputs.map(processSingleInput))
    endState <- State.get[SimulationState]
  } yield (endState.machine.candies, endState.machine.coins)

}
