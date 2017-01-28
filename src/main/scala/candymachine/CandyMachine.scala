package candymachine

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class CandyMachine(locked: Boolean, candies: Int, coins: Int)

case class Simulation(machine: CandyMachine, recordedInputs: List[Input])
