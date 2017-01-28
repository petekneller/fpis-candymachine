package candymachine

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class CandyMachine(locked: Boolean, candies: Int, coins: Int)

object CandyMachine {
  def processInput(input: Input, currentState: CandyMachine): CandyMachine =
    input match {
      case Coin =>
        if (currentState.locked && currentState.candies > 0) currentState.copy(locked = false, coins = currentState.coins + 1)
        else currentState
      case Turn =>
        if (currentState.locked) currentState
        else currentState.copy(candies = currentState.candies - 1, locked = true)
    }
}
