package candymachine

import org.scalatest.FunSuite

trait CandyMachineTest extends FunSuite {

  def runSimulation(inputs: Seq[Input], initialState: Machine): (Int, Int, Machine)

  test("locked -> {coin inserted} -> unlocked") {

    val (candies, coins, endMachine) = runSimulation(List(Coin), Machine(true, 1, 0))

    assert(endMachine.locked === false)
    assert(coins === 1)
  }

  test("unlocked -> {coin inserted} -> unlocked (doesn't keep coin)") {

    val (candies, coins, machine) = runSimulation(List(Coin), Machine(false, 1, 0))
    assert(machine.locked === false)
    assert(coins === 0)
  }

  test("locked -> {knob turned} -> locked") {

    val (candies, coins, machine) = runSimulation(List(Turn), Machine(true, 1, 0))

    assert(machine.locked === true)
    assert(candies === 1)
    assert(coins === 0)
  }

  test("* -> {* if out of candy} -> does nothing") {

    val (candies, coins, machine) = runSimulation(List(Coin), Machine(true, 0, 0))

    assert(machine.locked === true)
    assert(candies === 0)
    assert(coins === 0)
  }

  test("unlocked -> {knob turned} -> locked (and dispenses candy)") {

    val (candies, coins, machine) = runSimulation(List(Turn), Machine(false, 1, 1))

    assert(machine.locked === true)
    assert(candies === 0)
    assert(coins === 1)
  }

  test("everything") {

    val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
    val (candies, coins, _) = runSimulation(inputs, Machine(true, 5, 10))

    assert(candies === 1)
    assert(coins === 14)
  }

}
