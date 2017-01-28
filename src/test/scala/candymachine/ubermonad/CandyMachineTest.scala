package candymachine.ubermonad

import candymachine._

class CandyMachineTest extends candymachine.CandyMachineTest {

  def runSimulation(inputs: Seq[Input], initialState: CandyMachine): (Int, Int, CandyMachine) = {
    val simulation = Simulation.create(inputs)
    val ((candies, coins), endMachine, recordedInputs) = simulation.run(initialState, Seq.empty)
    Predef.assert(recordedInputs != Nil)
    (candies, coins, endMachine)
  }

}
