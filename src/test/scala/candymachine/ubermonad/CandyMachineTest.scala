package candymachine.ubermonad

import candymachine._

class CandyMachineTest extends candymachine.CandyMachineTest {

  def runSimulation(inputs: Seq[Input], initialState: CandyMachine): (Int, Int, CandyMachine) = {
    val simulation = Simulation.simulateMachine(inputs)
    val ((candies, coins), candymachine.Simulation(endMachine, recordedInputs)) = simulation.run(candymachine.Simulation(initialState, Nil))
    Predef.assert(recordedInputs != Nil)
    (candies, coins, endMachine)
  }

}
