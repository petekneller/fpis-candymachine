package candymachine.ubermonad

import candymachine._

class CandyMachineTest extends candymachine.CandyMachineTest {

  def runSimulation(inputs: Seq[Input], initialState: CandyMachine): (Int, Int, CandyMachine) = {
    val simulation = Simulation.create(inputs)
    val ((candies, coins), SimulationState(endMachine, recordedInputs)) = simulation.run(SimulationState(initialState, Nil))
    Predef.assert(recordedInputs != Nil)
    (candies, coins, endMachine)
  }

}
