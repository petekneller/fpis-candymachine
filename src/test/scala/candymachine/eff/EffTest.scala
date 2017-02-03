package candymachine.eff

import candymachine._

class EffTest extends candymachine.CandyMachineTest {

  def runSimulation(inputs: Seq[Input], initialState: CandyMachine): (Int, Int, CandyMachine) = {
    val summary = Simulation.create(inputs)
    val (recordedInputs, (finalMachineState, (candies, coins))) = summary.run(initialState).run(List.empty).run
    Predef.assert(recordedInputs != Nil)
    (candies, coins, finalMachineState)
  }

}
