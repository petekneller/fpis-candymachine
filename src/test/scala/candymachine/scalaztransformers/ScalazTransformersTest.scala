package candymachine.scalaztransformers

import candymachine._

// since the implementation is not working
class ScalazTransformers { //extends CandyMachineTest {

  def runSimulation(inputs: Seq[Input], initialState: CandyMachine): (Int, Int, CandyMachine) = {
    val simulation = Simulation.create(inputs)
    val (Simulation(endMachine, _), (recordedInputs, (candies, coins))) = simulation.run(Simulation(initialState, Nil))
    Predef.assert(recordedInputs != Nil)
    (candies, coins, endMachine)
  }

}
