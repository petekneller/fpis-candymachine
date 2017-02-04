package candymachine.eff

import org.atnos.eff.syntax.all._
import candymachine._

class EffTest extends candymachine.CandyMachineTest {

  def runSimulation(inputs: Seq[Input], initialState: CandyMachine): (Int, Int, CandyMachine) = {
    val summary = Simulation.create(inputs)
    val (((candies, coins), finalMachineState), recordedInputs) = summary.runState(initialState).runWriter.run
    Predef.assert(recordedInputs != Nil)
    (candies, coins, finalMachineState)
  }

}
