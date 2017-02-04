package candymachine.eff

import cats._, data._
import org.atnos.eff._
import org.atnos.eff.syntax.all._
import candymachine._

class ParameterizedEffTest extends candymachine.CandyMachineTest {

  type WriterInputs[A] = Writer[Input, A]
  type StateCandyMachine[A] = State[CandyMachine, A]
  type TheStack = Fx.fx2[StateCandyMachine, WriterInputs]

  def runSimulation(inputs: Seq[Input], initialState: CandyMachine): (Int, Int, CandyMachine) = {
    val summary = ParameterizedSimulation.create[TheStack](inputs)
    val (((candies, coins), finalMachineState), recordedInputs) = summary.runState(initialState).runWriter.run
    Predef.assert(recordedInputs != Nil)
    (candies, coins, finalMachineState)
  }

}
