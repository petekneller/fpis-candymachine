package candymachine.catstransformers

import cats._
import cats.data.WriterT
import cats.data.WriterT._
import cats.instances.list._
import candymachine._

class CatsTransformersTest extends CandyMachineTest {

  def runSimulation(inputs: Seq[Input], initialState: CandyMachine): (Int, Int, CandyMachine) = {
    val simulation = Simulation.create(inputs)
    val (recordedInputs, (endMachine, (candies, coins))) = simulation.run(initialState)(FlatMap[WriterT[Eval, List[Input], ?]]).run.value
    Predef.assert(recordedInputs != Nil)
    (candies, coins, endMachine)
  }

}
