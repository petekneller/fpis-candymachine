package candymachine.mytransformers

import candymachine._

class MyTransformersTest extends candymachine.CandyMachineTest {

  def runSimulation(inputs: Seq[Input], initialState: CandyMachine): (Int, Int, CandyMachine) = {

//    /* StateT[WriterT[Identity]]
    type M1[X] = Identity[X]
    type M2[X] = WriterT[Input, M1, X]
    type M3[X] = StateT[CandyMachine, M2, X]

    val identityOps = new IdentityOps
    val writerOps = new WriterTOps[Input, M1](identityOps)
    val stateOps = new StateTOps[CandyMachine, M2](writerOps)

    val stateWriterOps = WriterTOps.toWriterMonad[Input, M2, M3](stateOps, writerOps)

    val simulation = new Simulation[M3](stateOps, stateWriterOps)
//    */

    /*  WriterT[StateT[Identity]]
    type M1[X] = Identity[X]
    type M2[X] = StateT[SimulationState, M1, X]
    type M3[X] = WriterT[Input, M2, X]

    val identityOps = new IdentityOps
    val stateOps = new StateTOps[SimulationState, M1](identityOps)
    val writerOps = new WriterTOps[Input, M2](stateOps)

    val writerStateOps = StateTOps.toStateMonad[SimulationState, M2, M3](writerOps, stateOps)

    val simulation = new Simulation[M3](writerStateOps, writerOps)
    */

    // common
    val summary = simulation.create(inputs)

//    /*   StateT[WriterT[Identity]]
    val (recordedInputs, (finalMachineState, (candies, coins))) = summary.run(initialState).run(List.empty).run
//    */

    /*   WriterT[StateT[Identity]]
    val (finalMachineState, (recordedInputs, (candies, coins))) = summary.run(List.empty).run(initialState).run
    */

    // common
    Predef.assert(recordedInputs != Nil)
    (candies, coins, finalMachineState)
  }

}
