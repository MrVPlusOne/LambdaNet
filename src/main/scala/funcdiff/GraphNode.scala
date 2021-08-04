package funcdiff

sealed trait GraphMode
/** [[CompNode]] created under this mode consumes less memory but does not support back
 *  propagation. Use this mode during testing. */
case object ModeEval extends GraphMode
/** [[CompNode]] created under this mode needs to keep the entire computation graph
 * in the memory to support gradient back propagation. Use this mode during training. */
case object ModeTraining extends GraphMode
