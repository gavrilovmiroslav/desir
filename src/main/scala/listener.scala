
package desir.jpf

import desir._
import gov.nasa.jpf.jvm.bytecode._
import gov.nasa.jpf.jvm._
import gov.nasa.jpf.vm._
import gov.nasa.jpf._
import scala.util.{ Try, Success, Failure }

class DesIRListener(val target: String) extends ListenerAdapter {
	var execution = Seq[DelayedInstruction]()
	val analyzer = new BytecodeAnalyzer(target)
	var frame: JVMStackFrame = null
	val mapping = CollectingSemantics.index(analyzer)

	def classMethodName(method: MethodInfo) =
		s"${method.getClassName}.${method.getName}"

	override def methodEntered(vm: VM, currentThread: ThreadInfo, enteredMethod: MethodInfo) {
		val fullName = classMethodName(enteredMethod)
		if(target == fullName) {
			//println("in " + fullName)
			frame = currentThread.getTopFrame.asInstanceOf[JVMStackFrame]
		}
	}

	override def methodExited(vm: VM, currentThread: ThreadInfo, exitedMethod: MethodInfo) {
		val fullName = classMethodName(exitedMethod)
		if(target == fullName) {
			//println("out " + fullName)
		}
	}

	override def instructionExecuted(vm: VM, currentThread: ThreadInfo, nextInstruction: Instruction , executedInstruction: Instruction ) {
		if(nextInstruction != null) {
			val method = nextInstruction.getMethodInfo
			val fullName = classMethodName(method)
			if(target == fullName) {
				val pos = nextInstruction.getPosition

				//println("\t" + fullName + ":" + pos)
				//println("\t\t" + nextInstruction.getMnemonic)

				// todo: it's INT here for proof of concept, should be something more durable
				// why: stupid java interface with JPF
				mapping.get(pos) match {
					case Some(inst) =>
						val vars = 
							(frame.getLocalVars.toSeq
								.map { v => v.getName -> Try(frame.getLocalVariable(v.getName)) } 	// pay attention: getLocalVariable return INT!
								.filter { _._2.isSuccess } ++ 
									(if(nextInstruction.getMnemonic.endsWith("return")) 	// is this the return instruction?
										Seq("return-value" -> Try(frame.getResult))
									else
										Seq()
									)
							).map { case (name, value) => (name.toString, value.get) }

						//println(vars)
						execution :+= DelayedInstruction(vars.toMap, inst, pos)
					case None =>
						println(s"Shouldn't occur but still did? Got None from mapping at $pos (DesIRListener#instructionExecuted:52).")
				}
			}
		}
	}

	override def exceptionThrown(vm: VM, currentThread: ThreadInfo, thrownException: ElementInfo ) {
		println("except " + thrownException.getClassInfo.getName)
	}
}
