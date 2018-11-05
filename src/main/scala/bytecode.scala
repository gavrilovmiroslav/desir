
package desir

import gov.nasa.jpf.jvm._
import gov.nasa.jpf.vm._
import gov.nasa.jpf._
import javassist._

object Framing {
	def extractName(frame: JVMStackFrame, instIndex: Int, slot: Int): Option[String] = 
		frame.getLocalVars.toSeq
		 .filter(v => instIndex >= v.getStartPC - 1 && instIndex <= v.getStartPC + v.getLength)
		 .filter(v => v.getSlotIndex == slot)
		 .headOption.map { _.getName }

	def extractName(frame: JVMStackFrame, instIndex: Int, loc: Location): Location =
		extractName(frame, instIndex, loc.actualPosition) match {
			case Some(name) => Named(name)
			case None => 
				frame.getLocalVars.toSeq.map { println }
				loc
		}
}

class BytecodeAnalysisException(msg: String) extends Exception(msg)

case class Inst(op: String, args: Seq[Int], pos: Int) {
	override def toString = "(" + Seq(op, args.mkString(" ")).mkString(" ").trim + ")" 
}

object Inst {
	def get(op: String, arg: Seq[Int], pos: Int): Inst = 
		op match {
			case name if name.endsWith("_0") 
					  || name.endsWith("_1") 
					  || name.endsWith("_2") 
					  || name.endsWith("_3") 
					  || name.endsWith("_4") =>
				val parts = name.split('_')
				Inst(parts.head, parts.tail.head.toInt +: arg, pos)
			case _ =>
				Inst(op, arg, pos)
		}
}

class BytecodeAnalyzer(classAndMethodName: String) {
	val (className, methodName) = {
		val parts = classAndMethodName.split('.')
		(parts.init.mkString("."), parts.last)
	}

	val pool = ClassPool.getDefault
	val klass = pool.get(className)
	val method = klass.getDeclaredMethod(methodName)

	if(method == null)
		throw new BytecodeAnalysisException(s"Method ${className}.${methodName} not found.")

	val isStatic = Modifier.isStatic(method.getModifiers)

	val locals: Map[Int, Location] = {
		val parLen = method.getParameterTypes.length

		val values: Seq[Location] = (
			(if(!isStatic) Seq(This) else Seq[Location]()) ++
			(0 until parLen).map { i => Param(i + 1, if(isStatic) i else i + 1) }.toSeq ++
			(0 to 20).map { i => Local(i, (if(isStatic) 0 else 1) + parLen + i) })

		(0 until values.length).map { i => (i, values(i)) }.toMap
	}

	val consts = klass.getClassFile.getConstPool

	val instructions = {
		val it = method.getMethodInfo.getCodeAttribute.iterator
		it.begin

		var insts = Seq[Inst]()
		var current = 0
		while(it.hasNext) {
			var current = it.next
			var next = it.lookAhead
			val inst = (current until next).map { it.byteAt(_) }

			insts :+= Inst.get(javassist.bytecode.Mnemonic.OPCODE(inst.head), inst.tail, current)
		}

		insts
	}
}
