
package desir

import javassist._
import gov.nasa.jpf.jvm._
import gov.nasa.jpf.vm._
import gov.nasa.jpf._

/*************************************************************
	Traces
**************************************************************/

case class Trace(stack: List[Value], env: Map[Location, Value], exec: List[ExecutionStep]) {
	def executionTrace = exec
}

object Trace {
	def empty(locals: Map[Int, Location]) = Trace(Nil, locals.map { case (k, v) => (v, Nul) }, Nil)
}

/*************************************************************
	Collecting Semantics Elements
**************************************************************/

trait CollectingSemantics { this: CollectingSemantics =>
	private var pos: Int = -1

	def at(p: Int) = {
		pos = p
		this
	}

	def transform(trace: Trace, vars: Map[String, Any]): Trace
}

case class Just(inst: Inst) extends CollectingSemantics {
	override def transform(trace: Trace, vars: Map[String, Any]) = 
		trace
}

case class Store(location: Location) extends CollectingSemantics {
	override def transform(trace: Trace, vars: Map[String, Any]) = {
		trace.copy(
			stack = trace.stack.tail, 
			env   = trace.env + (location -> trace.stack.head),
			exec  = trace.exec :+ Assignment(location, trace.stack.head).copy(vars = vars)
		)
	}
}

case class Load(location: Location) extends CollectingSemantics {
	override def transform(trace: Trace, vars: Map[String, Any]) = {
		trace.copy(
			stack = new From(location) :: trace.stack,
			env   = trace.env
		)
	}
}

case object Dup extends CollectingSemantics {
	override def transform(trace: Trace, vars: Map[String, Any]) = 
		trace.copy(stack = trace.stack.head :: trace.stack)
}

case class BinOp(op: String) extends CollectingSemantics {
	override def transform(trace: Trace, vars: Map[String, Any]) = {
		val right = trace.stack.head
		val left = trace.stack.tail.head
		val res = new Res(op, left, right)

		trace.copy(
			stack = res :: trace.stack.tail.tail
		)
	}
}

// todo: fix args
case class New(klass: String) extends CollectingSemantics {
	val heap = Heap.next
	override def transform(trace: Trace, vars: Map[String, Any]) = {
		val instance = new Instance(klass)
		trace.copy(
			env   = trace.env + (heap -> instance),
			exec  = trace.exec :+ Assignment(heap, instance).copy(vars = vars),
			stack = new From(heap) :: trace.stack
		)
	}
}

case class If(rel: String, sat: Boolean) extends CollectingSemantics {
	override def transform(trace: Trace, vars: Map[String, Any]) = {
		val r: Relation = Relation.fromString(rel, trace.stack.head, new Num(0))

		trace.copy(
			stack = trace.stack.tail,
			exec  = trace.exec :+ Assertion(if(sat) r else r.negative).copy(vars = vars)
		)
	}
}

case class Cmp(rel: String, sat: Boolean) extends CollectingSemantics {
	override def transform(trace: Trace, vars: Map[String, Any]) = {
		val r: Relation = Relation.fromString(rel, trace.stack.tail.head, trace.stack.head)

		trace.copy(
			stack = trace.stack.tail.tail,
			exec  = trace.exec :+ Assertion(if(sat) r else r.negative).copy(vars = vars)
		)
	}
}

case class MethodCall(static: Boolean, klass: String, name: String, sig: String) extends CollectingSemantics {
	def getReturnType(sig: String): Option[String] = 
		sig.split(')').last match {
			case "V" => None
			case "I" => Some("Int")
			case "Z" => Some("Boolean")
			case "B" => Some("Byte")
			case "C" => Some("Char")
			case "S" => Some("Short")
			case "J" => Some("Long")
			case "F" => Some("Float")
			case "D" => Some("Double")
			case name if name.startsWith("[") => 
				Some("Array[" + getReturnType(name.substring(1)).get + "]")
			case name =>
				Some(name.substring(1, name.length - 1).replaceAll("/", "."))
		}

	def getArgumentCount(sig: String): Int = {
		val params = sig.split(')').head.substring(1)
		val noObj = params.replaceAll("L[a-zA-Z0-9$_/\\.]+;", "O")
		val noArray = noObj.replaceAll("\\[[a-zA-Z0-9]{1}", "A")
		noArray.length
	}

	override def transform(trace: Trace, vars: Map[String, Any]) = {
		val obj = if(static) None else Some(trace.stack.head)
		val paramCount = getArgumentCount(sig)
		val params = trace.stack.take(paramCount).toSeq.reverse
		val stack = (if(static) trace.stack else trace.stack.tail).drop(paramCount)
		val ret = getReturnType(sig).map { k => Heap.next -> new Instance(k) }

		val env = ret.map { r => trace.env + r }.getOrElse(trace.env)
		val execAdd = ret match {
			case Some(r) if static =>
				Func(s"$klass.$name", params, Heap.current).copy(vars = vars)
			case Some(r) =>
				Func(s"${obj.get}.$name", params, Heap.current).copy(vars = vars)
			case None if static =>
				Proc(s"$klass.$name", params).copy(vars = vars)
			case None =>
				Proc(s"${obj.get}.$name", params).copy(vars = vars)
		}

		val exec = trace.exec :+ execAdd

		trace.copy(
			stack = stack,
			env = env,
			exec = exec
		)
	}
}

case object Throw extends CollectingSemantics {
	override def transform(trace: Trace, vars: Map[String, Any]) = trace.stack.head match {
		case l: From =>
			trace.copy(
				stack = trace.stack.tail,
				exec  = trace.exec :+ Exceptional(trace.env(l.location)).copy(vars = vars)
			)
		case other =>
			trace.copy(
				stack = trace.stack.tail,
				exec  = trace.exec :+ Exceptional(trace.stack.head).copy(vars = vars)
			)		
	}
}

case object Return extends CollectingSemantics {
	override def transform(trace: Trace, vars: Map[String, Any]) = 
		trace.copy(
			stack = Nil,
			exec = trace.exec :+ Expected(trace.stack.head).copy(vars = vars)
		)
}

case class ConcreteInstruction(vars: Map[String, Any], inst: CollectingSemantics) {
	def transform(trace: Trace) = 
		inst.transform(trace, vars)
}

case class DelayedInstruction(vars: Map[String, Any], inst: Int => CollectingSemantics, pos: Int) {
	def apply(n: Int, frame: JVMStackFrame) = {
		val renamed = inst(n).at(pos)  match {
			case Store(loc) => Store(loc.namedIn(frame, pos))
			case Load(loc) => Load(loc.namedIn(frame, pos))
			case other => other
		}

		ConcreteInstruction(vars, renamed)
	}
}

object CollectingSemantics {
	def lift(ba: BytecodeAnalyzer)(inst: Inst): (Int => CollectingSemantics) = {
		(nextPos: Int) => {
			(inst match {
				case Inst(name, args, _) if name.endsWith("load") => 
					Load(ba.locals(args.head))
				
				case Inst(name, args, _) if name.endsWith("store") => 
					Store(ba.locals(args.head))
				
				case Inst(name, args, _) if name.endsWith("mul") => 
					BinOp("*")
				
				case Inst(name, args, _) if name.endsWith("sub") => 
					BinOp("-")
				
				case Inst(name, args, pos) if name.startsWith("if_a") || name.startsWith("if_i") => 
					val index = (args(0) << 8) | args(1)
					Cmp(name.takeRight(2), pos + index == nextPos)
				
				case Inst(name, args, pos) if name.startsWith("if") => 
					val index = (args(0) << 8) | args(1)
					If(name.substring(2), pos + index == nextPos)
				
				case Inst("new", args, _) => 
					val index = (args(0) << 8) | args(1)
					New(ba.klass.getClassFile.getConstPool.getClassInfo(index))
				
				case Inst("dup", args, _) => 
					Dup
				
				case Inst(name, args, _) if name.startsWith("invoke") =>
					val index = (args(0) << 8) | args(1)
					val klass = ba.klass.getClassFile.getConstPool.getMethodrefClassName(index)
					val meth = ba.klass.getClassFile.getConstPool.getMethodrefName(index)
					val sig = ba.klass.getClassFile.getConstPool.getMethodrefType(index)
					MethodCall((name == "invokestatic"), klass, meth, sig)
				
				case Inst(name, _, _) if name.endsWith("throw") =>
					Throw				
				
				case Inst(name, _, _) if name.endsWith("return") && name != "return" =>
					Return
				case other => Just(other)
			}).at(inst.pos)
		}
	}

	def index(ba: BytecodeAnalyzer): Map[Int, Int => CollectingSemantics] = {
		ba.instructions.map { case inst =>
			inst.pos -> lift(ba)(inst)
		}.toMap
	}

	def execute(frame: JVMStackFrame, ba: BytecodeAnalyzer, pi: Seq[DelayedInstruction]) = {
		val nextPos = pi.map { _.pos }.tail ++ Seq(pi.last.pos)
		val concretePath = pi.zip(nextPos).map { case (now, next) => 
			now(next, frame)
		}

		val trace = concretePath.foldLeft(Trace.empty(ba.locals)) { case (trace, inst) =>
			inst.transform(trace)
		}

		trace.executionTrace.foreach { println }
	}
}
