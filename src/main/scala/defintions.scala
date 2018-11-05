
package desir

import javassist._
import gov.nasa.jpf.jvm._
import gov.nasa.jpf.vm._
import gov.nasa.jpf._

/*************************************************************
	Locations
**************************************************************/

trait Location {
	def toConcreteString(vars: Map[String, Any]): String =
		toString

	def actualPosition: Int

	def namedIn(frame: JVMStackFrame, instIndex: Int) =
		Framing.extractName(frame, instIndex, this)
}

case class Named(name: String) extends Location {
	var ap: Int = -1

	override def toConcreteString(vars: Map[String, Any]): String = {
		if(vars.contains(name)) s"$name(${vars(name)})" else s"${name}(?)"
	}

	override def actualPosition = ap

	override def toString = s"$name"
}

case object This extends Location {
	override def actualPosition = 0

	override def toString = s"this"
}

case class Param(n: Int, ap: Int) extends Location {
	assert(n >= 0)

	def actualPosition = ap
}

case class Local(n: Int, ap: Int) extends Location {
	assert(n >= 0)

	def actualPosition = ap
}

case class Instantiated(k: String) extends Location {
	def actualPosition = -1
}

case class Heap(n: Int) extends Location {
	assert(n >= 0)

	def actualPosition = -n
}

object Heap {
	private var count: Int = 0 

	def current = Heap(count)

	def next = {
		count += 1
		Heap(count)
	}
}

/*************************************************************
	Values
**************************************************************/

trait Value {
	def toConcreteString(vars: Map[String, Any]): String =
		toString
}

class Instance(k: String) extends Value {
	override def toString = s"instance($k)"
}

class Num(n: Float) extends Value {
	override def toString = s"$n"
}

object Invalid extends Value {
	override def toString = s"<invalid>"
}

object Nul extends Value {
	override def toString = s"<null>"
}

class From(val location: Location) extends Value {
	override def toString = s"$location"

	override def toConcreteString(vars: Map[String, Any]): String = 
		location.toConcreteString(vars)	
}

class Res(op: String, left: Value, right: Value) extends Value {
	override def toString = s"$left $op $right"	

	override def toConcreteString(vars: Map[String, Any]): String = 
		s"${left.toConcreteString(vars)} $op ${right.toConcreteString(vars)}"
}

/*************************************************************
	Execution Steps
**************************************************************/

trait ExecutionStep {
	def vars: Map[String, Any]
}

case class Assignment(left: Location, right: Value, vars: Map[String, Any] = Map()) extends ExecutionStep {
	override def toString = {
		s"${left.toConcreteString(vars)} := ${right.toConcreteString(vars)}"
	}
}

case class Assertion(rel: Relation, vars: Map[String, Any] = Map()) extends ExecutionStep {
	override def toString = s"assert ${rel.toConcreteString(vars)}"
}

case class Proc(name: String, args: Seq[Value], vars: Map[String, Any] = Map()) extends ExecutionStep {
	override def toString = s"$name(" + args.map { _.toConcreteString(vars) }.mkString(", ") + ")"
}

case class Func(name: String, args: Seq[Value], ret: Location, vars: Map[String, Any] = Map()) extends ExecutionStep {
	override def toString = s"$ret := $name(" + args.map { _.toConcreteString(vars) }.mkString(", ") + ")"
}

case class Exceptional(obj: Value, vars: Map[String, Any] = Map()) extends ExecutionStep {
	override def toString = s"throws ${obj.toConcreteString(vars)}"
}

case class Expected(obj: Value, vars: Map[String, Any] = Map()) extends ExecutionStep {
	override def toString = s"returns ${obj.toConcreteString(vars)} = ${vars("return-value")}"
}

