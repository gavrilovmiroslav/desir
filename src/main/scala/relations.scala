
package desir

trait Relation {
	def left: Value
	def right: Value
	def negative: Relation
	def toConcreteString(vars: Map[String, Any]): String = 
		toString
}

object Relation {
	def fromString(str: String, a: Value, b: Value) = str match {
		case "lt" | "<" => Lt(a, b)
		case "gt" | ">" => Gt(a, b)
		case "le" | "≤" | "<=" => Le(a, b)
		case "ge" | "≥" | ">=" => Ge(a, b)
		case "eq" | "=" | "==" => Eq(a, b)
		case "ne" | "≠" | "!=" => Ne(a, b)
		case _ => throw new Exception(s"No relation $str")
	}
}

case class Ge(left: Value, right: Value) extends Relation {
	override def negative: Relation = Lt(left, right)
	override def toString = s"$left ≥ $right"
	override def toConcreteString(vars: Map[String, Any]): String = 
		s"${left.toConcreteString(vars)} ≥ ${right.toConcreteString(vars)}"
}

case class Le(left: Value, right: Value) extends Relation {
	override def negative: Relation = Gt(left, right)
	override def toString = s"$left ≤ $right"
	override def toConcreteString(vars: Map[String, Any]): String = 
		s"${left.toConcreteString(vars)} ≤ ${right.toConcreteString(vars)}"
}

case class Gt(left: Value, right: Value) extends Relation {
	override def negative: Relation = Le(left, right)
	override def toString = s"$left > $right"
	override def toConcreteString(vars: Map[String, Any]): String = 
		s"${left.toConcreteString(vars)} > ${right.toConcreteString(vars)}"
}

case class Lt(left: Value, right: Value) extends Relation {
	override def negative: Relation = Ge(left, right)
	override def toString = s"$left < $right"
	override def toConcreteString(vars: Map[String, Any]): String = 
		s"${left.toConcreteString(vars)} < ${right.toConcreteString(vars)}"
}

case class Eq(left: Value, right: Value) extends Relation {
	override def negative: Relation = Ne(left, right)
	override def toString = s"$left = $right"
	override def toConcreteString(vars: Map[String, Any]): String = 
		s"${left.toConcreteString(vars)} = ${right.toConcreteString(vars)}"
}

case class Ne(left: Value, right: Value) extends Relation {
	override def negative: Relation = Eq(left, right)
	override def toString = s"$left ≠ $right"
	override def toConcreteString(vars: Map[String, Any]): String = 
		s"${left.toConcreteString(vars)} ≠ ${right.toConcreteString(vars)}"
}
