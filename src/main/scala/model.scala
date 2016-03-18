package org.cvogt.cosmetics

case class MyType(
  qualifiedName: List[String],
  children: Seq[MyType]
){
def name = qualifiedName.last
  def prettyPrint: String = (
    if(children.size == 2 && !name.exists(_.isLetterOrDigit)){
      children(0).prettyPrint ++ " " ++ name ++ " " ++ children(1).prettyPrint
    }else if(children.nonEmpty){
      name ++ "[ " ++ children.map(_.prettyPrint).mkString(", ") ++ " ]"
    }else name
  )
  def imports(color: String => String): Seq[String] = {
    (
      (
        if(qualifiedName.size > 1) Seq(qualifiedName.dropRight(1).mkString(".") ++ "." ++ color(qualifiedName.last))
        else Seq()
      ) ++ children.flatMap(_.imports(color))
    )
  }
}

case class FoundRequired(
  found: MyType,
  foundExpandsTo: Option[MyType],
  required: MyType,
  requiredExpandsTo: Option[MyType]
)

/** Structured input as parsed from Scalac/SBT output */
trait Input
object Input{  
  def render( s: Stream[Input], renderErrorMessage: ErrorMessage => Vector[String] ): Stream[Output] = {
    s.flatMap{
      case (p: Output) => Stream(p)
      case (e: ErrorMessage) => renderErrorMessage(e).toStream.map(ErrorLine.apply)
    }
  }
}

sealed trait Type
case object Error extends Type
case object Warning extends Type

case class ErrorMessage(
  file: String,
  line: Int,
  tpe: Option[Type],
  message: String,
  //code: String,
  pos: Int//,
  //foundRequired: Option[FoundRequired]
) extends Input{
    def render = s"""$file:$line: $message\n${" " * (pos-1)}^"""
  /*def formatNative: Vector[String] = {
    Vector(
      file ++ ":" ++ line.toString ++ ": " ++ message
    ) ++ Vector(
      code,
      (" "*pos) ++ "^"
    ) ++
    foundRequired.toSeq.flatMap{
      case FoundRequired(found, expFound, required, expRequired) => (
        Vector("found   : " + found.prettyPrint)
        ++
        expFound.map("    which expands to " ++ _.prettyPrint )
        ++
        Vector("required   : " + found.prettyPrint)
        ++
        expRequired.map("    which expands to " ++ _.prettyPrint )
      )
    }
  }*/
}

//Output.sbt(Input.render(input, _.formatNative))
/** passthrough and line based output, so we could e.g. prefix lines with SBT error log markers */
trait Output
case class PassThrough(c: Char) extends Output with Input
case class ErrorLine(line: String) extends Output
object Output{
  def sbt(s: Stream[Output]): Unit = {
    s.foreach{
      case PassThrough(c) => print(c)
      case ErrorLine(l) => print(l)
    }
  }
}

final case class StreamCharSequence(_chars: Stream[Char]) extends CharSequence {
  def length: Int                                     = ??? // __sequenceOfChars.length
  def charAt(index: Int): Char                        = _chars(index)
  def subSequence(start: Int, end: Int): CharSequence = new StreamCharSequence(_chars.slice(start, end))
  override def toString                               = _chars.toString
}
