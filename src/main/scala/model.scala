package org.cvogt.cosmetics
import org.cvogt.ansi.{colors => ansi}
import ansi.foreground._

case class MyType(
  tree: scala.meta.Tree,
  qualifiedName: List[String],
  children: Seq[MyType]
){
  def name = qualifiedName.last
  def prettyPrint: String = (
    if(name == "with"){
      children match {
        case p :: s :: _ if p.name == "Product" && s.name == "Serializable"
          => this.copy(children = children.drop(2)).prettyPrint
        case other => children.map(_.prettyPrint).mkString(" with ")
      }
    }else if(name == "()"){
      "( " ++ children.map(_.prettyPrint).mkString(", ") ++ " )"
    }else if(children.size == 2 && !name.exists(_.isLetterOrDigit)){
      children(0).prettyPrint ++ " " ++ name ++ " " ++ children(1).prettyPrint
    }else if(children.nonEmpty){
      name ++ "[" ++ children.map(_.prettyPrint).mkString(", ") ++ "]"
    }else name
  )
  def imports(color: String => String): Seq[String] = {
    (
      (
        if(qualifiedName.size > 1 && qualifiedName.head != "scala") Seq(qualifiedName.dropRight(1).mkString(".") ++ "." ++ color(qualifiedName.last))
        else Seq()
      ) ++ children.flatMap(_.imports(color))
    )
  }
}
object MyType{
  import scala.meta
  import scala.meta.{Type => _,Term => _,Name => _,_}
  import scala.meta.internal.ast._, Term.{Name => TermName, Super}, Type.{Name => TypeName, _}, Name.{Anonymous, Indeterminate}
  import scala.meta.dialects.Scala211;
  
  val ConstantTypeSuffix = "\\([0-9]+\\)".r
  def parse(s: String) = {
    val res = parseName(
      ConstantTypeSuffix.replaceAllIn( s, "" ).parse[meta.Type]
    )
    if(res.qualifiedName.head == "scala"){
      res.copy(qualifiedName = List(res.qualifiedName.last))
    }
    else res
  }
  private def parseName(t: scala.meta.Tree): MyType = {
    t match {
      case Type.Name(name) => MyType(t, name :: Nil,Seq())
      case Term.Name(name) => MyType(t, name :: Nil,Seq())
      case Term.Select(prefix, Term.Name(name)) =>
        val p = parseName(prefix)
        p.copy(tree = t, qualifiedName = p.qualifiedName :+ name)
      case Type.Select(prefix, Type.Name(name)) =>
        val p = parseName(prefix)
        p.copy(tree = t, qualifiedName = p.qualifiedName :+ name)
      case Type.Apply(name, args) => parseName(name).copy(tree = t, children=args.map(parseName))
      case Type.Function(args, body) => MyType(t, "=>" :: Nil, children=(args :+ body).map(parseName))
      case Type.Tuple(args) => MyType(t, "()" :: Nil, children=args.map(parseName))
      case Type.Compound(args,_) => MyType(t, "with" :: Nil, children=args.map(parseName))
      case other => MyType(t, "other: "+other.show[Structure]::Nil, Seq())
    }
  }
}

case class FoundRequired(
  found: MyType,
  foundExpandsTo: Option[MyType],
  required: MyType,
  requiredExpandsTo: Option[MyType]
){
  // FIXME: show "expands to"
  def render: String = {
    val is = (
      required.imports(red) ++ found.imports(green)
    ).distinct.sorted.map("import " ++ _)
    (
      is.mkString("\n") + "\n"
      ++ "found   " ++ ": " ++ red(found.prettyPrint)
      ++ "\n"
      ++ "required" ++ ": " ++ green(required.prettyPrint)
    )
  }
  /*
toSeq.flatMap{
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
*/
}

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
  foundRequired: Either[String,FoundRequired],
  codePos: (String,Int)
) extends Input{
  val (code,pos) = codePos
    def render = //s"""$file:$line: $message\n${" " * (pos-1)}^"""
  ///*def formatNative: Vector[String] = 
  {
    Vector(
      file ++ ":" ++ line.toString ++ ": " ++ message,
      foundRequired.fold(identity,_.render),
      code,
      (" "*pos) ++ "^"
    ).mkString("\n")
  }//*/
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
