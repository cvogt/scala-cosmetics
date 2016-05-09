package org.cvogt.cosmetics
import org.cvogt.ansi.{colors => ansi}
import scala.util.Try

object ErrorMessageParser extends{
  import fastparse.all._
  implicit class ParserExtensions[T](p: Parser[T]){
    def as[O](o: O) = p.map(_ => o)
    //def foreach[T](f: T => V) = 
  }
  implicit class StringParserExtensions(p: String){
    def as[O](o: O) = P(p).map(_ => o)
  }
  lazy val grammar = Start ~ CharIn("\n").rep ~ ((scalacError | compileError | phases) ~ CharIn("\n").?).rep ~ (summary ~ CharIn("\n")).rep ~ CharIn("\n").rep ~ End

  lazy val phases = " ".rep ~ "phase name" ~ (!"\n#partest\n" ~ AnyChar).rep ~ "\n#partest"
  lazy val summary = ("one" | "two" | "three" | "four" | Digit.rep) ~ " " ~ ("error" | "warning") ~ "s".? ~ " found".?

  lazy val scalacError = tpe ~ (!compileError ~ AnyChar).rep

  lazy val compileError = /*"\n".? ~ */ file ~ ":" ~ line ~ ": " ~ tpe.? ~ message ~ error ~ codeAndPosition map{
    (ErrorMessage.apply _).tupled
  }

  lazy val position = "\n" ~ ((" " | "\t").rep ~ "^").!.map(_.size -1)
  lazy val codeAndPosition = "\n" ~ (!"\n" ~ AnyChar).rep.! ~ position
  lazy val message = (!"\n" ~ AnyChar).rep.!
  lazy val error = (typeError | (!codeAndPosition ~ AnyChar).rep.!.map(s => Left("raw{"++s++"\n}")))
  lazy val typeError = (
    "\n"
    ~
    " found" ~ " ".rep ~ ": " ~ (!"\n" ~ AnyChar).rep.!
    ~
    "\n"
    ~
    " required" ~ " ".rep ~ ": " ~ (!"\n" ~ AnyChar).rep.!
  ).map{
    case (found, required) => 
      import ansi.foreground._
      Try{
        Right(
          FoundRequired(
            found = MyType.parse(found),
            foundExpandsTo = None,
            required = MyType.parse(required),
            requiredExpandsTo = None
          )
        )
      }.recover{
        case e if e.isInstanceOf[scala.meta.ParseException] => Left(
          " found" ++ "   : " ++ red( found )
          ++ "\n"
          ++ " required" ++ ": " ++ green( required )
        )
      }.get
  }

  lazy val tpe = ((Error.toString as Error) | ("Error" as Error) | ("warning" as Warning)) ~ ": "
  lazy val Digit = CharIn('0' to '9')

  lazy val line = Digit.rep(1).!.map(_.toInt)

  lazy val file = (!":" ~ AnyChar).rep.!

  //def render

  lazy val compileErrorOrPassThrough = (
    Start
    ~ (
      compileError.map( s => s.render )
      | AnyChar.!/*map{v => /*print(v);*/ v}*/
    ).map(print).rep
    ~ End
  )

  def parse(expr: String) = grammar.parse(expr)
}
