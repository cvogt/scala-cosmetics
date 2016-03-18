package org.cvogt.cosmetics
import org.cvogt.ansi.{colors => ansi}

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

  lazy val phases = " ".rep ~ "phase name" ~ (!("\n#partest\n") ~ AnyChar).rep ~ "\n#partest"
  lazy val summary = ("one" | "two" | "three" | "four" | Digit.rep) ~ " " ~ ("error" | "warning") ~ "s".? ~ " found".?

  lazy val scalacError = tpe ~ (!compileError ~ AnyChar).rep

  lazy val compileError = /*"\n".? ~ */ file ~ ":" ~ line ~ ": " ~ tpe.? ~ message ~ position map{
    (ErrorMessage.apply _).tupled
  }

  lazy val position = "\n" ~ ((" " | "\t").rep ~ "^").!.map(_.size)
  lazy val message = (!(position) ~ AnyChar).rep.!

  lazy val tpe = ((Error.toString as Error) | ("Error" as Error) | ("warning" as Warning)) ~ ": "
  lazy val Digit = CharIn('0' to '9')

  lazy val line = Digit.rep(1).!.map(_.toInt)

  lazy val file = (!":" ~ AnyChar).rep.!

  //def render

  lazy val compileErrorOrPassThrough = Start ~ (compileError.map(s => ansi.foreground.red(s.render)) | AnyChar.!/*map{v => /*print(v);*/ v}*/).map(print).rep ~ End

  def parse(expr: String) = grammar.parse(expr)
}
