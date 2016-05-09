package org.cvogt.cosmetics
import scala.annotation.tailrec
import org.cvogt.ansi.{colors => ansi}
import scala.meta
import scala.meta.{Type => _,Term => _,Name => _,_}
import scala.meta.internal.ast._, Term.{Name => TermName, Super}, Type.{Name => TypeName, _}, Name.{Anonymous, Indeterminate}
import scala.meta.dialects.Scala211;

object Main2 extends App{
  val msgs = Vector(
    """
/Users/chris/code/scalac-cosmetics/src/main/scala/parser.scala:22: type mismatch;
 found   : ((String, Int, Option[org.cvogt.cosmetics.Type], String, Either[String,org.cvogt.cosmetics.FoundRequired], (String, Int))) => org.cvogt.cosmetics.ErrorMessage
 required: ((String, Int, Option[Product with Serializable with org.cvogt.cosmetics.Type], Product with Serializable with scala.util.Either[String,org.cvogt.cosmetics.FoundRequired], (String, Int))) => ?
    (ErrorMessage.apply _).tupled
                           ^
one error found""".trim,
    """
/Users/chris/code/scalac-cosmetics/src/main/scala/Main.scala:11: type mismatch;
 found   : String("df")
 required: Int
  val i: Int = "df"
               ^
one error found""".trim
  ).map{ msg =>
    println("-"*80)
    println(msg)
    println("-"*80)
    ErrorMessageParser.compileErrorOrPassThrough.parse(msg)
    println()
    println("-"*80)
  }

  val zincOutput = s"""
[info] Compiling 1 Scala source to /Users/chris/code/cbt-example/test/build/target/scala-2.11/classes...
[info] Compile success at May 3, 2016 2:13:49 PM [0.312s]
Compiling to /Users/chris/code/cbt-example/test/target/scala-2.11/classes
[info] Compiling 1 Scala source to /Users/chris/code/cbt-example/test/target/scala-2.11/classes...
[error] /Users/chris/code/cbt-example/test/Main.scala:1: not found: object ammonite
[error] import ammonite.ops._
[error]        ^
[error] /Users/chris/code/cbt-example/test/Main.scala:4: not found: value Foo
[error]     assert( Foo(1,"test").i == 1 )
[error]             ^
[error] /Users/chris/code/scalac-cosmetics/src/main/scala/parser.scala:22: type mismatch;
[error]  found   : ((String, Int, Option[org.cvogt.cosmetics.Type], String, Either[String,org.cvogt.cosmetics.FoundRequired], (String, Int))) => org.cvogt.cosmetics.ErrorMessage
[error]  required: ((String, Int, Option[Product with Serializable with org.cvogt.cosmetics.Type], Product with Serializable with scala.util.Either[String,org.cvogt.cosmetics.FoundRequired], (String, Int))) => ?
[error]     (ErrorMessage.apply _).tupled
[error]                            ^
[error] two errors found
[error] Compile failed at May 3, 2016 2:13:49 PM [0.323s]
""".toStream
  
  println(
    StreamingParser.scan(zincOutput)
  )
}
object StreamingParser{
  import java.io.{BufferedReader, InputStreamReader}
  val in = new BufferedReader(new InputStreamReader(System.in))
  import ansi._
  import ansi.foreground._
  
  val baseColors = Seq( red _, magenta _, green _, cyan _, blue _, yellow _ )
  val boldColors = baseColors.map(c => (s:String) => bold(c(s)))
  val allColors = (baseColors ++ boldColors).map(_("*"))
  import scala.util.Random
  print(Random.shuffle(allColors).mkString ++ " scalac-cosmetics enabled " ++ Random.shuffle(allColors).mkString)

  lazy val `[error] ` = "[0m[[31merror[0m] [0m"
  lazy val `[info] ` = "[0m[[0minfo[0m] [0m"



  val input =
    '\n' #::
    Stream
      .continually(in.read)
      .takeWhile(_ != 4)
      .takeWhile(_ != -1)
      .map(_.toChar)
      //.map(_.replace(RED,""))
      //.map(_.replace(RESET,""))
      /*.filter(!_.contains("support was removed in 8.0"))
      .filter(!_.contains("Loading global plugins from"))
      .filter(!_.contains("Loading project definition from"))
      .filter(!_.contains("Set current project to"))*/
      //.map(_.map(s => "%x".format(s.toByte)).mkString)

  trait LinesOrContinue
  case class Line(
    line: String
  ) extends LinesOrContinue
  case class End( stream: Stream[Char]) extends LinesOrContinue

  implicit class CharStreamExtensions(s: Stream[Char]){
    def splitLine: (String, Stream[Char]) = {
      val line = s.takeWhile(_ != '\n')
      ( line.mkString, s.drop(line.size+1) )
    }
    def takeLinesWhileStartsWith(pattern: String): Stream[LinesOrContinue] = {
      if(s.startsWith(pattern)) {      
        val (line, tail) = splitLine
        Line(line) #:: tail.takeLinesWhileStartsWith(pattern)
      } else Stream(End(s))
    }
  }

/*
  def processLines( s: Stream[Char] ): Stream[Char] = {
    s match{
      case _ if
        Seq(
          `[info] ` ++ "Loading global plugins from",
          `[info] ` ++ "Loading project definition from",
          `[info] ` ++ "Set current project to scala",
          "Java HotSpot(TM) 64-Bit Server VM warning"
        ).exists(s.startsWith(_))
        =>
        //println("4b"+tail.take(30).flatMap(_.toString.getBytes.map("%02x".format(_)).mkString).mkString) 
        val (line, tail) = s.splitLine
        //println(line)
        //println("4b"+newTail.take(30).flatMap(_.toString.getBytes.map("%02x".format(_)).mkString).mkString) 
        processLines(tail)
      case _ if s.startsWith(`[error] `) =>
        val lines = s.takeLinesWhileStartsWith(`[error] `)
        process(lines.collect{ case Line(line) => line})
        lines.collect{ case End(stream) => stream}.head
      case _ =>
        s
    }
  }*/


  @scala.annotation.tailrec
  def scan( s: Stream[Char] ): Unit = {
    if(s.nonEmpty){
      scan{
        // this code is intricate. changing it may easily break interactive session UX
        s.head match {
          case c =>
            print(c)
            c match {
              case c@'\n' if s.tail.startsWith(`[error] `) || s.tail.startsWith("[error] ") =>
                process(s.tail)
              case _ => s.tail
            }
        }
      }
    }
  }

  //process(input)

  def formatMsgAndLine(msg: String, file: String, lineNoStr: String) = {
    val path = file.split("/")
    
    val projectPath = path.takeWhile(_ != "src")

    val config = path.dropWhile(_ != "src").takeWhile(_ != "scala")

    val fileInProject = path.dropWhile(_ != "src").dropWhile(_ != "scala").drop(1)

    (
      /*
      `[error] `
      ++
      */
      projectPath.dropRight(1).mkString("/")
      ++
      "/"
      ++
      blue(projectPath.last)
      ++
      ":"
      ++
      lineNoStr
      ++
      ": "
      ++
      background.red(msg)
      /*
      ++
      "/"
      ++
      config.mkString("/") ++ "/scala"
      ++ "/" ++
      blue(
        fileInProject.dropRight(1).mkString("/")
      )
      ++
      "/"
      ++
      underline(
        blue(
          fileInProject.last ++ ":" ++ lineNoStr
        )
      )
      */
    )
  }

  def formatFoundRequired(found: String, required: String) = {
    def parse(s: String) = MyType.parse(s)

    println("")
    try{
      val ConstantTypeSuffix = "\\([0-9]+\\)".r
      val requiredParsed = parse( ConstantTypeSuffix.replaceAllIn(required,"") )
      val foundParsed = parse( ConstantTypeSuffix.replaceAllIn(found,"") )
      val is = (requiredParsed.imports(red) ++ foundParsed.imports(green)).distinct.sorted.map("import " ++ _)
      if(is.nonEmpty){
        is.foreach(println)
        println("")
      }
      println(
        "found   " ++ ": " ++ red(foundParsed.prettyPrint)
        ++
        "\n"
        ++
        "required" ++ ": " ++ green(requiredParsed.prettyPrint)
      )
    } catch {
      case e: scala.meta.ParseException =>
        println("required" ++ ": " ++ green(required) )
        println("found" ++ "   : " ++ red(found) )
    }    
  }
  def formatCode(file: String, lineNoStr: String, code: String, caretPrefix: String){
    val lineNo = lineNoStr.toInt - 1

    // FIXME: beginning of file
    val source = scala.io.Source.fromFile(file).getLines.zipWithIndex.map(_.swap).toMap

    println("")

    if(lineNo-2 >= 0)
      println( blue((lineNo-2).toString ++ ": ") ++ source(lineNo - 2) )
    if(lineNo-1 >= 0)
      println( blue((lineNo-1).toString ++ ": ") ++ source(lineNo - 1) )

    val uncolored = ansi.strip(caretPrefix)
    val cleaned = uncolored.substring(uncolored.indexOf(" ")+1)
    val pos = cleaned.size
    def identChar = (c: Char) => c.isLetter || c.isDigit || c == "_"// || (c == ".")
    //val errorCodeBeforeCaret = code.take(pos).reverse.takeWhile(identChar).reverse
    val errorCode = code.drop(pos).takeWhile(identChar)

    println(
      blue(lineNo.toString ++ ": ")
      ++
      code.take(pos)// - errorCodeBeforeCaret.size)
      /*++
      underline(blue(errorCodeBeforeCaret))*/
      ++
      background.red(errorCode)
      ++
      code.drop(pos + errorCode.size)
    )

    if(lineNo+1 <= source.size)
      println( blue((lineNo+1).toString ++ ": ") ++ source(lineNo+1) )
    if(lineNo+2 <= source.size)
      println( blue((lineNo+2).toString ++ ": ") ++ source(lineNo+2) )

    //println((" "*lineNo.size) ++ "  " ++ caretPrefix + bold(magenta("^")) )

    /*
      lazy val nonIdentChars = Seq(
        '\u0020', '\u0009', '\u000D', '\u000A', '(', ')', '[', ']', '{', '}', '`', ''', '"', '.', ';', ','
      ).map(_.toString).map(Regex.quote).mkString("|")
    }

    */
  }

  import scala.util.matching.Regex

  case class MatchClean(pattern: String){
    def unapplySeq(s: String) = {
      val uncolored = ansi.strip(s)
      val cleaned = uncolored.substring(uncolored.indexOf(" ")+1)
      //println(cleaned)
      val res = pattern.r.unapplySeq(cleaned)
      //println(res)
      res
    }
  }


  lazy val fileAndLine = "(/.*):([0-8]*): "
  lazy val NotEnoughArguments = MatchClean(fileAndLine + "not enough arguments for method ([^ ]*): (.*)\\..*")
  lazy val FileLineMsg = MatchClean(fileAndLine ++ "(.*)")
  lazy val Found = MatchClean(" found   : (.*)")
       //"    \(which expands to\)  (.*),"
  lazy val Required = MatchClean(" required: (.*)")
  lazy val Anything = MatchClean("(.*)")
  lazy val Caret = MatchClean("(.*)\\^")

  def process(input: Stream[Char]): Stream[Char] = {
    //println("...")
    //println("processing lines")
    object TakeLine{
      def unapply(s: Stream[Char]) = Option{
        val str = s.takeWhile(_ != '\n').mkString
        //println("str: "+str+" |")
        (str, s.drop(str.size+1))
      }
    }
    object TakeBeforeCaret{
      def unapply(s: Stream[Char]) = {
        val str = s.takeWhile(!Seq('^','\n').contains(_)).mkString
        if(s.drop(str.size).headOption.forall(_ == '\n'))
          None
        else Option(
          (str, s.drop(str.size))
        )
      }
    }

    input match {
      /*case NotEnoughArguments(_,_,_,_) =>
        //println("1")
        input.tail match {
          case 
            Anything(message)
            #:: Anything(code)
            #:: Caret(caretPrefix)
            #:: tail
            =>
            tail
        }*/
      case TakeLine(
        FileLineMsg(file, lineNoStr, msg),
        foo
      )
       =>
        //println("2")
        println(("_") * 80)
        println("")
        println(formatMsgAndLine(msg, file, lineNoStr))
    //println(";;;")
    
        foo match {
          /*case
            TakeLine(
              Found(found),
              TakeLine(
                Anything(_),
                TakeLine(
                  Required(required),
                  TakeLine(
                    Anything(code),
                TakeBeforeCaret(
                    caretPrefix,
                    tail
            )
            )))) =>
            println(":::")
            foo*/
          case
            TakeLine(
              Found(found),
              TakeLine(
                Required(required),
                TakeLine(
                  Anything(code),
                  TakeBeforeCaret(
                    caretPrefix,
                    tail
            ))))
            =>
            /*println((
              file, lineNoStr, found, required, code, caretPrefix
            ))*/
            formatFoundRequired(
              found, required
            )
            formatCode(
              file, lineNoStr, code, caretPrefix
            )
            tail.drop(1)

          case
            TakeLine(
              Anything(code),
                  TakeBeforeCaret(
                    caretPrefix,
                    tail
            ))
            =>
            /*println((
              file, lineNoStr, found, required, code, caretPrefix
            ))*/
            formatCode(
              file, lineNoStr, code, caretPrefix
            )
            tail.drop(1)

          case
            TakeLine(
              Anything(msg2),
                TakeLine(
                  Anything(code),
                  TakeBeforeCaret(
                    caretPrefix,
                    tail
            )))
            =>
            /*println((
              file, lineNoStr, found, required, code, caretPrefix
            ))*/
            println(msg2)
            formatCode(
              file, lineNoStr, code, caretPrefix
            )
            tail.drop(1)

          case
            TakeLine(
              Found(found),
              TakeLine(
                Anything(_),
                TakeLine(
                  Required(required),
                  TakeLine(
                    Anything(code),
                    TakeBeforeCaret(
                      caretPrefix,
                      tail
            )))))
            =>
            /*println((
              file, lineNoStr, found, required, code, caretPrefix
            ))*/
            formatFoundRequired(
              found, required
            )
            formatCode(
              file, lineNoStr, code, caretPrefix
            )
            tail.drop(1)

          case _ => foo
        }
      case _ => input
    }
  }
}
