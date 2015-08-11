import scala.meta
import scala.meta.{Type => _,Term => _,Name => _,_}
import scala.meta.internal.ast._, Term.{Name => TermName, Super}, Type.{Name => TypeName, _}, Name.{Anonymous, Indeterminate}
import scala.meta.dialects.Scala211; 
object Main extends App{
  import java.io.{BufferedReader, InputStreamReader}
  val in = new BufferedReader(new InputStreamReader(System.in))
  import ansi._
  import ansi.foreground._
  
  val baseColors = Seq(
    red _,
    magenta _,
    green _,
    cyan _,
    blue _,
    yellow _
  )

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
              case c@'\n' if s.tail.startsWith(`[error] `) =>
                process(s.tail)
              case _ => s.tail
            }
        }
      }
    }
  }
  scan(input)

  //process(input)

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
  lazy val TypeMismatch = MatchClean(fileAndLine ++ "type mismatch;.*")
  lazy val CompileError = MatchClean(fileAndLine ++ "type mismatch;.*")
  lazy val Found = MatchClean(" found   : (.*)")
       //"    \(which expands to\)  (.*),"
  lazy val Required = MatchClean(" required: (.*)")
  lazy val Anything = MatchClean("(.*)")
  lazy val Caret = MatchClean("(.*)\\^")

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
      underline(red(msg))
      ++
      " in "
      ++
      projectPath.dropRight(1).mkString("/")
      ++
      "/"
      ++
      blue(projectPath.last)
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
    )
  }

  def formatMismatch(file: String, lineNoStr: String, found: String, required: String, code: String, caretPrefix: String) = {
    val lineNo = lineNoStr.toInt

    // FIXME: beginning of file
    val source = scala.io.Source.fromFile(file).getLines.drop(lineNo-3).take(5).toVector

    def log(s: String) = println(/*(" " * ansi.strip(`[error] `).size) ++ */s)
    
    val uncolored = ansi.strip(caretPrefix)
    val cleaned = uncolored.substring(uncolored.indexOf(" ")+1)
    val pos = cleaned.size
    val errorCode = {
      code.drop(pos).head.toString ++ code.drop(pos+1).takeWhile(c => c.isLetter || c.isDigit)
    }
    println("")

    log( blue((lineNo-2).toString ++ ": ") ++ source(0) )
    log( blue((lineNo-1).toString ++ ": ") ++ source(1) )

    log(
      blue(lineNoStr ++ ": ")
      ++
      code.take(pos)
      ++
      underline(red(errorCode))
      ++
      code.drop(pos + errorCode.size)
    )

    log( blue((lineNo+1).toString ++ ": ") ++ source(3) )
    log( blue((lineNo+2).toString ++ ": ") ++ source(4) )

    //log((" "*lineNo.size) ++ "  " ++ caretPrefix + bold(magenta("^")) )

    /*
      lazy val nonIdentChars = Seq(
        '\u0020', '\u0009', '\u000D', '\u000A', '(', ')', '[', ']', '{', '}', '`', ''', '"', '.', ';', ','
      ).map(_.toString).map(Regex.quote).mkString("|")
    }

    */

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
      def imports: Seq[String] = {
        (
          (
            if(qualifiedName.size > 1) Seq(qualifiedName.dropRight(1).mkString(".") ++ "." ++ red(qualifiedName.last))
            else Seq()
          ) ++ children.flatMap(_.imports)
        )
      }
    }
    def parse(s: String) = parseName(s.parse[meta.Type])
    def parseName(t: scala.meta.Tree): MyType = t match {
      case Type.Name(name) => MyType(name :: Nil,Seq())
      case Term.Name(name) => MyType(name :: Nil,Seq())
      case Term.Select(prefix, Term.Name(name)) =>
        val p = parseName(prefix)
        p.copy(qualifiedName = p.qualifiedName :+ name)
      case Type.Select(prefix, Type.Name(name)) =>
        val p = parseName(prefix)
        p.copy(qualifiedName = p.qualifiedName :+ name)
      case Type.Apply(name, names) => parseName(name).copy(children=names.map(parseName))
      case other => MyType("other: "+other.show[Structure]::Nil, Seq())
    }

    log("")
    try{
      val ConstantTypeSuffix = "\\([0-9]+\\)".r
      val requiredParsed = parse( ConstantTypeSuffix.replaceAllIn(required,"") )
      val foundParsed = parse( ConstantTypeSuffix.replaceAllIn(found,"") )
      log("required" ++ ": " ++ green(requiredParsed.prettyPrint) )
      log("found" ++ "   : " ++ red(foundParsed.prettyPrint) )
      val is = (requiredParsed.imports ++ foundParsed.imports).distinct.sorted.map("import " ++ _)
      if(is.nonEmpty){
        log("")
        is.foreach(log)
      }
    } catch {
      case e: scala.meta.ParseException =>
        log("required" ++ ": " ++ green(required) )
        log("found" ++ "   : " ++ red(found) )
    }    
  }

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
      def unapply(s: Stream[Char]) = Option{
        val str = s.takeWhile(_ != '^').mkString
        (str, s.drop(str.size))
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
        TypeMismatch(file, lineNoStr),
        foo
      )
       =>
        //println("2")
        println(("_") * 80)
        println("")
        println(formatMsgAndLine("type mismatch", file, lineNoStr))
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
            formatMismatch(
              file, lineNoStr, found, required, code, caretPrefix
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
            formatMismatch(
              file, lineNoStr, found, required, code, caretPrefix
            )
            tail.drop(1)

          case _ => foo
        }
      case _ => input
    }
  }
}

object ansi{
  def apply(codes: Int*)(s: String) = codes.map(code).mkString ++ reset(0)(s)
  def applyForeground(codes: Int*)(s: String) = codes.map(code).mkString ++ resetForeground(s)
  def applyBackground(codes: Int*)(s: String) = codes.map(code).mkString ++ resetBackground(s)
  
  private def reset(i: Int)(s: String) = s ++ (if(s endsWith code(i)) "" else code(i))
  
  /** strip ansi color codes */
  def strip(s: String) = s.replaceAll("\u001B\\[[;\\d]*m", "")

  ///** Reset / Normal  all attributes off */
  //def reset(s: String) = apply(0)(s)
  
  /** Bold or increased intensity  */
  def bold(s: String) = code(1) ++ reset(0)(s)
  
  /** Faint (decreased intensity) Not widely supported. */
  def faint(s: String) = apply(2)(s)
  
  /** Italic: on  Not widely supported. Sometimes treated as inverse. */
  def italic(s: String) = apply(3)(s)
  
  /** Underline: Single  */
  def underline(s: String) = code(4) ++ resetUnderline(s)
  
  /** Blink: Slow less than 150 per minute */
  def blinkSlow(s: String) = apply(5)(s)
  
  /** Blink: Rapid  MS-DOS ANSI.SYS; 150+ per minute; not widely supported */
  def blinkRapid(s: String) = apply(6)(s)
  
  /** Image: Negative inverse or reverse; swap foreground and background (reverse video) */
  def image(s: String) = apply(7)(s)
  
  /** Conceal Not widely supported. */
  def concealed(s: String) = apply(8)(s)
  
  /** Crossed-out Characters legible, but marked for deletion. Not widely supported. */
  def crossed(s: String) = apply(9)(s)
  
  /** Primary(default) font  */
  def primary(s: String) = apply(10)(s)
  
  /** alternative font 1 */
  def font1(s: String) = apply(11)(s)
  
  /** alternative font 2 */
  def font2(s: String) = apply(12)(s)
  
  /** alternative font 3 */
  def font3(s: String) = apply(13)(s)
  
  /** alternative font 4 */
  def font4(s: String) = apply(14)(s)
  
  /** alternative font 5 */
  def font5(s: String) = apply(15)(s)
  
  /** alternative font 6 */
  def font6(s: String) = apply(16)(s)
  
  /** alternative font 7 */
  def font7(s: String) = apply(17)(s)
  
  /** alternative font 8 */
  def font8(s: String) = apply(18)(s)
  
  /** alternative font 9 */
  def font9(s: String) = apply(19)(s)
  
  /** Fraktur hardly ever supported */
  def fraktur(s: String) = apply(20)(s)
  
  /** Bold: off or Underline: Double  Bold off not widely supported; double underline hardly ever supported. */
  private def resetBold(s: String) = reset(21)(s)
  
  /** Normal color or intensity Neither bold nor faint */
  def normal(s: String) = apply(22)(s)
  
  /** Not italic, not Fraktur  */
  def not(s: String) = apply(23)(s)
  
  /** Underline: None Not singly or doubly underlined */
  private def resetUnderline(s: String) = reset(24)(s)
  
  /* 
  /** Blink: off   */
  def blinkOff(s: String) = apply(25)(s)
  
  /** Reserved   */
  def reserved(s: String) = apply(26)(s)
  
  /** Image: Positive  */
  def image(s: String) = apply(27)(s)
  
  /** Reveal  conceal off */
  def reveal(s: String) = apply(28)(s)
  
  /** Not crossed out  */
  def not(s: String) = apply(29)(s)
  */
  
  object foreground{
    private val offset = 30
    /** Set text color */
    def black(s: String) = applyForeground(offset+0)(s)
    /** Set text color */
    def red(s: String) = applyForeground(offset+1)(s)
    /** Set text color */
    def green(s: String) = applyForeground(offset+2)(s)
    /** Set text color */
    def yellow(s: String) = applyForeground(offset+3)(s)
    /** Set text color */
    def blue(s: String) = applyForeground(offset+4)(s)
    /** Set text color */
    def magenta(s: String) = applyForeground(offset+5)(s)
    /** Set text color */
    def cyan(s: String) = applyForeground(offset+6)(s)
    /** Set text color */
    def white(s: String) = applyForeground(offset+7)(s)
  }

  /*
  /** Reserved for extended set foreground color  typical supported next arguments are 5;x where x is color index (0..255) or 2;r;g;b where r,g,b are red, green and blue color channels (out of 255) */
  def reserved(s: String) = apply(38)(s)
  */
  
  /** Default text color (foreground) implementation defined (according to standard) */
  private def resetForeground(s: String) = reset(39)(s)
  
  object background{
    val offset = 40
    /** Set background color */
    def black(s: String) = applyBackground(offset+0)(s)
    /** Set background color */
    def red(s: String) = applyBackground(offset+1)(s)
    /** Set background color */
    def green(s: String) = applyBackground(offset+2)(s)
    /** Set background color */
    def yellow(s: String) = applyBackground(offset+3)(s)
    /** Set background color */
    def blue(s: String) = applyBackground(offset+4)(s)
    /** Set background color */
    def magenta(s: String) = applyBackground(offset+5)(s)
    /** Set background color */
    def cyan(s: String) = applyBackground(offset+6)(s)
    /** Set background color */
    def white(s: String) = applyBackground(offset+7)(s)
  }
       
  /*
  /** Reserved for extended set background color  typical supported next arguments are 5;x where x is color index (0..255) or 2;r;g;b where r,g,b are red, green and blue color channels (out of 255) */
  def reserved(s: String) = apply(48)(s)
  */

  /** Default background color  implementation defined (according to standard) */
  private def resetBackground(s: String) = reset(49)(s)
  
  /*
  /** Reserved   */
  def reserved(s: String) = apply(50)(s)
  
  /** Framed   */
  def framed(s: String) = apply(51)(s)
  */

  /** Encircled  */
  def encircled(s: String) = apply(52)(s)
  
  /** Overlined  */
  def overlined(s: String) = apply(53)(s)
  
  /*
  /** Not framed or encircled  */
  def not(s: String) = apply(54)(s)
  
  /** Not overlined  */
  def not(s: String) = apply(55)(s)
  
  /** Reserved   */
  def reserved(s: String) = apply(56–59)(s)

  /** ideogram underline or right side line hardly ever supported */
  def ideogram(s: String) = apply(60)(s)
  
  /** ideogram double underline or double line on the right side  hardly ever supported */
  def ideogram(s: String) = apply(61)(s)
  
  /** ideogram overline or left side line hardly ever supported */
  def ideogram(s: String) = apply(62)(s)
  
  /** ideogram double overline or double line on the left side  hardly ever supported */
  def ideogram(s: String) = apply(63)(s)
  
  /** ideogram stress marking hardly ever supported */
  def ideogram(s: String) = apply(64)(s)
  
  /** ideogram attributes off hardly ever supported, reset the effects of all of 60–64 */
  def ideogram(s: String) = apply(65)(s)
  */
  
  /*
  /** Set foreground text color, high intensity aixterm (not in standard) */
  def set(s: String) = apply(90–97)(s)
  
  /** Set background color, high intensity  aixterm (not in standard) */
  def set(s: String) = apply(100–107)(s)
  */


  private def code(i: Int) = s"[${i}m"
  
  /*
  def ansi = apply _
  def apply(
    color: Color,
    subject = Foreground,


  )*/
}
