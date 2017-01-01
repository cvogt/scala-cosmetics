/*
package cbt
import cbt._
import scala.concurrent._
trait PrettyTypeErrors extends BasicBuild{
  override def compile = {
    /*
    private class VoidOutputStream extends OutputStream {
      override def write(b: Int) = ()
      override def write(b: Array[Byte]) = ()
      override def write(b: Array[Byte], off: Int, len: Int) = ()
    }
    val nullStream = new PrintStream(new VoidOutputStream())
    */
    val buffer = new BufferedOutputStream(System.out)
    
    Future{
      org.cvogt.cosmetics.StreamingParser.scan(
        Stream.continually(buffer).map(_.toChar)
      )
    }

    val oldOut = System.out
    try{
      System.setOut(buffer)
      super.compile
    } finally{
      System.setOut(oldOut)
    }

  }
}
*/
