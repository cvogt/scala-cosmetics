package org.cvogt.cosmetics.test
import org.cvogt.cosmetics._
import org.scalatest._

class Tests extends FunSuite with org.scalactic.TypeCheckedTripleEquals {
  test( "scalac-cosmetics tests" ) {
    import ammonite.ops._

    val checkFiles = ls( cwd / 'neg ).filter( _.toString.endsWith(".check") )//.take(1000)
    
    (checkFiles | read  zip checkFiles)| {
      case (content, name) =>
      println("------")
      println(name)
      (ErrorMessageParser.compileErrorOrPassThrough.parse(content))
    }
    /*| {
      println("------")
      
      x => x
    } | println*/
    
    /*val results = (checkFiles | read | parse)

    //results | (_+"\n\n") | println

    println(results.drop(200).take(5))

    val fails = (results zip checkFiles).filterNot(_._1.isInstanceOf[fastparse.core.Result.Success[_]])
    fails | {
      case (res,file) => 
      println("------")
      println(file)
      println(res)
      println(res.asInstanceOf[fastparse.core.Result.Failure].traced.trace)
      println("")
      println(read(file))
      println("------")

    }

    println(fails.size + " fails of "+checkFiles.size)
    */
  }
 }

/*
 abstract-class-2.scala:11: error: object creation impossible, since method f in trait S2 of type (x: P2.this.p.S1)Int is not defined
(Note that P.this.p.S1 does not match P2.this.S1: their prefixes (i.e. enclosing instances) differ)
  object O2 extends S2 {
         ^
one error found
*/