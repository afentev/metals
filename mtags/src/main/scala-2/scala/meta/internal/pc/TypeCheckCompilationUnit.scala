package scala.meta.internal.pc

import java.io.{PrintWriter, StringWriter}
import scala.meta.pc.{OffsetParams, VirtualFileParams}
import scala.tools.nsc.{Settings}
import scala.tools.nsc.reporters.{ConsoleReporter, Reporter, StoreReporter}

class TypeCheckCompilationUnit(
                             val compiler: MetalsGlobal,
                             val params: VirtualFileParams
                           ) {
  import compiler._
  val unit: RichCompilationUnit = addCompilationUnit(
    code = params.text(),
    filename = params.uri().toString(),
    cursor = None
  )
  val offset: Int = params match {
    case p: OffsetParams => p.offset()
    case _: VirtualFileParams => 0
  }
  val pos: Position = unit.position(offset)
  lazy val text = unit.source.content
  println("TypeCheckCompilationUnit constructed")

//  reporter = new StoreReporter(new Settings(s => {
//    println("not called for some reason " + s)
//    throw new IllegalStateException("or called?")
//  }))
  val stringWriterErr = new StringWriter();
  val printWriterErr = new PrintWriter(stringWriterErr, true);
  val stringWriter = new StringWriter();
  val printWriter = new PrintWriter(stringWriter, true);
//  reporter = new ConsoleReporter(new Settings(s => {
//        println("not called for some reason " + s)
//        throw new IllegalStateException("or called?")
//      }), Console.in, printWriterErr, printWriter)
//  reporter = Reporter.apply(new Settings(s => {
//    println("not called for some reason " + s)
//    throw new IllegalStateException("or called?")
//  }))
  val prevR = reporter

  val console = new StoreReporter
  reporter = console
  typeCheck(unit)
  reporter.finish()
  printWriter.flush()
  stringWriter.flush()
  pprint.log(stringWriter.toString)
  printWriterErr.flush()
  stringWriterErr.flush()
  pprint.log(stringWriterErr.toString)
  reporter = prevR

  def getInfos: Set[StoreReporter.Info] = {
    println("getInfos called")
//    Set.empty
    console.asInstanceOf[StoreReporter].infos.toSet
  }
}