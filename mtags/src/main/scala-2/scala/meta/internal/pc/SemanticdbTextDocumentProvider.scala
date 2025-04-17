package scala.meta.internal.pc

import org.eclipse.lsp4j.{Diagnostic, Range}

import java.io.{PrintWriter, StringWriter}
import java.net.URI
import java.nio.file.Paths
import scala.util.Properties
import scala.meta.dialects
import scala.meta.internal.mtags.MD5
import scala.meta.internal.mtags.MtagsEnrichments._
import scala.meta.internal.semanticdb.scalac.SemanticdbConfig
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.AbsolutePath
import scala.meta.parsers.ParseException
import scala.meta.tokenizers.TokenizeException
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.ConsoleReporter

class SemanticdbTextDocumentProvider(
    val compiler: MetalsGlobal,
    semanticdbCompilerOptions: List[String]
) extends WorksheetSemanticdbProvider {
  import compiler._

  def textDocument(
      uri: URI,
      code: String
  ): s.TextDocument = {
    print("SEMANTICDB START")
    val prevR = reporter
    val stringWriterErr = new StringWriter();
    val printWriterErr = new PrintWriter(stringWriterErr, true);
    reporter = new ConsoleReporter(new Settings(s => {
      println("not called for some reason " + s)
      throw new IllegalStateException("or called?")
    }), Console.in, printWriterErr, new PrintWriter(Console.out, true))

    val filePath = AbsolutePath(Paths.get(uri))
    val validCode = removeMagicImports(code, filePath)

    val unit = addCompilationUnit(
      code = validCode,
      filename = uri.toString(),
      cursor = None
    )
    typeCheck(unit)

    import semanticdbOps._
    // This cache is never updated in semanticdb and will contain the old source
    gSourceFileInputCache.remove(unit.source)
    semanticdbOps.config = SemanticdbConfig.parse(
      semanticdbCompilerOptions,
      _ => (),
      compiler.reporter,
      SemanticdbConfig.default
    )

    val explicitDialect = if (filePath.isSbt) {
      Some(dialects.Sbt1)
    } else if (filePath.isMill || filePath.isScalaScript) {
      Some(dialects.Scala213.withAllowToplevelTerms(true))
    } else {
      None
    }
    // we recalculate md5, since there seems to be issue with newlines sometimes
    val document =
      try {
        unit.toTextDocument(explicitDialect).withMd5(MD5.compute(code))
      } catch {
        case _: TokenizeException | _: ParseException =>
          s.TextDocument.defaultInstance
      }

    val u: s.TextDocument = compiler.workspace
      .flatMap { workspacePath =>
        scala.util.Try(workspacePath.relativize(filePath.toNIO)).toOption
      }
      .map { relativeUri =>
        val relativeString =
          if (Properties.isWin) relativeUri.toString().replace("\\", "/")
          else relativeUri.toString()
        document.withUri(relativeString)
      }
      .getOrElse(document)
    printWriterErr.flush()
    stringWriterErr.flush()
    print("SEMANTICDB END")

    val errors = stringWriterErr.toString.split('\n')
    val diagnostics = getDiagnosticsFromErrors(errors)
    pprint.log(diagnostics)
    reporter = prevR
    u.addDiagnostics(diagnostics.map(d => s.Diagnostic(range=Some(s.Range(d.getRange.getStart.getLine, d.getRange.getStart.getCharacter,
      d.getRange.getEnd.getLine, 100)), severity = s.Diagnostic.Severity.ERROR, message = d.getMessage)): _*)
  }

  private def getDiagnosticsFromErrors(errorLines: Array[String]): Seq[Diagnostic] = {
    errorLines.foldLeft(List.empty[Vector[String]]){case (accumulator, element) =>
      if (element.startsWith("file://")) {
        Vector(element) +: accumulator
      } else {
        (element +: accumulator.head) +: accumulator.tail
      }
    }.flatMap(errorsR => {
      val errors = errorsR.reverse
      for {
        head <- errors.headOption.map(_.drop("file://".length))
        splitted = head.split(":", 3)
        filepath <- splitted.headOption
        line1 = splitted.lift(1).map(_.toInt).get
        line0 = line1 - 1
        errorText <- splitted.lift(2)
        uriFilepath = "file://" + filepath

        pointerIndex = errors.indexWhere("""^\s*\^""".r.findPrefixOf(_).isDefined)
        (string, errorTextEnriched) = if (pointerIndex != -1) (errors(pointerIndex), (errorText +: errors.slice(1, pointerIndex + 1)).mkString("\n")) else ("", errorText)
        startIndex = string.indexOf('^').max(0)

        positionStart = new org.eclipse.lsp4j.Position(line0, startIndex)
        positionEnd = new org.eclipse.lsp4j.Position(line0, 100)
        range = new Range(positionStart, positionEnd)
        diagnostic = new Diagnostic(range, errorTextEnriched)
      } yield diagnostic
    }
    )
  }
}
