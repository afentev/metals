package scala.meta.internal.parsing

import org.eclipse.lsp4j.PublishDiagnosticsParams
import org.eclipse.lsp4j.{Diagnostic, Position, Range}

import scala.jdk.CollectionConverters.SeqHasAsJava


object BloopDiagnosticsParser {
  def getDiagnosticsFromErrors(errorLines: Array[String]): Iterable[PublishDiagnosticsParams] = {
    errorLines.foldLeft(Vector.empty[Vector[String]]){case (accumulator, element) =>
      if (element.startsWith("/")) {
        accumulator.prepended(Vector(element))
      } else {
        accumulator.tail.prepended(accumulator.head.prepended(element))
      }
    }.flatMap(errorsR => {
      val errors = errorsR.reverse
      for {
        head <- errors.headOption
        splitted = head.split(":", 3)
        filepath <- splitted.headOption
        line1 <- splitted.lift(1).flatMap(_.toIntOption)
        line0 = line1 - 1
        errorText <- splitted.lift(2)
        uriFilepath = "file://" + filepath

        pointerIndex = errors.indexWhere("""^\s*\^""".r.findPrefixOf(_).isDefined)
        (string, errorTextEnriched) = if (pointerIndex != -1) (errors(pointerIndex), (errorText +: errors.slice(1, pointerIndex)).mkString("\n")) else ("", errorText)
        startIndex = string.indexOf('^').max(0)

        positionStart = new Position(line0, startIndex)
        positionEnd = new Position(line0, 100)
        range = new Range(positionStart, positionEnd)
        diagnostic = new Diagnostic(range, errorTextEnriched)
      } yield (uriFilepath, diagnostic)
    }
    ).groupBy(_._1).map { case (uriFilepath, diagnostics) =>
      new PublishDiagnosticsParams(uriFilepath, diagnostics.map(_._2).toList.asJava)
    }
  }
}
