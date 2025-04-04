package scala.meta.internal.metals

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.meta.internal.implementation.TextDocumentWithPath
import scala.meta.internal.metals.codelenses.CodeLens
import scala.meta.internal.mtags.Semanticdbs
import scala.meta.io.AbsolutePath
import org.eclipse.{lsp4j, lsp4j as l}
import org.eclipse.lsp4j.{Diagnostic, Position, PublishDiagnosticsParams}
import scala. jdk. CollectionConverters._

import scala.meta.internal.metals.clients.language.ConfiguredLanguageClient

final class CodeLensProvider(
    codeLensProviders: List[CodeLens],
    semanticdbs: () => Semanticdbs,
    stacktraceAnalyzer: StacktraceAnalyzer,
)(implicit val ec: ExecutionContext) {
  // code lenses will be refreshed after compilation or when workspace gets indexed
  def findLenses(path: AbsolutePath, languageClient: ConfiguredLanguageClient): Future[Seq[l.CodeLens]] = {
    if (stacktraceAnalyzer.isStackTraceFile(path)) {
      Future(stacktraceAnalyzer.stacktraceLenses(path))
    } else {
      val enabledCodelenses = codeLensProviders.filter(_.isEnabled)
      val semanticdbCodeLenses = semanticdbs()
        .textDocument(path)
        .documentIncludingStale
        .map { textDocument =>
          languageClient.publishDiagnostics(new PublishDiagnosticsParams(path.toString(), textDocument.diagnostics.map( d => {
            val range = d.range.get
            new Diagnostic(new lsp4j.Range(new Position(range.startLine, range.startCharacter), new Position(range.endLine, range.endCharacter)), d.message)
          }
          ).asJava))
          val doc = TextDocumentWithPath(textDocument, path)
          enabledCodelenses.map(_.codeLenses(doc))
        }
        .getOrElse(Seq.empty)
      val otherCodeLenses = enabledCodelenses.map(_.codeLenses(path))
      Future.sequence(semanticdbCodeLenses ++ otherCodeLenses).map(_.flatten)
    }
  }
}
