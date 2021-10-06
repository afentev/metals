package scala.meta.internal.metals.findfiles

import java.io.BufferedReader
import java.io.InputStreamReader
import java.nio.file.Files

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.control.NonFatal

import scala.meta.internal.io.FileIO
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.ScalafmtConfig.PathMatcher.Nio
import scala.meta.internal.metals._
import scala.meta.io.AbsolutePath

import org.eclipse.lsp4j.Location
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.Range

class FindTextInDependencyJars(
    buildTargets: BuildTargets,
    workspace: () => AbsolutePath,
    languageClient: MetalsLanguageClient
)(implicit ec: ExecutionContext) {
  import FindTextInDependencyJars._

  def find(request: FindTextInDependencyJarsRequest): Future[List[Location]] = {
    val req = Request.fromRequest(request)

    def readInclude: Future[Option[String]] =
      paramOrInput(req.options.flatMap(_.include))(
        MetalsInputBoxParams(value = ".conf", prompt = "Enter file mask")
      )

    def readPattern: Future[Option[String]] =
      paramOrInput(Option(req.query.pattern))(
        MetalsInputBoxParams(prompt = "Enter content to search for")
      )

    readInclude.zipWith(readPattern) { (maybeInclude, maybePattern) =>
      maybeInclude
        .zip(maybePattern)
        .map { case (include, pattern) =>
          val allLocations: ArrayBuffer[Location] = new ArrayBuffer[Location]()

          buildTargets.allWorkspaceJars.foreach { classpathEntry =>
            try {
              val jarLocations: List[Location] =
                if (classpathEntry.isFile && classpathEntry.isJar) {
                  visitJar(
                    path = classpathEntry,
                    include = include,
                    exclude = req.options.flatMap(_.exclude),
                    pattern = pattern
                  )
                } else Nil

              allLocations ++= jarLocations
            } catch {
              case NonFatal(e) =>
                scribe.error(
                  s"Failed to find text in dependency files for $classpathEntry",
                  e
                )
            }
          }

          allLocations.toList
        }
        .flatten
        .toList
    }
  }

  private def isSuitableFile(
      path: AbsolutePath,
      include: String,
      exclude: Option[String]
  ): Boolean = {
    path.isFile &&
    Nio(include).matches(path) &&
    exclude.forall(e => !Nio(e).matches(path))
  }

  private def visitJar(
      path: AbsolutePath,
      include: String,
      exclude: Option[String],
      pattern: String
  ): List[Location] = {
    FileIO
      .withJarFileSystem(path, create = false, close = true) { root =>
        FileIO
          .listAllFilesRecursively(root)
          .filter(isSuitableFile(_, include, exclude))
          .flatMap { absPath =>
            val fileRanges: List[Range] = visitFileInsideJar(absPath, pattern)
            if (fileRanges.nonEmpty) {
              val result = absPath.toFileOnDisk(workspace())
              fileRanges
                .map(range => new Location(result.toURI.toString, range))
            } else Nil
          }
      }
      .toList
  }

  private def visitFileInsideJar(
      path: AbsolutePath,
      pattern: String
  ): List[Range] = {
    var reader: BufferedReader = null
    val positions: ArrayBuffer[Int] = new ArrayBuffer[Int]()
    val results: ArrayBuffer[Range] = new ArrayBuffer[Range]()
    val contentLength: Int = pattern.length()

    try {
      reader = new BufferedReader(
        new InputStreamReader(Files.newInputStream(path.toNIO))
      )
      var lineNumber: Int = 0
      var line: String = reader.readLine()
      while (line != null) {
        var occurence = line.indexOf(pattern)
        while (occurence != -1) {
          positions += occurence
          occurence = line.indexOf(pattern, occurence + 1)
        }

        positions.foreach { position =>
          results += new Range(
            new Position(lineNumber, position),
            new Position(lineNumber, position + contentLength)
          )
        }

        positions.clear()
        lineNumber = lineNumber + 1
        line = reader.readLine()
      }
    } finally {
      if (reader != null) reader.close()
    }

    results.toList
  }

  private def paramOrInput(
      param: Option[String]
  )(input: => MetalsInputBoxParams): Future[Option[String]] = {
    param match {
      case Some(value) =>
        Future.successful(Some(value))
      case None =>
        languageClient
          .metalsInputBox(input)
          .asScala
          .map(checkResult)
    }
  }

  private def checkResult(result: MetalsInputBoxResult) = result match {
    case name if !name.cancelled && name.value.nonEmpty =>
      Some(name.value)
    case _ =>
      None
  }
}

object FindTextInDependencyJars {
  // These are just more typesafe wrappers, duplicating the structure of original model
  private case class Request(options: Option[Options], query: TextSearchQuery)
  private object Request {
    def fromRequest(request: FindTextInDependencyJarsRequest): Request = {
      val options = Option(request.options).map { options =>
        Options(
          include = Option(options.include),
          exclude = Option(options.exclude)
        )
      }

      val query = TextSearchQuery(
        pattern = request.query.pattern,
        isRegExp = Option(request.query.isRegExp),
        isCaseSensitive = Option(request.query.isCaseSensitive),
        isWordMatch = Option(request.query.isWordMatch)
      )

      Request(options = options, query = query)
    }
  }

  private case class Options(include: Option[String], exclude: Option[String])
  private case class TextSearchQuery(
      pattern: String,
      isRegExp: Option[Boolean],
      isCaseSensitive: Option[Boolean],
      isWordMatch: Option[Boolean]
  )
}
