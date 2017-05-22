package pretty

import java.io.{ BufferedReader, File, FileInputStream, InputStreamReader }
import java.util.Scanner

import org.json4s._
import org.json4s.native.JsonMethods._

import scala.annotation.tailrec
import sys.process._

/**
  * Read a JSON report from Stainless (expected as the only argument),
  * and produce an HTML representation (on the standard output stream).
  *
  * Require GNU source-highlight on the $PATH.
  */
object PrettyReporter {
  val DEBUG = false

  def main(args: Array[String]): Unit = {
    if (args.length != 1) { help() } // and quit

    val inputFile = args(0)
    debug(s"Reading input JSON file: $inputFile")
    val sc = new Scanner(new File(inputFile))
    val sb = new StringBuilder
    while (sc.hasNextLine) { sb ++= sc.nextLine }

    parseOpt(sb.toString) match {
      case None => exit("Couldn't parse JSON report")
      case Some(report) => details.process(report)
    }
  }

  def splitPadReform(str: String, pad: String, post: String = "", on: String = "\n") = {
    str split on map { pad + _ + post } mkString on
  }

  private def debug(msg: => String) = if (DEBUG) { System.err.println(splitPadReform(msg, "[DEBUG] ")) }

  private def exit(msg: String): Nothing = {
    System.err.println(msg)
    sys.exit(1)
  }

  private def help() = exit("Expected one argument: the path to the JSON report.")

  private object details {
    import jdetails._

    case class VCReport(pos: Position, kind: String, fd: String, status: Status) {
      lazy val color: String = status match {
        case Unknown    => "Khaki"
        case Valid      => "PaleGreen"
        case Invalid(_) => "Tomato"
      }

      lazy val counterexample: Option[Seq[String]] = status match {
        case Invalid(info) => Some({
          val details = if (info.nonEmpty) {
            (info map {
              case (vd, JString(value)) => s"  when $vd is:\n${splitPadReform(value, "    ")}";
              case _ => ???
            })
          } else {
            Seq("empty counter example")
          }
          s"Counterexample for $kind violation in `$fd`:" +: details
        })
        case _ => None
      }
    }

    case class Position(file: String, begin: Coord, end: Coord) { // range is [begin, end[
      override def toString: String = s"$file:$begin->$end"
    }

    case class Coord(line: Int, col: Int) {
      override def toString: String = s"$line:$col"
    }

    abstract class Status
    case object Unknown extends Status
    case object Valid   extends Status
    case class  Invalid(info: List[JField]) extends Status


    /**
      * Entry point for processing:
      * extract the [[VCReport]]s for each files,
      * then process each file one at a time and output the results.
      */
    def process(report: JValue): Unit = {
      val vcrs: Seq[VCReport] = for {
        JArray(subReports) <- report \ "verification"
        sub <- subReports
        pos <- j2Position(sub \ "pos") // filter out Unknown position
        status = j2Status(sub \ "status", sub)
        JString(kind) = sub \ "kind"
        JString(fd) = sub \ "fd"
      } yield VCReport(pos, kind, fd, status)

      val byFile: Map[String, Seq[VCReport]] = vcrs groupBy { _.pos.file }
      val htmls = byFile map { case (file, vcrs) =>
        s"""<p>File: <b>$file</b></p><br />""" +
        process(file, sort(vcrs))
      }

      println(htmls mkString ("<br />" * 3))
    }


    /**
      * Process (sorted) [[VCReport]]s for the given source file.
      * Return an HTML representation of `file` based on `reports`.
      */
    private def process(file: String, reports: Seq[VCReport]): String = {
      debug(
        s"About to process reports with the following [begin, end[ ranges: " +
          (reports map { r => s"[${r.pos.begin}, ${r.pos.end}["} mkString ", ")
      )

      // First, we convert the file to HTML without adding the reports.
      // Drop the four first line (GNU source-highlight adds a comment we don't want).
      val htmlFile = convert2HTML(file)
      val fullHTML = read(htmlFile)
      debug(s"Full HTML before normalisation:\n${fullHTML drop 4 mkString "\n"}")
      val html = fullHTML drop 4 map normaliseHTML
      assert(html.nonEmpty && (html.head startsWith "<pre>") && (html.last endsWith "</pre>"))
      debug(s"HTML after normalisation:\n$html")

      // The "past" contain all the lines that were already processed.
      // The "future" contain what remains to be seen.
      var pastLines = Seq[String]()
      var mostRecentPast = StringBuilder.newBuilder // keep track of what was not yet pushed in `pastLines`
      var mostRecentFuture = "" // keep track of what lies directly ahead of us, on the current line.
      var openTagStack = Seq[String]()
      var closeTagStack = Seq[(Coord, String, Option[Seq[String]])]() // keep track, for the mostRecentFuture, of the closing tags & extra info
      var futurePastLines = Seq[String]() // keep track of extra information that should go into the
                                          // "past" soon (e.g. values for counter example)
      var futureLines = html
      var futureReports = reports

      def hasFuture = futureLines.nonEmpty || mostRecentFuture.nonEmpty || futurePastLines.nonEmpty

      // Keep track of the "real" position: the HTML file contains more data than the original source code.
      var currentLine = 0
      var currentColumn = 0

      // Update the state. To be called when the current line should be entirely consumed.
      // Closing tags are inserted on the current line if need be.
      def consumeMostRecentFutureAndContinue(): Unit = {
        // Place all tag closing on the current line
        while (closeTagStack.headOption match {
          case Some((coord, _, _)) if coord.line == currentLine => true
          case _ => false
        }) { consumeUntilPosition(closeTagStack.head._1) }

        assert(closeTagStack.length == openTagStack.length)

        // Finish consuming the line and close any open tag
        mostRecentPast ++= mostRecentFuture + (closeTagStack map { case (_, tag, _) => tag } mkString "")

        if (futureLines.isEmpty) {
          assert(closeTagStack.isEmpty)
          mostRecentFuture = ""
        } else {
          mostRecentFuture = futureLines.head
          futureLines = futureLines.tail
        }

        pastLines = pastLines ++ (mostRecentPast.toString +: futurePastLines)
        futurePastLines = Seq()
        mostRecentPast = StringBuilder.newBuilder

        // Re-open any temporarily closed tag
        if (openTagStack.nonEmpty) {
          mostRecentPast ++= openTagStack.reverse mkString ""
        }

        currentLine += 1
        currentColumn = 0
      }

      // Update the state after consuming one source (scala) character,
      // taking into account the HTML translation.
      def consumeOneSourceCharacterAndContinue(): Unit = {
        def consumeOne(): Unit = {
          assert(mostRecentFuture.nonEmpty)
          val current = mostRecentFuture.head
          mostRecentFuture = mostRecentFuture.tail
          mostRecentPast += current
        }

        def consumeUntilFound(c: Char): Unit = {
          assert(mostRecentFuture.nonEmpty)
          val (word, rest) = splitAt(mostRecentFuture, c)
          mostRecentFuture = rest
          mostRecentPast ++= word
        }

        // Assuming a "normalised" HTML, i.e. [<t1><t2>...<tn>](c|&...;)[</tn>...</t2></t1>]
        // Note: sometimes we want to go one past the end of the line to insert a closing tag.
        if (mostRecentFuture.nonEmpty) {
          while (mostRecentFuture.head == '<') { consumeUntilFound('>') }
          if (mostRecentFuture.head == '&') { consumeUntilFound(';') }
          else { consumeOne() }
        }
        currentColumn += 1
        while (mostRecentFuture startsWith "</") { consumeUntilFound('>') }

        // Insert all closing tag for the current position.
        while (closeTagStack.headOption match {
          case Some((coord, _, _)) if coord.line == currentLine && coord.col == currentColumn => true
          case _ => false
        }) {
          debug(s"Inserting closing tag at position $currentLine:$currentColumn")
          val (_, closeTag, extraOpt) = closeTagStack.head
          insertTag(closeTag)
          extraOpt foreach insertExtraInfoInFuturePast

          // Pop the stacks
          closeTagStack = closeTagStack.tail
          openTagStack = openTagStack.tail
          assert(closeTagStack.length == openTagStack.length)
        }
      }

      // Go to the given position.
      def consumeUntilPosition(coord: Coord) = {
        assert(
          // Check that the caller doesn't want to go back!
          (coord.line > currentLine) || {
            (coord.line == currentLine) && (coord.col >= currentColumn)}
        )

        debug(s"Looking for position $coord from $currentLine:$currentColumn")
        while (coord.line > currentLine) { consumeMostRecentFutureAndContinue() }
        while (coord.col > currentColumn) { consumeOneSourceCharacterAndContinue() }
        debug(s"Now at position $currentLine:$currentColumn")
      }


      // Add some content to the past, without moving the position.
      def insertTag(content: String): Unit = mostRecentPast ++= content

      def insertExtraInfoInFuturePast(extra: Seq[String]): Unit = {
        // Tag based on the default theme of GNU source-highlight
        val pre = """<i><font color="#9A1900">// """
        val post = """</font></i>"""
        val processed = extra map { str => splitPadReform(escapeHTML(str), pre, post) }
        futurePastLines = futurePastLines ++ processed
      }

      def insertTags(openTag: String, closeTag: String, extraOpt: Option[Seq[String]], closePos: Coord): Unit = {
        insertTag(openTag)

        // Register open tag, in case it's a multi-line position
        openTagStack = openTag +: openTagStack

        // Register close tag (for a future position)
        assert(closePos.line > currentLine || {
          (closePos.line == currentLine) && (closePos.col > currentColumn)
        })
        debug(s"Registering closing tag for $closePos from $currentLine:$currentColumn")
        closeTagStack = (closePos, closeTag, extraOpt) +: closeTagStack

        assert(closeTagStack.length == openTagStack.length)
      }


      // Process each report one after the other.
      // Each time, we look for the character in the "future" HTML that correspond
      // to the report start position.
      while (futureReports.nonEmpty) {
        val currentReport = futureReports.head
        futureReports = futureReports.tail

        debug(s"Current Report Position: ${currentReport.pos}")

        // Go to opening position & insert opening tag + some additional info for counterexamples.
        // Register closing tag (because tags can be nested)
        val targetCol = currentReport.pos.begin.col
        if (targetCol == 0) {
          /* we go nowhere */
        } else {
          /* we go right before the position */
          val target = currentReport.pos.begin.copy(col = targetCol - 1)
          consumeUntilPosition(target)
        }
        val color = currentReport.color
        val openTag = s"""<!-- open pos = ${currentReport.pos}--><span style="background-color: $color">"""
        val closeTag = s"""</span><!-- close pos = ${currentReport.pos}-->"""
        val closeTargetCol = currentReport.pos.end.col
        val closePos = currentReport.pos.end.copy(col = closeTargetCol - 1) // make it inclusive
        val extraOpt = currentReport.counterexample
        insertTags(openTag, closeTag, extraOpt, closePos)
      }

      // Add the remaining lines so that the "past" contains all the "future".
      while (hasFuture) { consumeMostRecentFutureAndContinue() }

      assert(pastLines.length >= html.length)

      val pretty = pastLines mkString "\n"
      //debug(s"Pretty output:\n$pretty")

      pretty
    }

    /**
      * Using GNU source-highlight, convert the given scala file into HTML.
      * Return the generated [[File]].
      *
      * NOTE the output file will be deleted upon exit.
      */
    private def convert2HTML(scalaFile: String): File = {
      val cmd = s"source-highlight -s scala -f html -i $scalaFile"
      val output = File.createTempFile(scalaFile, ".html")
      output.deleteOnExit()

      val errBuilder = StringBuilder.newBuilder
      val logger = ProcessLogger(
        fout = { str => exit(s"Unexpected string $str on stdout of process") },
        ferr = { str => errBuilder ++= str }
      )

      // Run the external source highlighter and save the result in a file.
      debug(s"Converting $scalaFile to HTML")
      val status = {
       import scala.language.postfixOps
       cmd #> output !
      }
      val success = (status == 0) && errBuilder.toString.isEmpty

      if (!success) {
        exit(s"""|Converting $scalaFile to HTML failed; exit code: $status.
                 |Sub process reported this:\n${errBuilder.toString}""".stripMargin)
      }

      output
    }

    private def escapeHTML(value: String): String = value // FIXME implement proper HTML escape

    // Split at (include 'c' in first part)
    private def splitAt(str: String, c: Char): (String, String) = {
      val idx = (str indexOf c) + 1
      str splitAt idx
    }

    /**
      * Normalise HTML input: each HTML tag is opened before each character and close right after.
      *
      * Example: <b>bold</b> is mapped to <b>b</b><b>o</b><b>l</b><b>d</b>
      */
    private def normaliseHTML(html: String): String = {
      var future = html
      val builder = StringBuilder.newBuilder
      var openTags = Seq[String]()

      def makeCloseTag(openTag: String): String = {
        assert(openTag.nonEmpty && (openTag startsWith "<") && (openTag endsWith ">"))
        "</" + (openTag drop 1 takeWhile { c => !(Seq('>', ' ') contains c) }) + ">"
      }

      def processTag(): Unit = {
        assert(future.head == '<')
        assert(future contains '>')

        val (tag, rest) = splitAt(future, '>')
        future = rest // drop tag

        if (tag(1) == '/') {
          // process close
          assert(tag == makeCloseTag(openTags.head))
          openTags = openTags.tail
        } else {
          // process open
          openTags = tag +: openTags
        }
      }

      def injectOpens(): Unit = {
        openTags.reverse foreach { tag =>
          builder ++= tag
        }
      }

      def injectCloses(): Unit = {
        openTags foreach { tag =>
          builder ++= makeCloseTag(tag)
        }
      }

      def copyUntil(c: Char): Unit = {
        injectOpens()
        val (word, rest) = splitAt(future, c)
        builder ++= word
        future = rest
        injectCloses()
      }

      def copyOne(): Unit = {
        injectOpens()
        val current = future.head
        builder += current
        future = future.tail
        injectCloses()
      }

      // All tags are expected to be open and closed on the same line.
      // Except for <pre><tt> which is closed on a different line.
      val init = "<pre><tt>"
      if (future startsWith init) {
        future = future drop init.length
        builder ++= init
      }

      val tini = "</tt></pre>"
      val ending = if (future endsWith tini) {
        future = future dropRight tini.length
        tini
      } else ""

      // Process each remaining tag/encoded character/regular character
      while (future.nonEmpty) future.head match {
        case '<' => processTag()
        case '&' => copyUntil(';')
        case _   => copyOne()
      }

      // Path ending
      builder ++= ending

      builder.toString
    }

    /**
      * Read a file and return its content, line by line.
      */
    private def read(file: File): Seq[String] = {
      val br = new BufferedReader(new InputStreamReader(new FileInputStream(file)))
      try {
        val content = Iterator continually br.readLine takeWhile { _ != null }
        content.toList // toList and not toSeq to actually consume the stream
      } finally {
        br.close()
      }
    }


    /**
      * Sorts report according to their positions.
      *
      * It is illegal to have reports from different files here.
      * It is illegal to have reports overlapping each others (they can be nested however).
      */
    private def sort(reports: Seq[VCReport]): Seq[VCReport] = if (reports.isEmpty) { reports } else {
      implicit class CoordOrderOps(val coord: Coord) {
        def <(that: Coord): Boolean = {
          (coord.line < that.line) || {
            (coord.line == that.line) && (coord.col < that.col)
          }
        }

        def <=(that: Coord) = (this < that) || (this.coord == that)
      }

      implicit class PositionOrderOps(val pos: Position) {
        def <(that: Position): Boolean = {
          require(that.file == pos.file)
          (pos.begin < that.begin) || {
            (pos.begin == that.begin) && (that.end < pos.end)
          }
        }
      }

      implicit class VCReportOrderOps(val r: VCReport) {
        def <(that: VCReport): Boolean = this.r.pos < that.pos
      }

      reports sortWith { _ < _ }
    }
  }

  /**
    * Provides facilities to convert JSON objects into
    * [[PrettyReporter.details.Coord]],
    * [[PrettyReporter.details.Position]] and
    * [[PrettyReporter.details.VCReport]]
    */
  private object jdetails {
    import details._

    def j2Coord(value: JValue) = {
      val JInt(line) = value \ "line"
      val JInt(col) = value \ "col"
      Coord(line.intValue, col.intValue)
    }

    def j2Position(value: JValue): Option[Position] = if (value == JString("Unknown")) { None } else {
      val JString(file) = value \ "file"
      val begin = j2Coord(value \ "begin")
      val end = j2Coord(value \ "end")
      Some(Position(file, begin, end))
    }

    def j2Status(hint: JValue, subReport: JValue): Status = hint match {
      case JString("valid") => Valid
      case JString("unknown") | JString("timeout") => Unknown
      case JString("invalid") =>
        val JObject(info) = subReport \ "counterexample"
        Invalid(info)
      case _ => exit(s"Unknown hint for status: $hint")
    }

  }

}
