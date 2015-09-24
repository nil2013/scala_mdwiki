package me.nsmr.parser

import java.io.File
import scala.collection.mutable.ListBuffer
import com.tristanhunt.knockoff._
import scala.util.parsing.input.Position

class GFChunkParser extends ChunkParser {
  import com.tristanhunt.knockoff._

  override lazy val indentedChunk: Parser[Chunk] =
    rep1(indentedLine) ^^ (lines => IndentedChunk(foldedString(lines))) |
    { lazy val _triple: Parser[List[Chunk]] = rep(emptyLine)~"```".r ^^ { _ => List() } | (textLine | emptyLine | emptyString)~_triple ^^ { case x~y => x +: y}
      """```""" ~ rep(emptyLine) ~> _triple} ^^ (lines => CodeBlockChunk(foldedString(lines)))
  override lazy val hardBreakTextLine: Parser[Chunk] =
    """[\t ]*\S[^\n]*[ ]{2}\r?\n""".r ^^ {
      s => TextChunk(s)
    }
  /** A special case where an emphasis marker on a word on a text block doesn't
      make the block a list item. */
  override lazy val leadingStrongTextBlock: Parser[Chunk] =
    """[ ]{0,3}\*\*[^*\n]+\*\*[^\n]*\n?""".r ~ rep(textLine) ^^ {
      case ~(strLine, textSeq) => TextChunk(strLine + foldedString(textSeq))
    }
  override lazy val equalsLine: Parser[Any] = """=+\r?\n""".r
  override lazy val dashesLine: Parser[Any] = """-+\r?\n""".r
  override lazy val horizontalRule: Parser[Chunk] = {
    val hrChar = """*\-_="""
    s"""[ ]{0,3}[$hrChar][\t ]?[$hrChar][\t ]?[$hrChar][\t $hrChar]*\r?\n""".r ^^ {
      s => HorizontalRuleChunk
    }
  }
  override lazy val linkDefinition: Parser[Chunk] =
    linkIDAndURL ~ opt(linkTitle) <~ """[ ]*(\r?\n)?""".r ^^ {
      case ~(idAndURL, titleOpt) =>
        LinkDefinitionChunk(idAndURL._1, idAndURL._2, titleOpt)
    }
  private def linkIDAndURL: Parser[(String, String)] =
    """[ ]{0,3}\[[^\[\]]*\]:[ ]+<?[\w\p{Punct}]+>?""".r ^^ {
      linkString =>
        val linkMatch = """^\[([^\[\]]+)\]:[ ]+<?([\w\p{Punct}]+)>?$""".r
          .findFirstMatchIn(linkString.trim).get;
        (linkMatch.group(1), linkMatch.group(2))
    }

  private def linkTitle: Parser[String] =
    """\s*""".r ~> """["'(].*["')]""".r ^^ (// " <- My TextMate bundle fails here
      str => str.substring(1, str.length - 1))
  private def foldedString(texts: List[Chunk]): String =
      ("" /: texts)((current, text) => current + text.content)
}
case class CodeBlockChunk(val content: String) extends Chunk {

  def appendNewBlock(list: ListBuffer[Block],
                     remaining: List[(Chunk, Seq[Span], Position)],
                     spans: Seq[Span], position: Position,
                     discounter: Discounter) {
    if (list.isEmpty) {
      spans.head match {
        case text: Text => list += CodeBlock(text, position)
      }
    } else {
      list.last match {
        case OrderedList(items) =>
          val blocks = discounter.knockoff(content)
          val li = OrderedItem(items.last.children ++ blocks, items.last.position)
          list.update(list.length - 1, OrderedList(items.take(items.length - 1) ++ List(li)))

        case UnorderedList(items) =>
          val blocks = discounter.knockoff(content)
          val li =
            UnorderedItem(items.last.children ++ blocks, items.last.position)
          list.update(list.length - 1, UnorderedList(items.take(items.length - 1) ++ List(li)))

        case _ =>
          spans.head match {
            case text: Text => list += CodeBlock(text, position)
          }
      }
    }
  }
}

trait GFChunkStreamFactory  extends ChunkStreamFactory {
  /** Overridable factory method. */
  override def newChunkParser: ChunkParser = new GFChunkParser
}
trait GFDiscounter extends Discounter with GFChunkStreamFactory {
  import scala.util.parsing.input.CharSequenceReader
  import com.tristanhunt.knockoff._
  override def createSpanConverter(linkDefinitions: Seq[LinkDefinitionChunk]): SpanConverter =
    new GFSpanConverter(linkDefinitions)
}

object GFDefaultDiscounter extends GFDiscounter

class GFSpanConverter(definitions: Seq[LinkDefinitionChunk]) extends SpanConverter(definitions) {
  override def apply(chunk: Chunk): Seq[Span] = {
    chunk match {
      case IndentedChunk(content) => List(new Text(content))
      case CodeBlockChunk(content) => List(new Text(content))
      case _ => convert(chunk.content, Nil)
    }
  }
}