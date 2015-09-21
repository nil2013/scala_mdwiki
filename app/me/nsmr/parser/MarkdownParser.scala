package me.nsmr.parser

import com.tristanhunt.knockoff.ChunkParser
import com.tristanhunt.knockoff.Chunk
import com.tristanhunt.knockoff.IndentedChunk
import com.tristanhunt.knockoff.Discounter
import com.tristanhunt.knockoff.ChunkStreamFactory
import java.io.File
import scala.collection.mutable.ListBuffer
import com.tristanhunt.knockoff.Block
import com.tristanhunt.knockoff.Span
import scala.util.parsing.input.Position
import com.tristanhunt.knockoff.Text
import com.tristanhunt.knockoff.CodeBlock
import com.tristanhunt.knockoff.OrderedList
import com.tristanhunt.knockoff.OrderedItem
import com.tristanhunt.knockoff.UnorderedList
import com.tristanhunt.knockoff.UnorderedItem

class GFChunkParser extends ChunkParser {
  override lazy val indentedChunk: Parser[Chunk] =
    rep1(indentedLine) ^^ (lines => IndentedChunk(foldedString(lines))) |
    { lazy val _triple: Parser[List[Chunk]] = rep(emptyLine)~"```".r ^^ { _ => List() } | (textLine | emptyLine | emptyString)~_triple ^^ { case x~y => x +: y}
      """```""" ~ rep(emptyLine) ~> _triple} ^^ (lines => CodeBlockChunk(foldedString(lines)))

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
trait GFDiscounter extends Discounter with GFChunkStreamFactory

object GFDefaultDiscounter extends GFDiscounter