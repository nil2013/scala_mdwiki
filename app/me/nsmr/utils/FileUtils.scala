package me.nsmr.utils

import java.io.File
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.FileInputStream

object FileUtil {
  val DEFAULT_CODE = "UTF-8"
  def getBufferedReaderForFile(file: File, code: String = DEFAULT_CODE)
    = new BufferedReader(new InputStreamReader(
        new FileInputStream(file), code
       ))
  def getIterator(file: File, code: String = DEFAULT_CODE) = {
    val br = getBufferedReaderForFile(file, code)
    new Iterator[String] {
      var opt = Option(br.readLine)
      def hasNext = opt.nonEmpty
      def next() = {
        val r = opt.orNull; opt = Option(br.readLine); r
      }
    }
  }
  def readFile(file: File, code: String = DEFAULT_CODE) = {
    val sb = new StringBuffer
    sb.append(getIterator(file, code).mkString("\n"))
    sb.toString()
  }
}
