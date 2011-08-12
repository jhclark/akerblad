package akerblad

import collection._
import io._
import math._

object Document {
  val TOTAL = "TTAALL"
}

class Document(conf: Config) {

  // mutable for loading only -- builder?
  val st = new mutable.ArrayBuffer[String]
  val len = new mutable.ArrayBuffer[Int]
  val tokenStats = new MultiSet[String]
  val tokenStatsBySent = new mutable.ArrayBuffer[MultiSet[String]]
  val token2Sent = new mutable.HashMap[String,mutable.Set[Int]]

  def load(file: String, stopwords: Set[String]) = {
    var sentId = 0
    for(line <- Source.fromFile(file).getLines) {
      // TODO: Emulate champollion's faulty whitespace normalization
      val norm = line.trim.replaceAll("""\s+""", " ")
      st += norm
      val toks = norm.split(' ')
      // TODO: Filter stopwords here?
      val leng = toks.view.map(_.length).sum // TODO: Emulate champollion's buggy byte counting?
      len += leng

      val nonstopToks = toks.filter(!stopwords(_))
      for(nonstopTok <- nonstopToks) {
        tokenStats += nonstopTok
        token2Sent.getOrElseUpdate(nonstopTok, new mutable.HashSet) += sentId
      }
      tokenStats.add(Document.TOTAL, nonstopToks.size)
      sentId += 1
    }
  }
  // number of sentences in the document
  def size = st.size
}

class DocPair(conf: Config, val x: Document, val y: Document) {

  val xyRatio = x.size.toFloat / y.size.toFloat
  private val w1_size = xyRatio * x.size * conf.WIN_PER_100 / 100
  private val w2_size = abs(x.size - y.size) * 3 / 4
  val windowSize = min(max(conf.MIN_WIN_SIZE, max(w1_size, w2_size)), conf.MAX_WIN_SIZE).toInt
}
