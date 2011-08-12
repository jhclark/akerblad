package akerblad

import math._
import collection._

import akerblad.Types._

class Aligner(conf: Config, dp: DocPair) {

  val pathX = Array.fill(dp.x.size+1, dp.y.size+1)(0)
  val pathY = Array.fill(dp.x.size+1, dp.y.size+1)(0)
  val score = Array.fill(dp.x.size+1, dp.y.size+1)(Double.NegativeInfinity) // 2d array

  def set(x: Int, y: Int, d: Double) = { score(x)(y) = d }
  def get(x: Int, y: Int) = score(x)(y) match {
    case s if(s.isNegInfinity) => 0
    case s @ _ => s
  }

  // returns a list of pairs, each of which denotes a correspondence between x sentences and y sentences
  def align(): Alignments = {
    for(j <- 0 until dp.y.size + 1) {
      val center = (j * dp.xyRatio).toInt
      val windowStart = max(center - dp.windowSize, 0)
      val windowEnd = min(center + dp.windowSize, dp.x.size)
      for(i <- windowStart until windowEnd + 1) {
        // TODO: Do we really need to use Config.MINS or is 0.0 sufficient?
        // similarity matrix of s(i,j) through s(i-4, j-1), etc. (see Section 4.2 of Champollion paper)
        // s(0,0) is s(i,j) and s(4,1) is s(i-4,j-1)
        val s = Array.fill(5,5)(0.0) // TODO: Reconcile 13 single variables with this matrix...
        
        // valid (supported sentences in an alignment) offsets to subtract from i,j
        // 0-1 and 1-0 means not aligning one sentence with anything
        val validPairs = conf.disallow3 match {
          case true => List( (1,0), (0,1), (1,1), (2,1), (1,2) )
          case false => List( (1,0), (0,1), (1,1), (2,1), (1,2), (1,3), (3,1), (1,4), (4,1) )
        }
        var bestM = (0,0)
        var smax = 0.0
        for( (mi, mj) <- validPairs) {
          if(i-mi < 0 || j-mj < 0) {
            s(mi)(mj) = 0.0 // invalid (Config.MINS)
          } else {
            val xSents: Seq[Int] = for(xid <- i-mi until i) yield xid
            val ySents: Seq[Int] = for(yid <- j-mj until j) yield yid
            s(mi)(mj) = get(i-mi, j-mj) + sim(xSents, ySents)
            if(s(mi)(mj) > smax) {
              smax = s(mi)(mj)
              bestM = (mi, mj)
            }
          }
        }
        set(i, j, smax)
        if(smax > 0.0) {
          // save backpointers to the best alignment
          val(mi, mj) = bestM
          pathX(i)(j) = i - mi
          pathY(i)(j) = j - mj
        }
      }
    }

    // now backtrace the best path through the lattice
    // TODO: For large documents, we might not want to create the full X by Y arrays for the paths
    var n = 0
    var i = dp.x.size
    var j = dp.y.size
    val rAlign = new mutable.ArrayBuffer[(Seq[Int], Seq[Int], Double)] // aka Alignments
    while(i > 0 || j > 0) {
      val mi = i - pathX(i)(j)
      val mj = j - pathY(i)(j)
      
      val xSents: Seq[Int] = for(xid <- i-mi until i) yield xid
      val ySents: Seq[Int] = for(yid <- j-mj until j) yield yid
      val score = get(i, j)

      rAlign.append( (xSents, ySents, score) )

      i = pathX(i)(j)
      j = pathY(i)(j)
      n += 1
    }
    rAlign
  }

  // the sim() function, defined in Section 4.1 of the Champollion paper
  def sim(xs: Seq[Int], ys: Seq[Int]): Double = {

    if(xs.size == 0 || ys.size == 0)
      return -0.1 // TODO: Move to config? XXX: Breaks 0.0 as lowest value...

    // eSentences, fSentences are concatenated strings! gross!

    // get lexical similarity scores
    val scoreLex = simLex(xs, ys)

    // sum sentence lengths
    val xLen = xs.map(sentId => dp.x.len(sentId)).sum
    val yLen = ys.map(sentId => dp.y.len(sentId)).sum

    // TODO: Move some of these magic numbers to config?
    val lengthPenalty = {
      if(max(xLen, yLen.toDouble / conf.x2yCharRatio) > 60) {
        log10(6 + 4 * min(xLen * conf.x2yCharRatio, yLen)
              / max(xLen * conf.x2yCharRatio, yLen))
      } else {
        1.0
      }
    }

    // raw lexical score is tempered by a length penalty
    // and a penalty for how many segments are being combined
    scoreLex * lengthPenalty * conf.penalty(xs.size)(ys.size)
  }

  def simLex(xs: Seq[Int], ys: Seq[Int]): Double = {
    // we update these sets as we go along to remove
    // matched words from the counts
    val xTokens = new MultiSet[String]
    for(xid <- xs) xTokens ++= dp.x.tokenStatsBySent(xid)
    val yTokens = new MultiSet[String]
    for(yid <- ys) yTokens ++= dp.y.tokenStatsBySent(yid)

    // score bag of words
    // TODO: Add dictionary plugins someday?
    var score = 0.0
    val xTotalToks = dp.x.tokenStats(Document.TOTAL)
    for( (xToken: String, xTokFreq: Int) <- xTokens) {
      if(yTokens.contains(xToken) && !conf.xStop.contains(xToken)) {
        // attempt to match "cognates"
        score += log((xTotalToks.toDouble / dp.x.tokenStats(xToken).toDouble)
                     * min(xTokFreq, yTokens(xToken)) + 1)
        // TODO: Shouldn't we be subtracting these from the matched counts as well?
      } else {
        // attempt a dictionary lookup
        var foundTranslation = false // TODO: What's this do?
        val translations: Iterable[String] = conf.dict.getOrElse(xToken, Seq.empty)
        for(trans <- translations.takeWhile(dummy => !foundTranslation)) {
          if(yTokens.contains(trans)) {
            val minPairs = min(xTokens(xToken), yTokens(trans))
            if(minPairs > 0) {
              // executed once per xToken (iff we find a match)
              score += conf.scorer.score(dp.x.tokenStats, minPairs, xTotalToks, xToken)
              xTokens.remove(xToken, minPairs)
              yTokens.remove(trans, minPairs)
              foundTranslation = true
            }
          }
        }
      }
    }
    score
  }
}
