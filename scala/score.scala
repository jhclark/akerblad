package akerblad

import collection._

/** Implements the Segment-wide Term Frequency -- Inverse Document-wide Term
 * Frequency Similarity Scoring function as described in Section 4.1 of the LREC
 * Champollion paper. */
class Scorer {
  def score(xTokenStats: MultiSet[String], minPairs: Int, xTotalTokens: Int, xToken: String) = {
    math.log( (xTotalTokens.toDouble / xTokenStats(xToken).toDouble) * minPairs + 1)
  }
}
