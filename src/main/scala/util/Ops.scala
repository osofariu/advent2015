package util

object Ops {
  def makeIndexPairs(maxLen: Int, nextIndex : Int = 1) = {
    for (
      i ← 0 until Math.max(0, maxLen - nextIndex)
    ) yield (i, i + nextIndex)
  }

  def makeCharPairs(str: String, nextIndex: Int = 1) =
    makeIndexPairs(str.length, nextIndex)
  .map(indexes ⇒ (str.charAt(indexes._1), str.charAt(indexes._2)))
}
