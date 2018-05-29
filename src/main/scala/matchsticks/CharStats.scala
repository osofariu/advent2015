package matchsticks

import scala.io.Source

object CharStats {

  def space(str: String) : SpaceStats = {
    val spaceStats = CharEncoder.encode(str)
      .map(c ⇒ c.space)
    aggregateStats(spaceStats)
  }

  def spaceForFile(filename: String) = {
    val spaceStats =
      Source
        .fromFile(filename)
        .getLines
        .map(space)
    aggregateStats(spaceStats.toList)
  }

  def reEncodedStatsForFile(filename: String) = {
    val spaceStats =
      Source
        .fromFile(filename)
        .getLines
        .map(CharEncoder.reencode)
        .map(space)
    aggregateStats(spaceStats.toList)
  }

  private def aggregateStats(stats: Seq[SpaceStats]) : SpaceStats = {
    stats.reduce((ss1, ss2) ⇒
    SpaceStats(code = ss1.code + ss2.code,
             memory = ss1.memory + ss2.memory)
    )}
}
