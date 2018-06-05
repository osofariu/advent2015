package tsales

import org.scalatest.{path, Matchers}

class TravelSalesTest extends path.FunSpec with Matchers {

  describe("parse distance between locations"){

    it("between two cities") {
      val tSales = new TravelSales()
      tSales.parseDistance("Faerun to Tristram = 65")
      tSales.distance("Faerun", "Tristram") shouldEqual 65
      tSales.distance("Tristram", "Faerun") shouldEqual 65
    }

    it("from a file figure out travel options") {
      val tSales = new TravelSales("input/9_input.txt")
      tSales
        .routeDistance(List("Faerun", "Tristram", "Tambi",
                       "Norrath", "Snowdin", "Straylight",
                       "AlphaCentauri", "Arbre")) shouldEqual(458)
    }
  }

  describe("calculate possible routes") {

    it("given all routes, find minimum distance") {
      val tSales = new TravelSales("input/9_input.txt")
      tSales.shortestRoute shouldBe (117,List("Faerun", "AlphaCentauri", "Tambi",
                                              "Snowdin", "Norrath", "Tristram",
                                              "Arbre", "Straylight"))
    }

    it("given all routes, find maximum distance") {
      val tSales = new TravelSales("input/9_input.txt")
      tSales.longestRoute shouldBe (909,List("Snowdin", "Tristram", "Straylight",
                                             "AlphaCentauri", "Norrath", "Faerun",
                                             "Arbre", "Tambi")) 
    }
  }
}
