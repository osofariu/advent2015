package delivery

import org.scalatest.{path, Matchers}
import scala.io.Source

class GridDeliveryTest extends path.FunSpec with Matchers {

  describe("Santa delivers package on a grid") {
    val delivery = GridDelivery

    describe("taking up steps") {

      it("delivers one package by not moving at all") {
        val results = delivery.moves("")
        results.housesVisited shouldEqual 1
        results.presentsDelivered shouldEqual 1
      }

      it("also delivers a package to house to the right") {
        val results = delivery.moves(">")
        results.housesVisited shouldEqual(2)
        results.presentsDelivered.shouldEqual(2)
      }

      it("delivers two packages by going right, then left") {
        val results = delivery.moves("><")
        results.housesVisited shouldEqual(2)
        results.presentsDelivered shouldEqual(3)
      }

      it("delivers packages in a small loop") {
        val results = delivery.moves(">^<v")
        results.housesVisited shouldEqual(4)
        results.presentsDelivered shouldEqual(5)
      }
    }

    describe("processing steps from file") {
      it("reads moves to a string, then counts them") {
        val results = delivery.movesFromFile("3a_input.txt")
        val visited = results.housesVisited
        visited shouldEqual(2572)
      }
    }

    describe("with robo-santa in the mix") {

      it("delivers to three houses in two moves, same axis") {
        val results = delivery.twinMoves("<>")
        results.housesVisited shouldEqual(3)
      }

      it("reads moves the same way, them tracks both santa") {
        val results: DeliveryResults = delivery.twinMovesFromFile("3a_input.txt")
        results.housesVisited shouldEqual(2631)
      }
    }
  }
}
