package barneshut

import barneshut.FloatOps._
import org.scalatest.FunSuite

import scala.collection._

/**
  * Created by Campbell on 2016/08/26.
  */
class SimulatorTest extends FunSuite {
  val model = new SimulationModel
  val simulator = new Simulator(model.taskSupport, model.timeStats)

  test("testUpdateBoundaries") {
    val b1 = new Boundaries
    b1.minX = 5
    b1.maxX = 10
    b1.minY = 7
    b1.maxY = 14

    simulator.updateBoundaries(b1, new Body(3, 6, 7, 0, 0))
    assert(b1.minX === 5)
    assert(b1.maxX === 10)
    assert(b1.minY === 7)
    assert(b1.maxY === 14)

    simulator.updateBoundaries(b1, new Body(3, 9, 17, 0, 0))
    assert(b1.minX === 5)
    assert(b1.maxX === 10)
    assert(b1.minY === 7)
    assert(b1.maxY === 17)

    simulator.updateBoundaries(b1, new Body(3, 3, 0, 0, 0))
    assert(b1.minX === 3)
    assert(b1.maxX === 10)
    assert(b1.minY === 0)
    assert(b1.maxY === 17)
  }

  test("testMergeBoundaries") {
    val b1 = new Boundaries
    b1.minX = 5
    b1.maxX = 10
    b1.minY = 7
    b1.maxY = 14

    val b2 = new Boundaries
    b2.minX = 3
    b2.maxX = 6
    b2.minY = 8
    b2.maxY = 16

    val merged = simulator.mergeBoundaries(b1, b2)
    assert(merged.minX === b2.minX)
    assert(merged.maxX === b1.maxX)
    assert(merged.minY === b1.minY)
    assert(merged.maxY === b2.maxY)
  }

  test("testComputeSectorMatrix") {
    val boundaries = new Boundaries
    boundaries.minX = 0
    boundaries.minY = 0
    boundaries.maxX = 10
    boundaries.maxY = 10

    val b1 = new Body(1, 2, 3, 0, 0)

    val sectorMatrix = simulator.computeSectorMatrix(Seq(b1), boundaries)

    assert(SECTOR_PRECISION === sectorMatrix.sectorPrecision)
    assert(boundaries === sectorMatrix.boundaries)
    assert(1.25 === sectorMatrix.sectorSize)
    assert(1 === sectorMatrix(1, 2).size)
    assert(sectorMatrix(1, 2).find(_ == b1).isDefined)

    val bodies = Seq(new Body(1, 0, 0.5f, 0, 0), new Body(2, 7, 8.9f, 0, 0), new Body(3, 1, 1, 0, 0), new Body(4, 12, 2, 0, 0))
    val matrix = simulator.computeSectorMatrix(bodies, boundaries)
    assert(boundaries === matrix.boundaries)
    assert(1.25 === matrix.sectorSize)
    assert(2 === matrix(0, 0).size)
    assert(matrix(0, 0).find(_ == bodies(0)).isDefined)
    assert(matrix(0, 0).find(_ == bodies(2)).isDefined)
    assert(1 === matrix(5, 7).size)
    assert(matrix(5, 7).find(_ == bodies(1)).isDefined)
    assert(1 === matrix(7, 1).size)
    assert(matrix(7, 1).find(_ == bodies(3)).isDefined)
  }

  test("testUpdateBodies") {
    val oldBodies = Seq(new Body(123f, 18f, 26f, 0f, 0f),
      new Body(524.5f, 24.5f, 25.5f, 0f, 0f),
      new Body(245f, 22.4f, 41f, 0f, 0f))

    val quad = Leaf(15f, 30f, 20f, oldBodies.tail)

    val newBodies = simulator.updateBodies(oldBodies, quad)

    assert(newBodies.head.xspeed ~= 12.587037f)
    assert(newBodies.head.yspeed ~= 0.015557117f)
  }

}
