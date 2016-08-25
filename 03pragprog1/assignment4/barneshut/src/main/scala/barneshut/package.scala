import common._
import barneshut.conctrees._

package object barneshut {

  class Boundaries {
    var minX = Float.MaxValue

    var minY = Float.MaxValue

    var maxX = Float.MinValue

    var maxY = Float.MinValue

    def width = maxX - minX

    def height = maxY - minY

    def size = math.max(width, height)

    def centerX = minX + width / 2

    def centerY = minY + height / 2

    override def toString = s"Boundaries($minX, $minY, $maxX, $maxY)"
  }

  sealed abstract class Quad {
    def massX: Float

    def massY: Float

    def mass: Float

    def centerX: Float

    def centerY: Float

    def size: Float

    def total: Int

    def insert(b: Body): Quad
  }

  case class Empty(centerX: Float, centerY: Float, size: Float) extends Quad {
    def massX: Float = centerX
    def massY: Float = centerY
    def mass: Float = 0
    def total: Int = 0
    def insert(b: Body): Quad = Leaf(centerX, centerY, size, Seq(b))
  }

  case class Fork(
    nw: Quad, ne: Quad, sw: Quad, se: Quad
  ) extends Quad {
    val centerX: Float = nw.centerX + (nw.size / 2)
    val centerY: Float = nw.centerY + (nw.size / 2)
    val size: Float = nw.size + ne.size + sw.size + se.size
    val mass: Float = nw.mass + ne.mass + sw.mass + se.mass
    val massX: Float = if (size == 0) centerX else (nw.massX * nw.mass + ne.massX * ne.mass + sw.massX * sw.mass + se.massX * se.mass) / mass
    val massY: Float = if (size == 0) centerY else (nw.massY * nw.mass + ne.massY * ne.mass + sw.massY * sw.mass + se.massY * se.mass) / mass
    val total: Int = nw.total + ne.total + sw.total + se.total

    def insert(b: Body): Fork = {
      if (bodyIsInQuad(nw, b)) Fork(nw.insert(b), ne, sw, se)
      else if (bodyIsInQuad(ne, b)) Fork(nw, ne.insert(b), sw, se)
      else if (bodyIsInQuad(sw, b)) Fork(nw, ne, sw.insert(b), se)
      else if (bodyIsInQuad(se, b)) Fork(nw, ne, sw, se.insert(b))
      else this
    }

    private def bodyIsInQuad(quad: Quad, body: Body) = {
      quad.centerX - quad.size / 2 < body.x && body.x < quad.centerX + quad.size / 2 &&
        quad.centerY - quad.size / 2 < body.y && body.y < quad.centerY + quad.size / 2
    }
  }

  case class Leaf(centerX: Float, centerY: Float, size: Float, bodies: Seq[Body])
  extends Quad {
    val mass = bodies.foldLeft(0f)(_ + _.mass)
    val massX = bodies.foldLeft(0f)((x, b) => x + (b.x * b.mass)) / mass
    val massY = bodies.foldLeft(0f)((y, b) => y + (b.y * b.mass)) / mass
    val total: Int = bodies.size
    def insert(b: Body): Quad = {
      if (size < minimumSize) Leaf(centerX, centerY, size, b +: bodies)
      else {
        val centerOffset = size / 4
        var result = Fork(Empty(centerX - centerOffset, centerY - centerOffset, size / 2), // nw
          Empty(centerX + centerOffset, centerY - centerOffset, size / 2), //ne
          Empty(centerX - centerOffset, centerY + centerOffset, size / 2), // sw
          Empty(centerX + centerOffset, centerY + centerOffset, size / 2) // se
        )
        bodies.foreach[Unit](b => result = result.insert(b))
        result
      }
    }
  }

  def minimumSize = 0.00001f

  def gee: Float = 100.0f

  def delta: Float = 0.01f

  def theta = 0.5f

  def eliminationThreshold = 0.5f

  def force(m1: Float, m2: Float, dist: Float): Float = gee * m1 * m2 / (dist * dist)

  def distance(x0: Float, y0: Float, x1: Float, y1: Float): Float = {
    math.sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)).toFloat
  }

  class Body(val mass: Float, val x: Float, val y: Float, val xspeed: Float, val yspeed: Float) {

    def updated(quad: Quad): Body = {
      var netforcex = 0.0f
      var netforcey = 0.0f

      def addForce(thatMass: Float, thatMassX: Float, thatMassY: Float): Unit = {
        val dist = distance(thatMassX, thatMassY, x, y)
        /* If the distance is smaller than 1f, we enter the realm of close
         * body interactions. Since we do not model them in this simplistic
         * implementation, bodies at extreme proximities get a huge acceleration,
         * and are catapulted from each other's gravitational pull at extreme
         * velocities (something like this:
         * http://en.wikipedia.org/wiki/Interplanetary_spaceflight#Gravitational_slingshot).
         * To decrease the effect of this gravitational slingshot, as a very
         * simple approximation, we ignore gravity at extreme proximities.
         */
        if (dist > 1f) {
          val dforce = force(mass, thatMass, dist)
          val xn = (thatMassX - x) / dist
          val yn = (thatMassY - y) / dist
          val dforcex = dforce * xn
          val dforcey = dforce * yn
          netforcex += dforcex
          netforcey += dforcey
        }
      }

      def traverse(quad: Quad): Unit = (quad: Quad) match {
        case Empty(_, _, _) =>
          // no force
        case Leaf(_, _, _, bodies) =>
          // add force contribution of each body by calling addForce
          bodies.foreach(b => addForce(b.mass, b.x, b.y))
        case Fork(nw, ne, sw, se) =>
          // see if node is far enough from the body,
          // or recursion is needed
          handleQuad(nw)
          handleQuad(ne)
          handleQuad(sw)
          handleQuad(se)
      }

      def handleQuad(quad: Quad) = {
        if (quad.size / distance(x, y, quad.massX, quad.massY) < theta) addForce(quad.mass, quad.massX, quad.massY)
        else traverse(quad)
      }

      traverse(quad)

      val nx = x + xspeed * delta
      val ny = y + yspeed * delta
      val nxspeed = xspeed + netforcex / mass * delta
      val nyspeed = yspeed + netforcey / mass * delta

      new Body(mass, nx, ny, nxspeed, nyspeed)
    }

  }

  val SECTOR_PRECISION = 8

  class SectorMatrix(val boundaries: Boundaries, val sectorPrecision: Int) {
    val sectorSize = boundaries.size / sectorPrecision
    val matrix = new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)
    for (i <- 0 until matrix.length) matrix(i) = new ConcBuffer

    def +=(b: Body): SectorMatrix = {
      if (boundaries.minX < b.x && b.x < boundaries.maxX && boundaries.minY < b.y && b.y < boundaries.maxY) {
        // Body is within the boundaries of the SectorMatrix
        var (x, y) = (0, 0)
        var found = false
        while (x < sectorPrecision && y < sectorPrecision && !found) {
          found = if (x * sectorSize < b.x && b.x < (x + 1) * sectorSize && y * sectorSize < b.y && b.y < (y + 1) * sectorSize) {
            matrix(y * sectorPrecision + x) += b
            true
          } else false
          x = (x + 1) % sectorPrecision
          y = if (x == 0) y + 1 else y
        }
      } else {
        // Body is outside the SectorMatrix.  Find closest body and add there
        var closestIndex = -1
        var closestDistance = Float.MaxValue
        var (x, y) = (0, 0)
        while (x < sectorPrecision && y < sectorPrecision) {
          val centerX = x * sectorSize + (sectorSize / 2)
          val centerY = y * sectorSize + (sectorSize / 2)
          val currentDistance = distance(centerX, centerY, b.x, b.y)
          closestIndex = if (currentDistance < closestDistance) {
            closestDistance = currentDistance
            (y * sectorPrecision) + x
          } else closestIndex
          x = (x + 1) % sectorPrecision
          y = if (x == 0) y + 1 else y
        }
      }
      this
    }

    def apply(x: Int, y: Int) = matrix(y * sectorPrecision + x)

    def combine(that: SectorMatrix): SectorMatrix = {
      val combinedMatrix = new SectorMatrix(boundaries, sectorPrecision)
      for (index <- matrix.indices) combinedMatrix.matrix(index) = matrix(index) combine that.matrix(index)
      combinedMatrix
    }

    def toQuad(parallelism: Int): Quad = {
      def BALANCING_FACTOR = 4
      def quad(x: Int, y: Int, span: Int, achievedParallelism: Int): Quad = {
        if (span == 1) {
          val sectorSize = boundaries.size / sectorPrecision
          val centerX = boundaries.minX + x * sectorSize + sectorSize / 2
          val centerY = boundaries.minY + y * sectorSize + sectorSize / 2
          var emptyQuad: Quad = Empty(centerX, centerY, sectorSize)
          val sectorBodies = this(x, y)
          sectorBodies.foldLeft(emptyQuad)(_ insert _)
        } else {
          val nspan = span / 2
          val nAchievedParallelism = achievedParallelism * 4
          val (nw, ne, sw, se) =
            if (parallelism > 1 && achievedParallelism < parallelism * BALANCING_FACTOR) parallel(
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            ) else (
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            )
          Fork(nw, ne, sw, se)
        }
      }

      quad(0, 0, sectorPrecision, 1)
    }

    override def toString = s"SectorMatrix(#bodies: ${matrix.map(_.size).sum})"
  }

  class TimeStatistics {
    private val timeMap = collection.mutable.Map[String, (Double, Int)]()

    def clear() = timeMap.clear()

    def timed[T](title: String)(body: =>T): T = {
      var res: T = null.asInstanceOf[T]
      val totalTime = /*measure*/ {
        val startTime = System.currentTimeMillis()
        res = body
        (System.currentTimeMillis() - startTime)
      }

      timeMap.get(title) match {
        case Some((total, num)) => timeMap(title) = (total + totalTime, num + 1)
        case None => timeMap(title) = (0.0, 0)
      }

      println(s"$title: ${totalTime} ms; avg: ${timeMap(title)._1 / timeMap(title)._2}")
      res
    }

    override def toString = {
      timeMap map {
        case (k, (total, num)) => k + ": " + (total / num * 100).toInt / 100.0 + " ms"
      } mkString("\n")
    }
  }
}
