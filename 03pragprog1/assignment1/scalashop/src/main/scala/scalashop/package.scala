

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    if (radius == 0) src(x, y)
    else {
      var pixelCount = 0
      var r = 0; var g = 0; var b = 0; var a = 0;
      for {
        xOffset <- clamp(x - radius, 0, src.width - 1) to clamp(x + radius, 0, src.width - 1)
        yOffset <- clamp(y - radius, 0, src.height - 1) to clamp(y + radius, 0, src.height - 1)
      } {
        pixelCount = pixelCount + 1
        r = r + red(src(xOffset, yOffset))
        g = g + green(src(xOffset, yOffset))
        b = b + blue(src(xOffset, yOffset))
        a = a + alpha(src(xOffset, yOffset))
      }

      rgba(r / pixelCount, g / pixelCount, b / pixelCount, a / pixelCount)
    }
  }

  def getRange(start: Int, end: Int, step: Int): List[Int] = {
    val adaptedStep = if (step < 1) 1 else step
    val range = (0 to end by adaptedStep).toList

    if (end % adaptedStep != 0) range ::: List(end)
    else range
  }
}
