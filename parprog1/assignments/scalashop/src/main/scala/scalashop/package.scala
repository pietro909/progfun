
import common._

import scala.collection.mutable.ArrayBuffer

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
    override def toString =
      if (data.isEmpty) "[]"
      else data.toList.sliding(width, width).map(_.mkString(" - ")).mkString("\n")
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    // TODO implement using while loops
    val minX = clamp(x - radius, 0, src.width)
    var minY = clamp(y - radius, 0, src.height)
    val maxX = clamp(x + radius, 0, src.width)
    val maxY = clamp(y + radius, 0, src.height)
    var average: Option[RGBA] = None
    println(s"*** $x, $y with $radius")
    println(src)
    while (minY <= maxY) {
      var currentX = minX
      while (currentX <= maxX) {
        val pixel = src(currentX, minY)
        println(s" at($currentX, $minY) is $pixel")
        average = average match {
          case Some(number) =>
            val r = (red(pixel) + red(number)) / 2
            val g = (green(pixel) + green(number)) / 2
            val b = (blue(pixel) + blue(number)) / 2
            val a = (alpha(pixel) + alpha(number)) / 2
            Some(rgba(r, g, b, a))
          case None =>
            Some(pixel)
          }
        currentX += 1
      }
      minY += 1
    }
    println(s"average = $average")
    average.get
  }

}
