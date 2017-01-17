
import common._

import scala.collection.immutable.IndexedSeq

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
    // TODO implement using while loops
    val startX = clamp(x - radius, 0, src.width)
    val startY = clamp(y - radius, 0, src.height)
    val endX = clamp(x + radius, 0, src.width)
    val endY = clamp(y + radius, 0, src.height)
    var (cX, cY) = (startX, startY)
    val pixels: IndexedSeq[(Int, Int)] = for {
      x <- startX to endX
      y <- startY to endY
    } yield (x, y)

    val center: RGBA = src(x, y)
    val size = pixels.size
    val map: IndexedSeq[RGBA] = pixels
      .map(t => src(t._1, t._2))
    def op(acc: (Int, Int, Int, Int), i: RGBA) : (Int, Int, Int, Int) = {
      val i1: Int = red(i) + acc._1
      val i2: Int = green(i) + acc._2
      val i3: Int = blue(i) + acc._3
      val i4: Int = alpha(i) + acc._4
      (i1, i2, i3, i4)
    }
    val res: (Int, Int, Int, Int) = map
      .foldLeft((0, 0, 0, 0))(op)
    if (size > 0) rgba(res._1 / size, res._2 / size, res._3 / size, res._4 / size)
    else center
  }

}
