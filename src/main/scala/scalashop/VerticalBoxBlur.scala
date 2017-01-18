package scalashop

import common._
import org.scalameter._

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
    * `dst`, starting with `from` and ending with `end` (non-inclusive).
    *
    * Within each column, `blur` traverses the pixels by going from top to
    * bottom.
    */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    if (from == end) innerBlur(from)
    else
      (from until end).foreach(innerBlur)
    def innerBlur(col: Int) = (0 until src.height).foreach(row => dst.update(col, row, boxBlurKernel(src, col, row, radius)))
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
    *
    * Parallelization is done by stripping the source image `src` into
    * `numTasks` separate strips, where each strip is composed of some number of
    * columns.
    */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    if (numTasks == 0) throw new IllegalArgumentException("cannot split into 0 tasks")
    val tasksCount = Integer.min(numTasks, src.width)
    val sizeOfChunk: Int = src.width / tasksCount
    val stepOfChunk: Int = if (sizeOfChunk > 1) sizeOfChunk - 1 else 1
    (0 to src.width).toArray.sliding(sizeOfChunk, stepOfChunk).foreach(arr => task {
      blur(src, dst, arr.head, arr.last, radius)
    })

    blur(src, dst, src.width - 1, src.width, radius)
  }

}
