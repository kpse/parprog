package scalashop

import java.util.concurrent.ForkJoinTask

import org.scalameter._
import common._

import scala.collection.immutable.IndexedSeq

object HorizontalBoxBlurRunner {

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
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}


/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
    * starting with `from` and ending with `end` (non-inclusive).
    *
    * Within each row, `blur` traverses the pixels by going from left to right.
    */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    if (from > src.height - 1) return
    else if (from == end) innerBlur(from)
    else (from until end).foreach(innerBlur)

    def innerBlur(row: Int) = (0 until src.width).foreach(col => dst.update(col, row, boxBlurKernel(src, col, row, radius)))
  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
    *
    * Parallelization is done by stripping the source image `src` into
    * `numTasks` separate strips, where each strip is composed of some number of
    * rows.
    */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    if (numTasks == 0) throw new IllegalArgumentException("cannot split into 0 tasks")
    val tasksCount = Integer.min(numTasks, src.width)
    val sizeOfChunk: Int = src.width / tasksCount
    val stepOfChunk: Int = if (sizeOfChunk > 1) sizeOfChunk - 1 else 1
    (0 to src.height).toArray.sliding(sizeOfChunk, stepOfChunk).map(arr => task {
      blur(src, dst, arr.head, arr.last, radius)
    }).foreach(x => x.join())

  }

}
