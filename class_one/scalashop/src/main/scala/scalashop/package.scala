

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
      val x_c = clamp(x - radius, 0, src.width - 1)
      val x_max = clamp(x + radius, 0, src.width - 1)
      val y_c = clamp(y - radius, 0, src.height - 1)
      val y_max = clamp(y + radius, 0, src.height - 1)
      def sumRgba(p: RGBA, acc: RGBA):RGBA = rgba(red(p) + red(acc), green(p) + green(acc), blue(p) + blue(acc), alpha(p) + alpha(acc))
      val accRed = 0
      val accGreen = 0
      val accBlue = 0
      val accAlpha = 0

      val counter = 0
      while (x_c <= x_max) {
          while (y_c <= y_max) {
              counter = counter + 1
              val p = src.apply(x_c, y_c)
              accRed = red(p)
              accGreen = green(p)
              accBlue = blue(p)
              accAlpha = alpha(p)
              y_c = y_c + 1
          }
          x_c = x_c + 1
      }
      rgba(accRed/counter, accGreen/counter, accBlue/counter, accAlpha/ counter)
  }

}
