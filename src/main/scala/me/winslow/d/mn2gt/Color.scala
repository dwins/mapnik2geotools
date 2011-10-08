package me.winslow.d.mn2gt

class Color(color: String) {
  import CSS.NamedColor

  /**
   * Extractor for RGB colors using the rgb function, e.g. rgba(255,255,255)
   */
  val Rgb = """rgb\((\d+),\s*(\d+),\s*(\d+)\)""".r

  /**
   * Extractor for RGBA colors using the rgba function, e.g. rgba(255,255,255,0.25)
   */
  val Rgba = """rgba\((\d+),\s*(\d+),\s*(\d+),\s*([\d.]+)\)""".r

  /**
   * Extractor for short hexadecimal RGB strings such as #888, giving each
   * channel as a String of length 1
   */
  val ShortHex = """#(\p{XDigit})(\p{XDigit})(\p{XDigit})""".r

  /**
   * Extractor for hexadecimal RGB strings such as #888888, giving each channel
   * as a String of length 2
   */
  val Hex = """#(\p{XDigit}{2})(\p{XDigit}{2})(\p{XDigit}{2})""".r

  val (hex, alpha) = color match {
    case NamedColor(hex) => (hex, null)
    case ShortHex(r, g, b) => (Seq("#", r, r, g, g, b, b).mkString, null)
    case Hex(r, g, b) => (Seq("#", r, g, b).mkString, null)
    case Rgb(r, g, b) => (rgb2hex(r, g, b), null)
    case Rgba(r, g, b, a) => (rgb2hex(r, g, b), a)
    case _ => (color, null)
  }

  def rgb2hex(r: String, g: String, b: String): String = {
    "#%02x%02x%02x".format(r.toInt, g.toInt, b.toInt)
  }

  def hasAlpha() = alpha != null
}
