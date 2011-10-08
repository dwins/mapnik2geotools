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
    case NamedColor(hex) => (hex, None)
    case ShortHex(r, g, b) => (Seq("#", r, r, g, g, b, b).mkString, None)
    case Hex(r, g, b) => (Seq("#", r, g, b).mkString, None)
    case Rgb(r, g, b) => (rgb2hex(r, g, b), None)
    case Rgba(r, g, b, a) => (rgb2hex(r, g, b), Some(a))
    case _ => (color, None)
  }

  def rgb2hex(r: String, g: String, b: String): String = {
    "#%02x%02x%02x".format(r.toInt, g.toInt, b.toInt)
  }
}
