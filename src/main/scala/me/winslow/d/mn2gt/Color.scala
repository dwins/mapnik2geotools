package me.winslow.d.mn2gt

class Color(color: String) {
  import CSS.NamedColor

  val Rgb = """rgb\((\d+),\s*(\d+),\s*(\d+)\)""".r
  val Rgba = """rgba\((\d+),\s*(\d+),\s*(\d+),\s*([\d.]+)\)""".r
  val ShortHex = """#(\p{XDigit})(\p{XDigit})(\p{XDigit})""".r
  val Hex = """#(\p{XDigit}{2})(\p{XDigit}{2})(\p{XDigit}{2})""".r

  var hex: String = null
  var alpha: String = null

  hex = color match {
    // named color, e.g. blue
    case NamedColor(hex) => hex
    // short hex, e.g. #888
    case ShortHex(r, g, b) => Seq("#", r, r, g, g, b, b).mkString
    // hex, e.g. #888888
    case Hex(r, g, b) => Seq("#", r, g, b).mkString
    // rgb, e.g. rgb(255,255,255)
    case Rgb(r, g, b) => rgb2hex(r, g, b)
    // rgba, e.g. rgba(255,255,255,0.25)
    case Rgba(r, g, b, a) =>
      alpha = a
      rgb2hex(r, g, b)
    case _ =>
      color
  }

  def rgb2hex(r: String, g: String, b: String): String = {
    "#%02x%02x%02x".format(r.toInt, g.toInt, b.toInt)
  }

  def hasAlpha() = alpha != null
}
