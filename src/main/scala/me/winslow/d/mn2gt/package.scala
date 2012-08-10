package me.winslow.d
package object mn2gt {
  import xml.{ Elem, Node }

  /**
   * Extract the attributes of a mapnik-style symbolizer node:
   * {code}
   * <LineSymbolizer stroke="#00FF00"/>
   * {code}
   * 
   * and convert them to a list of SLD-style CSSParameter elements:
   * {code}
   * <CssParameter name="stroke">#00FF00</CssParameter>
   * {code}
   */
  def attsToParams(e: Elem): Seq[Node] =
    e.attributes.asAttrMap.map({ case (k, v) => 
      <CssParameter name={k}>{v}</CssParameter>
    })(collection.breakOut)
}