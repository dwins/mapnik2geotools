package me.winslow.d
package object mn2gt {
  import xml.{ Elem, Node }

  def attsToParams(e: Elem): Seq[Node] =
    e.attributes.asAttrMap.toSeq.map { case (k, v) => 
      <CssParameter name={k}>{v}</CssParameter>
    }
}
