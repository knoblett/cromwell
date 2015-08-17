package cromwell.binding.types

import cromwell.binding.values.{WdlFile, WdlString}
import spray.json.JsString

case object WdlFileType extends WdlPrimitiveType {
  val toWdlString: String = "File"

  override protected def coercion = {
    case s: String => WdlFile(s)
    case s: JsString => WdlFile(s.value)
    case s: WdlString => WdlFile(s.valueString)
    case f: WdlFile => f
  }
}
