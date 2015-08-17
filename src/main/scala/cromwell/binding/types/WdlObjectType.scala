package cromwell.binding.types

import cromwell.binding.values.WdlObject

case object WdlObjectType extends WdlType {
  val toWdlString: String = "Object"

  override protected def coercion = {
    case o: WdlObject => o
  }

  override def fromWdlString(rawString: String) = ???
}
