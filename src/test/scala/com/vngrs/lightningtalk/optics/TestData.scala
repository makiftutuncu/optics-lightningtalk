package com.vngrs.lightningtalk.optics

import com.vngrs.lightningtalk.optics.Json._

trait TestData {
  final case class User(id: Int, name: String, isMale: Boolean, languages: List[String], metadata: Metadata) {
    def toJson: Json =
      JObject(
        "id"        -> JInt(id),
        "name"      -> JString(name),
        "isMale"    -> JBoolean(isMale),
        "languages" -> JArray(languages.map(JString.apply)),
        "metadata"  -> metadata.toJson
      )
  }

  final case class Metadata(isSuperstar: Boolean) {
    def toJson: Json = JObject("isSuperstar" -> JBoolean(isSuperstar))
  }

  val akif: User = User(1, "akif", isMale = true, List("Scala", "Java"), Metadata(false))

  val esra: User = User(2, "esra", isMale = false, List("SQL", "Java", "NodeJS"), Metadata(true))

  val users: Json = JArray(akif.toJson, esra.toJson)
}
