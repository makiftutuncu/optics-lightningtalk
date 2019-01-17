package com.vngrs.lightningtalk.optics

import com.vngrs.lightningtalk.optics.Json._
import org.scalatest.{Matchers, WordSpec}

class JsonSpec extends WordSpec with Matchers with TestData {
  private def isFirstUserASuperstar(users: Json): Boolean = {
    val maybe =
      for {
        usersArray         <- users.as[JArray]
        firstJson          <- usersArray.get(0)
        firstObject        <- firstJson.as[JObject]
        metadataJson       <- firstObject.get("metadata")
        metadataObject     <- metadataJson.as[JObject]
        isSuperstarJson    <- metadataObject.get("isSuperstar")
        isSuperstarBoolean <- isSuperstarJson.as[JBoolean]
      } yield {
        isSuperstarBoolean.value
      }

    maybe.getOrElse(false)
  }

  private val jArrayOptic            = Optic[Json, JArray](_.as[JArray])(modifier => json => json.as[JArray].map(modifier).getOrElse(JArray.empty))
  private val jArrayHeadOptic        = Optic[JArray, Json](_.get(0))(modifier => arr => JArray(arr.js.take(1).map(modifier) ++ arr.js.drop(1)))
  private val jObjectOptic           = Optic[Json, JObject](_.as[JObject])(modifier => json => json.as[JObject].map(modifier).getOrElse(JObject.empty))
  private val jBooleanOptic          = Optic[Json, JBoolean](_.as[JBoolean])(modifier => json => json.as[JBoolean].map(modifier).getOrElse(JBoolean(false)))
  private val jBooleanToBooleanOptic = Optic[JBoolean, Boolean](jb => Some(jb.value))(modifier => jb => JBoolean(modifier(jb.value)))

  private def jObjectFieldOptic(field: String): Optic[JObject, Json] = Optic[JObject, Json](_.get(field))(modifier => obj => {
    JObject(
      obj.pairs.foldLeft[List[(String, Json)]](List.empty) {
        case (l, (k, v)) if k == field => l :+ (k -> modifier(v))
        case (l, (k, v))               => l :+ (k -> v)
      }
    )
  })

  private val firstUserASuperstarOptic: Optic[Json, Boolean] =
    jArrayOptic                      compose
    jArrayHeadOptic                  compose
    jObjectOptic                     compose
    jObjectFieldOptic("metadata")    compose
    jObjectOptic                     compose
    jObjectFieldOptic("isSuperstar") compose
    jBooleanOptic                    compose
    jBooleanToBooleanOptic

  "Json" should {
    "be shown properly" in {
      val akifString = """{"id": 1, "name": "akif", "isMale": true, "languages": ["Scala", "Java"], "metadata": {"isSuperstar": false}}"""
      val esraString = """{"id": 2, "name": "esra", "isMale": false, "languages": ["SQL", "Java", "NodeJS"], "metadata": {"isSuperstar": true}}"""

      akif.toJson.show shouldBe akifString
      esra.toJson.show shouldBe esraString

      users.show shouldBe s"[$akifString, $esraString]"
    }

    "can be accessed without optics" in {
      isFirstUserASuperstar(users)               shouldBe false
      isFirstUserASuperstar(JArray(esra.toJson)) shouldBe true
    }

    "can be accessed with optics" in {
      firstUserASuperstarOptic.getOption(users)               shouldBe Some(false)
      firstUserASuperstarOptic.getOption(JArray(esra.toJson)) shouldBe Some(true)
      firstUserASuperstarOptic.getOption(JString("foo"))      shouldBe None

      val superstarAkif = akif.copy(metadata = Metadata(true))

      firstUserASuperstarOptic.set(true)(users) shouldBe JArray(superstarAkif.toJson, esra.toJson)
    }
  }

  "JObject" should {
    "be able to dynamically access a field" in {
      val jObject = akif.toJson.as[JObject].getOrElse(JObject.empty)

      jObject.name shouldBe Some(JString("akif"))
      jObject.age  shouldBe None
    }
  }

  "JArray" should {
    "be able to dynamically access an index" in {
      val jArray = users.as[JArray].getOrElse(JArray.empty)

      jArray(0) shouldBe Some(akif.toJson)
      jArray(1) shouldBe Some(esra.toJson)
      jArray(2) shouldBe None
    }
  }
}
