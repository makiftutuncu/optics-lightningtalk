package com.vngrs.lightningtalk.optics

import com.vngrs.lightningtalk.optics.Json._
import org.scalatest.{Matchers, WordSpec}

class PrismSpec extends WordSpec with Matchers with TestData {
  private val jsonStringPrism = Prism[Json, String]       { case JString(s) => s      } { JString.apply }
  private val jsonJArrayPrism = Prism[Json, JArray]       { case a: JArray => a       } { identity }
  private val jArrayListPrism = Prism[JArray, List[Json]] { case JArray(list) => list } { JArray.apply }

  private val jsonListPrism: Prism[Json, List[Json]] = jsonJArrayPrism compose jArrayListPrism

  "Prism" should {
    "be able to reverse get a field" in {
      jsonStringPrism.reverseGet("foo") shouldBe JString("foo")
    }

    "be able to optionally get a field" in {
      jsonStringPrism.getOption(JNull)          shouldBe None
      jsonStringPrism.getOption(JString("foo")) shouldBe Some("foo")
    }

    "be able to optionally modify" in {
      jsonStringPrism.modifyOption(_.toUpperCase)(JNull)          shouldBe None
      jsonStringPrism.modifyOption(_.toUpperCase)(JString("foo")) shouldBe Some("FOO")
    }

    "be able to get composed" in {
      val arr  = JArray(JNull, JInt(1))
      val list = List(JNull, JInt(1))

      jsonListPrism.reverseGet(list) shouldBe arr

      jsonListPrism.getOption(JString("foo")) shouldBe None
      jsonListPrism.getOption(arr)            shouldBe Some(list)

      jsonListPrism.modifyOption(_.take(1))(JNull) shouldBe None
      jsonListPrism.modifyOption(_.take(1))(arr)   shouldBe Some(List(JNull))
    }
  }
}
