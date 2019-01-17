package com.vngrs.lightningtalk.optics

import org.scalatest.{Matchers, WordSpec}

class GetterSpec extends WordSpec with Matchers with TestData {
  private val nameGetter              = Getter[User, String](_.name)
  private val metadataGetter          = Getter[User, Metadata](_.metadata)
  private val metadataSuperstarGetter = Getter[Metadata, Boolean](_.isSuperstar)

  private val isSuperstarGetter: Getter[User, Boolean] = metadataGetter compose metadataSuperstarGetter

  "Getter" should {
    "be able get a field" in {
      nameGetter.get(esra) shouldBe "esra"
    }

    "be able to get composed" in {
      isSuperstarGetter.get(esra) shouldBe true
      isSuperstarGetter.get(akif) shouldBe false
    }
  }
}
