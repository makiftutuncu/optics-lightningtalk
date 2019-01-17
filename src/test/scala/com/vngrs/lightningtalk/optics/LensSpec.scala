package com.vngrs.lightningtalk.optics

import org.scalatest.{Matchers, WordSpec}

class LensSpec extends WordSpec with Matchers with TestData {
  private val nameLens                = Lens[User, String](_.name)(modifier => user => user.copy(name = modifier(user.name)))
  private val metadataLens            = Lens[User, Metadata](_.metadata)(modifier => user => user.copy(metadata = modifier(user.metadata)))
  private val metadataIsSuperstarLens = Lens[Metadata, Boolean](_.isSuperstar)(modifier => metadata => metadata.copy(isSuperstar = modifier(metadata.isSuperstar)))

  private val isSuperstarLens: Lens[User, Boolean] = metadataLens compose metadataIsSuperstarLens

  "Lens" should {
    "be able get a field" in {
      nameLens.get(akif) shouldBe "akif"
    }

    "be able to set a field" in {
      val newEsra = nameLens.set("Esra")(esra)

      esra.name    shouldBe "esra"
      newEsra.name shouldBe "Esra"
    }

    "be able to modify a field" in {
      val newAkif = nameLens.modify(_.toUpperCase)(akif)

      akif.name    shouldBe "akif"
      newAkif.name shouldBe "AKIF"
    }

    "be able to get composed" in {
      isSuperstarLens.get(esra) shouldBe true
      isSuperstarLens.get(akif) shouldBe false

      val newAkif = isSuperstarLens.set(true)(akif)

      isSuperstarLens.get(newAkif) shouldBe true
    }
  }
}
