package com.vngrs.lightningtalk.optics

import org.scalatest.{Matchers, WordSpec}

class OpticSpec extends WordSpec with Matchers with TestData {
  private val languagesOptic = Optic[User, List[String]](user => Some(user.languages))(modifier => user => user.copy(languages = modifier(user.languages)))

  private val listHeadOptic = Optic[List[String], String](_.headOption)(modifier => list => list.map(modifier))

  private val firstLanguageOptic: Optic[User, String] = languagesOptic compose listHeadOptic

  "Optic" should {
    "be able to optionally get" in {
      languagesOptic.getOption(akif) shouldBe Some(List("Scala", "Java"))
    }

    "be able to modify a field" in {
      val newAkif = languagesOptic.modify(_.take(1))(akif)

      akif.languages    shouldBe List("Scala", "Java")
      newAkif.languages shouldBe List("Scala")
    }

    "be able to set a field" in {
      val newAkif = languagesOptic.set(List("Go"))(akif)

      akif.languages    shouldBe List("Scala", "Java")
      newAkif.languages shouldBe List("Go")
    }

    "be able to get composed with other optics" in {
      firstLanguageOptic.getOption(akif)                              shouldBe Some("Scala")
      firstLanguageOptic.getOption(akif.copy(languages = List.empty)) shouldBe None

      val newAkif1 = firstLanguageOptic.set("Go")(akif)

      akif.languages     shouldBe List("Scala", "Java")
      newAkif1.languages shouldBe List("Go", "Go")

      val newAkif2 = firstLanguageOptic.modify(_.toUpperCase)(akif)

      newAkif2.languages shouldBe List("SCALA", "JAVA")

      val newAkif3 = firstLanguageOptic.modify(_.toUpperCase)(akif.copy(languages = List.empty))

      newAkif3.languages shouldBe List.empty
    }
  }
}
