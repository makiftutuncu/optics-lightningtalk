package com.vngrs.lightningtalk.optics

import org.scalatest.{Matchers, WordSpec}

class SetterSpec extends WordSpec with Matchers with TestData {
  private val nameSetter      = Setter[User, String](modifier => user => user.copy(name = modifier(user.name)))
  private val languagesSetter = Setter[User, List[String]](modifier => user => user.copy(languages = modifier(user.languages)))

  private val antiJavaSetter: Setter[List[String], List[String]] =
    Setter[List[String], List[String]](modifier => languages => modifier(languages).filterNot(_ == "Java"))

  private val antiJavaLanguagesSetter: Setter[User, List[String]] = languagesSetter compose antiJavaSetter

  "Setter" should {
    "be able to set a field" in {
      val newEsra = nameSetter.set("Esra")(esra)

      esra.name    shouldBe "esra"
      newEsra.name shouldBe "Esra"
    }

    "be able to modify a field" in {
      val modifiedAkif = languagesSetter.modify(oldLanguages => oldLanguages :+ "Go")(akif)

      akif.languages         shouldBe List("Scala", "Java")
      modifiedAkif.languages shouldBe List("Scala", "Java", "Go")
    }

    "be able to get composed" in {
      val newAkif = antiJavaLanguagesSetter.modify(oldLanguages => oldLanguages :+ "Go")(akif)

      akif.languages    shouldBe List("Scala", "Java")
      newAkif.languages shouldBe List("Scala", "Go")
    }
  }
}
