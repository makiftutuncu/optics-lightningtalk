package com.vngrs.lightningtalk.optics

import scala.language.dynamics
import scala.reflect.ClassTag

sealed trait Json extends Product with Serializable { self =>
  def as[J <: Json : ClassTag]: Option[J] =
    self match {
      case j: J => Some(j)
      case _    => None
    }

  def show: String

  override def toString: String = show
}

object Json {
  case object JNull extends Json {
    override def show: String = "null"
  }

  final case class JBoolean(b: Boolean) extends Json {
    val value: Boolean = b

    override def show: String = b.toString
  }

  final case class JInt(i: Int) extends Json {
    val value: Int = i

    override def show: String = i.toString
  }

  final case class JString(s: String) extends Json {
    val value: String = s

    override def show: String = s""""$s""""
  }

  final case class JArray(js: List[Json]) extends Json with Dynamic {
    def get(index: Int): Option[Json] = js.drop(index).headOption

    def applyDynamic(s: String)(index: Int): Option[Json] = get(index)

    override def show: String = js.mkString("[", ", ", "]")
  }

  object JArray {
    val empty: JArray = new JArray(List.empty)

    def apply(js: Json*): JArray = new JArray(js.toList)
  }

  final case class JObject(pairs: List[(String, Json)]) extends Json with Dynamic {
    def get(key: String): Option[Json] = pairs.find(_._1 == key).map(_._2)

    def selectDynamic(key: String): Option[Json] = get(key)

    override def show: String = pairs.map { case (k, j) => s""""$k": $j""" }.mkString("{", ", ", "}")
  }

  object JObject {
    val empty: JObject = new JObject(List.empty)

    def apply(pairs: (String, Json)*): JObject = new JObject(pairs.toList)
  }
}
