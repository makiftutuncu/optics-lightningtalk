package com.vngrs.lightningtalk.optics

trait Prism[S, A] { self =>
  def reverseGet(a: A): S
  def getOption(s: S): Option[A]

  def modifyOption(modifier: A => A): S => Option[A] = s => getOption(s).map(modifier)

  def compose[B](prism: Prism[A, B]): Prism[S, B] =
    new Prism[S, B] {
      override def reverseGet(b: B): S = {
        val getA: B => A = prism.reverseGet
        val getS: A => S = self.reverseGet

        (getA andThen getS).apply(b)
      }

      override def getOption(s: S): Option[B] = self.getOption(s).flatMap(a => prism.getOption(a))
    }
}

object Prism {
  def apply[S, A](collector: PartialFunction[S, A])(reverseGetter: A => S): Prism[S, A] =
    new Prism[S, A] {
      override def reverseGet(a: A): S = reverseGetter(a)

      override def getOption(s: S): Option[A] = Option(s).collect(collector)
    }
}
