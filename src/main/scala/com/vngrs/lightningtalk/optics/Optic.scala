package com.vngrs.lightningtalk.optics

trait Optic[S, A] { self =>
  def getOption(source: S): Option[A]
  def modify(modifier: A => A): S => S

  def set(field: A): S => S = modify(_ => field)

  def compose[B](optic: Optic[A, B]): Optic[S, B] =
    new Optic[S, B] {
      override def getOption(source: S): Option[B] =
        self.getOption(source).flatMap(optic.getOption)

      override def modify(modifier: B => B): S => S = {
        val bToAModifier: (B => B) => A => A = optic.modify
        val aModifier: (A => A) => S => S    = self.modify

        (bToAModifier andThen aModifier).apply(modifier)
      }
    }
}

object Optic {
  def apply[S, A](getter: S => Option[A])(updated: (A => A) => S => S): Optic[S, A] =
    new Optic[S, A] {
      override def getOption(source: S): Option[A]  = getter(source)
      override def modify(modifier: A => A): S => S = updated(modifier)
    }
}
