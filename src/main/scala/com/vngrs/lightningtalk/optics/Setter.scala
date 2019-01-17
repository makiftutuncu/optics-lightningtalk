package com.vngrs.lightningtalk.optics

trait Setter[S, A] { self =>
  def modify(modifier: A => A): S => S

  def set(field: A): S => S = modify(_ => field)

  def compose[B](setter: Setter[A, B]): Setter[S, B] =
    new Setter[S, B] {
      override def modify(modifier: B => B): S => S = {
        val bToAModifier: (B => B) => A => A = setter.modify
        val aModifier: (A => A) => S => S    = self.modify

        (bToAModifier andThen aModifier).apply(modifier)
      }
    }
}

object Setter {
  def apply[S, A](updater: (A => A) => S => S): Setter[S, A] =
    new Setter[S, A] {
      override def modify(modifier: A => A): S => S = updater(modifier)
    }
}
