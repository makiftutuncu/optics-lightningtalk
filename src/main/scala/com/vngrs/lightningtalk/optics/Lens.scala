package com.vngrs.lightningtalk.optics

trait Lens[S, A] extends Getter[S, A] with Setter[S, A] { self =>
  def compose[B](lens: Lens[A, B]): Lens[S, B] =
    new Lens[S, B] {
      override def modify(modifier: B => B): S => S = {
        val bToAModifier: (B => B) => A => A = lens.modify
        val aModifier: (A => A) => S => S    = self.modify

        (bToAModifier andThen aModifier).apply(modifier)
      }

      override def get(source: S): B = {
        val aGetter: S => A = self.get
        val bGetter: A => B = lens.get

        (aGetter andThen bGetter).apply(source)
      }
    }
}

object Lens {
  def apply[S, A](getter: S => A)(setter: (A => A) => S => S): Lens[S, A] =
    new Lens[S, A] {
      override def modify(modifier: A => A): S => S = setter(modifier)
      override def get(source: S): A                = getter(source)
    }
}
