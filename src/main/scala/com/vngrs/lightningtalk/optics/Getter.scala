package com.vngrs.lightningtalk.optics

trait Getter[S, A] { self =>
  def get(source: S): A

  def compose[B](getter: Getter[A, B]): Getter[S, B] =
    new Getter[S, B] {
      override def get(source: S): B = {
        val aGetter: S => A = self.get
        val bGetter: A => B = getter.get

        (aGetter andThen bGetter).apply(source)
      }
    }
}

object Getter {
  def apply[S, A](getter: S => A): Getter[S, A] =
    new Getter[S, A] {
      override def get(source: S): A = getter(source)
    }
}


