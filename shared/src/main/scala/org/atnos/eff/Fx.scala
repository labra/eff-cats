package org.atnos.eff

/** one effect, basically a type constructor */
sealed trait Effect[F[_]]

sealed trait Fx

object Fx {
  def apply[S](implicit e: EffectsToFx[S]): Program[S, e.X] = new Program[S, e.X]
}

class Program[F, R] {
  type Fx = R
}



/**
  * Append an effect at the beginning of a list of effects
  */
final case class Append[L, R](left: L, right: R) extends Fx

object Append {
  type apply[T[_], R] = Append[In1[T], R]
}
final case class In1[F[_]](e: Effect[F]) extends Fx
final case class In2[L[_], R[_]](left: Effect[L], right: Effect[R]) extends Fx
final case class In3[L[_], M[_], R[_]](left: Effect[L], middle: Effect[M], right: Effect[R]) extends Fx

/**
  * Nil case for the list of effects
  */
class NoFx extends Fx {
  def |:[G[_]](g: Effect[G]) =
    Append(In1(g), this)
}

object NoFx extends NoFx
