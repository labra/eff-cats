package org.atnos.eff

import cats.data.Xor
import scala.annotation.implicitNotFound

@implicitNotFound("No instance found for MemberIn[${T}, ${R}]. The ${T} effect is not part of the stack ${R}")
trait MemberIn[T[_], R] {
  def inject[V](tv: T[V]): Union[R, V]
}

object MemberIn extends MemberInLower1 {

  @implicitNotFound("No instance found for MemberIn[${T}, ${R}]. The ${T} effect is not part of the stack ${R}")
  type |=[T[_], R] = MemberIn[T, R]

  implicit def MemberInEffects1[T1[_]]: MemberIn[T1, T1 |: NoEffect] =
    MemberIn.MemberIn1[T1].asInstanceOf[MemberIn[T1, T1 |: NoEffect]]

  implicit def MemberInEffects2[T[_], T1[_], T2[_]](implicit m: MemberIn[T, In2[T1, T2]]): MemberIn[T, T1 |: T2 |: NoEffect] =
    m.asInstanceOf[MemberIn[T, T1 |: T2 |: NoEffect]]

  implicit def MemberInEffects2_[T[_], T1[_], T2[_]](implicit m: MemberIn[T, In2[T1, T2]]): MemberIn[T, T1 |:: T2] =
    m.asInstanceOf[MemberIn[T, T1 |:: T2]]

  implicit def MemberInEffects3[T[_], T1[_], T2[_], T3[_]](implicit m: MemberIn[T, In3[T1, T2, T3]]): MemberIn[T, T1 |: T2 |: T3 |: NoEffect] =
    m.asInstanceOf[MemberIn[T, T1 |: T2 |: T3 |: NoEffect]]

  implicit def MemberInEffects3_[T[_], T1[_], T2[_], T3[_]](implicit m: MemberIn[T, In3[T1, T2, T3]]): MemberIn[T, T1 |: T2 |:: T3] =
    m.asInstanceOf[MemberIn[T, T1 |: T2 |:: T3]]

  implicit def MemberInEffects4[T[_], T1[_], T2[_], T3[_], T4[_]](implicit m: MemberIn[T, Append[In1[T1], In3[T2, T3, T4]]]): MemberIn[T, T1 |: T2 |: T3 |: T4 |: NoEffect] =
    m.asInstanceOf[MemberIn[T, T1 |: T2 |: T3 |: T4 |: NoEffect]]

  implicit def MemberInEffects4_[T[_], T1[_], T2[_], T3[_], T4[_]](implicit m: MemberIn[T, Append[In1[T1], In3[T2, T3, T4]]]): MemberIn[T, T1 |: T2 |: T3 |:: T4] =
    m.asInstanceOf[MemberIn[T, T1 |: T2 |: T3 |:: T4]]

  implicit def MemberInEffects5[T[_], T1[_], T2[_], T3[_], T4[_], T5[_]](implicit m: MemberIn[T, Append[In2[T1, T2], In3[T3, T4, T5]]]): MemberIn[T, T1 |: T2 |: T3 |: T4 |: T5 |: NoEffect] =
    m.asInstanceOf[MemberIn[T, T1 |: T2 |: T3 |: T4 |: T5 |: NoEffect]]

  implicit def MemberInEffects5_[T[_], T1[_], T2[_], T3[_], T4[_], T5[_]](implicit m: MemberIn[T, Append[In2[T1, T2], In3[T3, T4, T5]]]): MemberIn[T, T1 |: T2 |: T3 |: T4 |:: T5] =
    m.asInstanceOf[MemberIn[T, T1 |: T2 |: T3 |: T4 |:: T5]]

  implicit def MemberInEffects6[T[_], T1[_], T2[_], T3[_], T4[_], T5[_], T6[_]](implicit m: MemberIn[T, Append[In3[T1, T2, T3], In3[T4, T5, T6]]]): MemberIn[T, T1 |: T2 |: T3 |: T4 |: T5 |: T6 |: NoEffect] =
    m.asInstanceOf[MemberIn[T, T1 |: T2 |: T3 |: T4 |: T5 |: T6 |: NoEffect]]

  implicit def MemberInEffects6_[T[_], T1[_], T2[_], T3[_], T4[_], T5[_], T6[_]](implicit m: MemberIn[T, Append[In3[T1, T2, T3], In3[T4, T5, T6]]]): MemberIn[T, T1 |: T2 |: T3 |: T4 |: T5 |:: T6] =
    m.asInstanceOf[MemberIn[T, T1 |: T2 |: T3 |: T4 |: T5 |:: T6]]

}

trait MemberInLower1 extends MemberInLower2 {
  implicit def MemberIn1[T[_]]: MemberIn[T, In1[T]] = new MemberIn[T, In1[T]] {
    def inject[V](effect: T[V]): Union[In1[T], V] =
      Union1(effect)
  }

  implicit def MemberIn2L[L[_], R[_]]: MemberIn[L, In2[L, R]] = new MemberIn[L, In2[L, R]] {
    def inject[V](effect: L[V]): Union[In2[L, R], V] =
      Union2L(effect)
  }

  implicit def MemberIn3L[L[_], M[_], R[_]]: MemberIn[L, In3[L, M, R]] = new MemberIn[L, In3[L, M, R]] {
    def inject[V](effect: L[V]): Union[In3[L, M, R], V] =
      Union3L(effect)
  }

  implicit def MemberInAppendL[T[_], L, R](implicit append: MemberIn[T, L]): MemberIn[T, Append[L, R]] = new MemberIn[T, Append[L, R]] {
    def inject[V](effect: T[V]): Union[Append[L, R], V] =
      UnionAppendL(append.inject(effect))
  }

  implicit def MemberIn2R[L[_], R[_]]: MemberIn[R, In2[L, R]] = new MemberIn[R, In2[L, R]] {
    def inject[V](effect: R[V]): Union[In2[L, R], V] =
      Union2R(effect)
  }

}

trait MemberInLower2 extends MemberInLower3 {
  implicit def MemberInAppendR[T[_], L, R](implicit append: MemberIn[T, R]): MemberIn[T, Append[L, R]] = new MemberIn[T, Append[L, R]] {
    def inject[V](effect: T[V]): Union[Append[L, R], V] =
      UnionAppendR(append.inject(effect))
  }

  implicit def MemberIn3M[L[_], M[_], R[_]]: MemberIn[M, In3[L, M, R]] = new MemberIn[M, In3[L, M, R]] {
    def inject[V](effect: M[V]): Union[In3[L, M, R], V] =
      Union3M(effect)
  }
}

trait MemberInLower3 extends MemberInLower4 {
  implicit def MemberIn3R[L[_], M[_], R[_]]: MemberIn[R, In3[L, M, R]] = new MemberIn[R, In3[L, M, R]] {
    def inject[V](effect: R[V]): Union[In3[L, M, R], V] =
      Union3R(effect)
  }
}

trait MemberInLower4 {
  implicit def MemberInAppendAnyL[T[_], R]: MemberIn[T, Append[In1[T], R]] = new MemberIn[T, Append[In1[T], R]] {
    def inject[V](effect: T[V]): Union[Append[In1[T], R], V] =
      UnionAppendL(Union1(effect))
  }

  implicit def MemberInAppendAnyR[T[_], L, R](implicit m: MemberIn[T, R]): MemberIn[T, Append[L, R]] = new MemberIn[T, Append[L, R]] {
    def inject[V](effect: T[V]): Union[Append[L, R], V] =
      UnionAppendR(m.inject(effect))
  }
}

@implicitNotFound("No instance found for Member[${T}, ${R}]. The ${T} effect is not part of the stack ${R} or it was not possible to determine the stack that would result from removing ${T} from ${R}")
trait Member[T[_], R] extends MemberIn[T, R] {
  type Out

  def accept[V](union: Union[Out, V]): Union[R, V]

  def project[V](union: Union[R, V]): Union[Out, V] Xor T[V]

  def aux: Member.Aux[T, R, Out] =
    this
}

object Member extends MemberLower1 {

  @implicitNotFound("No instance found for Member[${T}, ${R}]. The ${T} effect is not part of the stack ${R} or it was not possible to determine the stack that would result from removing ${T} from ${R}")
  type <=[T[_], R] = Member[T, R]

  @implicitNotFound("No instance found for Member[${T}, ${R}]. The ${T} effect is not part of the stack ${R} or it was not possible to determine the stack that would result from removing ${T} from ${R}")
  type Aux[T[_], R, U] = Member[T, R] { type Out = U }

  def apply[T[_], R](implicit m: Member[T, R]): Member[T, R] =
    m

  def aux[T[_], R, U](implicit m: Member.Aux[T, R, U]): Member.Aux[T, R, U] =
    m

  def unaux[T[_], R, U](implicit m: Member.Aux[T, R, U]): Member[T, R] =
    m

  implicit def MemberEffects1[T1[_]]: Member.Aux[T1, T1 |: NoEffect, NoFx] =
    Member.Member1[T1].asInstanceOf[Member.Aux[T1, T1 |: NoEffect, NoFx]]

  implicit def MemberEffects2[T[_], T1[_], T2[_], U](implicit m: Member.Aux[T, In2[T1, T2], U]): Member.Aux[T, T1 |: T2 |: NoEffect, U] =
    m.asInstanceOf[Member.Aux[T, T1 |: T2 |: NoEffect, U]]

  implicit def MemberEffects3[T[_], T1[_], T2[_], T3[_], U](implicit m: Member.Aux[T, In3[T1, T2, T3], U]): Member.Aux[T, T1 |: T2 |: T3 |: NoEffect, U] =
    m.asInstanceOf[Member.Aux[T, T1 |: T2 |: T3 |: NoEffect, U]]

  implicit def MemberEffects4[T[_], T1[_], T2[_], T3[_], T4[_], U](implicit m: Member.Aux[T, Append[In1[T1], In3[T2, T3, T4]], U]): Member.Aux[T, T1 |: T2 |: T3 |: T4 |: NoEffect, U] =
    m.asInstanceOf[Member.Aux[T, T1 |: T2 |: T3 |: T4 |: NoEffect, U]]

  implicit def MemberEffects5[T[_], T1[_], T2[_], T3[_], T4[_], T5[_], U](implicit m: Member.Aux[T, Append[In2[T1, T2], In3[T3, T4, T5]], U]): Member.Aux[T, T1 |: T2 |: T3 |: T4 |: T5 |: NoEffect, U] =
    m.asInstanceOf[Member.Aux[T, T1 |: T2 |: T3 |: T4 |: T5 |: NoEffect, U]]

  implicit def MemberEffects6[T[_], T1[_], T2[_], T3[_], T4[_], T5[_], T6[_], U](implicit m: Member.Aux[T, Append[In3[T1, T2, T3], In3[T4, T5, T6]], U]): Member.Aux[T, T1 |: T2 |: T3 |: T4 |: T5 |: T6 |: NoEffect, U] =
    m.asInstanceOf[Member.Aux[T, T1 |: T2 |: T3 |: T4 |: T5 |: T6 |: NoEffect, U]]

}

trait MemberLower1 extends MemberLower2 {
  implicit def MemberEffects2_[T[_], T1[_], T2[_], U](implicit m: Member.Aux[T, In2[T1, T2], U]): Member.Aux[T, T1 |:: T2, U] =
    m.asInstanceOf[Member.Aux[T, T1 |:: T2, U]]

  implicit def MemberEffects3_[T[_], T1[_], T2[_], T3[_], U](implicit m: Member.Aux[T, In3[T1, T2, T3], U]): Member.Aux[T, T1 |: T2 |:: T3, U] =
    m.asInstanceOf[Member.Aux[T, T1 |: T2 |:: T3, U]]

  implicit def MemberEffects4_[T[_], T1[_], T2[_], T3[_], T4[_], U](implicit m: Member.Aux[T, Append[In1[T1], In3[T2, T3, T4]], U]): Member.Aux[T, T1 |: T2 |: T3 |:: T4, U] =
    m.asInstanceOf[Member.Aux[T, T1 |: T2 |: T3 |:: T4, U]]

  implicit def MemberEffects5_[T[_], T1[_], T2[_], T3[_], T4[_], T5[_], U](implicit m: Member.Aux[T, Append[In2[T1, T2], In3[T3, T4, T5]], U]): Member.Aux[T, T1 |: T2 |: T3 |: T4 |:: T5, U] =
    m.asInstanceOf[Member.Aux[T, T1 |: T2 |: T3 |: T4 |:: T5, U]]

  implicit def MemberEffects6_[T[_], T1[_], T2[_], T3[_], T4[_], T5[_], T6[_], U](implicit m: Member.Aux[T, Append[In3[T1, T2, T3], In3[T4, T5, T6]], U]): Member.Aux[T, T1 |: T2 |: T3 |: T4 |: T5 |:: T6, U] =
    m.asInstanceOf[Member.Aux[T, T1 |: T2 |: T3 |: T4 |: T5 |:: T6, U]]

  implicit def Member1[T[_]]: Member.Aux[T, In1[T], NoFx] = new Member[T, In1[T]] { outer =>
    type Out = NoFx

    def inject[V](tv: T[V]): Union[In1[T], V] =
      Union1[T, V](tv)

    def accept[V](union: Union[Out, V]): Union[In1[T], V] =
      sys.error("cannot accept a nil effect as In1")

    def project[V](union: Union[In1[T], V]): Union[Out, V] Xor T[V] =
      union match {
        case Union1(e) => Xor.Right(e)
      }

    /** @return a member instance for another effect in the remaining stack */
    def out[W[_]](implicit w: MemberIn[W, In1[T]]): MemberIn[W, Out] = new MemberIn[W, Out] {
      def inject[V](effect: W[V]) =
        w.inject[V](effect) match {
          case Union1(x) => Union1(x).asInstanceOf[Union[Out, V]]
        }
    }
  }

  implicit def Member2L[L[_], R[_]]: Member.Aux[L, In2[L, R], In1[R]] = new Member[L, In2[L, R]] { outer =>
    type Out = In1[R]

    def inject[V](tv: L[V]): Union[In2[L, R], V] =
      Union2L[L, R, V](tv)

    def accept[V](union: Union[Out, V]): Union[In2[L, R], V] =
      union match {
        case Union1(e) => Union2R(e)
      }

    def project[V](union: Union[In2[L, R], V]): Union[Out, V] Xor L[V] =
      union match {
        case Union2L(e) => Xor.Right(e)
        case Union2R(e) => Xor.Left(Union1[R, V](e))
      }
  }

  implicit def Member3L[L[_], M[_], R[_]]: Member.Aux[L, In3[L, M, R], In2[M, R]] = new Member[L, In3[L, M, R]] { outer =>
    type Out = In2[M, R]

    def inject[V](tv: L[V]): Union[In3[L, M, R], V] =
      Union3L[L, M, R, V](tv)

    def accept[V](union: Union[Out, V]): Union[In3[L, M, R], V] =
      union match {
        case Union2L(e) => Union3M(e)
        case Union2R(e) => Union3R(e)
      }

    def project[V](union: Union[In3[L, M, R], V]): Union[Out, V] Xor L[V] =
      union match {
        case Union3L(e) => Xor.Right(e)
        case Union3M(e) => Xor.Left(Union2L[M, R, V](e))
        case Union3R(e) => Xor.Left(Union2R[M, R, V](e))
      }
  }

  implicit def Member4L[T[_], L[_], M[_], R[_]]: Member.Aux[T, Append[In1[T], In3[L, M, R]], In3[L, M, R]] = new Member[T, Append[In1[T], In3[L, M, R]]] { outer =>
    type Out = In3[L, M, R]

    def inject[V](tv: T[V]): Union[Append[In1[T], In3[L, M, R]], V] =
      UnionAppendL[In1[T], In3[L, M, R], V](Union1(tv))

    def accept[V](union: Union[Out, V]): Union[Append[In1[T], In3[L, M, R]], V] =
      UnionAppendR(union)

    def project[V](union: Union[Append[In1[T], In3[L, M, R]], V]): Union[Out, V] Xor T[V] =
      union match {
        case UnionAppendL(Union1(e)) => Xor.Right(e)
        case UnionAppendR(u)         => Xor.left(u)
      }
  }
}

trait MemberLower2 extends MemberLower3 {
  implicit def MemberAppend1L[T[_], R]: Member.Aux[T, Append[In1[T], R], R] = new Member[T, Append[In1[T], R]] {
    type Out = R

    def inject[V](e: T[V]): Union[Append[In1[T], R], V] =
      UnionAppendL(Union1(e))

    def accept[V](union: Union[Out, V]): Union[Append[In1[T], R], V] =
      UnionAppendR(union)

    def project[V](union: Union[Append[In1[T], R], V]): Union[Out, V] Xor T[V] =
      union match {
        case UnionAppendL(Union1(e)) => Xor.Right(e)
        case UnionAppendR(u)         => Xor.Left(u)
      }

  }

  implicit def MemberAppendL[T[_], L, R, U](implicit append: Member.Aux[T, L, U]): Member.Aux[T, Append[L, R], Append[U, R]] = new Member[T, Append[L, R]] {
    type Out = Append[U, R]

    def inject[V](effect: T[V]): Union[Append[L, R], V] =
      UnionAppendL(append.inject(effect))

    def accept[V](union: Union[Out, V]): Union[Append[L, R], V] =
      union match {
        case UnionAppendL(u) => UnionAppendL(append.accept(u))
        case UnionAppendR(u) => UnionAppendR(u)
      }

    def project[V](union: Union[Append[L, R], V]): Union[Out, V] Xor T[V] =
      union match {
        case UnionAppendL(u) => append.project(u).leftMap(UnionAppendL.apply)
        case UnionAppendR(u) => Xor.Left(UnionAppendR(u))
      }

  }
}

trait MemberLower3 extends MemberLower4 {

  implicit def Member2R[L[_], R[_]]: Member.Aux[R, In2[L, R], In1[L]] = new Member[R, In2[L, R]] { outer =>
    type Out = In1[L]

    def inject[V](tv: R[V]): Union[In2[L, R], V] =
      Union2R[L, R, V](tv)

    def accept[V](union: Union[Out, V]): Union[In2[L, R], V] =
      union match {
        case Union1(e) => Union2L(e)
      }

    def project[V](union: Union[In2[L, R], V]): Union[Out, V] Xor R[V] =
      union match {
        case Union2R(e) => Xor.Right(e)
        case Union2L(e) => Xor.Left(Union1[L, V](e))
      }

  }

  implicit def Member3M[L[_], M[_], R[_]]: Member.Aux[M, In3[L, M, R], In2[L, R]] = new Member[M, In3[L, M, R]] { outer =>
    type Out = In2[L, R]

    def inject[V](tv: M[V]): Union[In3[L, M, R], V] =
      Union3M[L, M, R, V](tv)

    def accept[V](union: Union[Out, V]): Union[In3[L, M, R], V] =
      union match {
        case Union2L(e) => Union3L(e)
        case Union2R(e) => Union3R(e)
      }

    def project[V](union: Union[In3[L, M, R], V]): Union[Out, V] Xor M[V] =
      union match {
        case Union3L(e) => Xor.Left(Union2L[L, R, V](e))
        case Union3M(e) => Xor.Right(e)
        case Union3R(e) => Xor.Left(Union2R[L, R, V](e))
      }
  }


  implicit def MemberAppendR[T[_], L, R, U](implicit append: Member.Aux[T, R, U]): Member.Aux[T, Append[L, R], Append[L, U]] = new Member[T, Append[L, R]] {
    type Out = Append[L, U]

    def inject[V](effect: T[V]): Union[Append[L, R], V] =
      UnionAppendR(append.inject(effect))

    def accept[V](union: Union[Out, V]): Union[Append[L, R], V] =
      union match {
        case UnionAppendL(u) => UnionAppendL(u)
        case UnionAppendR(u) => UnionAppendR(append.accept(u))
      }

    def project[V](union: Union[Append[L, R], V]): Union[Out, V] Xor T[V] =
      union match {
        case UnionAppendL(u) => Xor.Left(UnionAppendL(u))
        case UnionAppendR(u) => append.project(u).leftMap(UnionAppendR.apply)
      }

  }

}

trait MemberLower4 extends MemberLower5 {
  implicit def Member3R[L[_], M[_], R[_]]: Member.Aux[R, In3[L, M, R], In2[L, M]] = new Member[R, In3[L, M, R]] { outer =>
    type Out = In2[L, M]

    def inject[V](tv: R[V]): Union[In3[L, M, R], V] =
      Union3R[L, M, R, V](tv)

    def accept[V](union: Union[Out, V]): Union[In3[L, M, R], V] =
      union match {
        case Union2L(e) => Union3L(e)
        case Union2R(e) => Union3M(e)
      }

    def project[V](union: Union[In3[L, M, R], V]): Union[Out, V] Xor R[V] =
      union match {
        case Union3L(e) => Xor.Left(Union2L[L, M, V](e))
        case Union3M(e) => Xor.Left(Union2R[L, M, V](e))
        case Union3R(e) => Xor.Right(e)
      }
    }
}

trait MemberLower5 {

  implicit def MemberAppendAnyL[T[_], R]: Member.Aux[T, Append[In1[T], R], R] = new Member[T, Append[In1[T], R]] { outer =>
    type Out = R

    def inject[V](tv: T[V]): Union[Append[In1[T], R], V] =
      UnionAppendL[In1[T], R, V](Union1(tv))

    def accept[V](union: Union[Out, V]): Union[Append[In1[T], R], V] =
      UnionAppendR(union)

    def project[V](union: Union[Append[In1[T], R], V]): Union[Out, V] Xor T[V] =
      union match {
        case UnionAppendL(Union1(e)) => Xor.Right(e)
        case UnionAppendR(u)         => Xor.left(u)
      }
  }
}

/** prepend an effect on a list of existing effects (structured as a tree) */
trait Prepend[H[_], L] {
  type Out

  def inject[V](u: Union[L, V]): Union[Out, V]

  def decompose[V](u: Union[Out, V]): Union[L, V] Xor H[V]

  def member: Member.Aux[H, Out, L]
}

import Prepend._

object Prepend extends PrependLower1 {

  def apply[H[_], L, U](implicit prep: Prepend.Aux[H, L, U]): Aux[H, L, U] = prep

  /** proof that H is prepended to L, giving Out as a result */
  type Aux[H[_], L, U] = Prepend[H, L] { type Out = U }

  /** prepend 2 effects together */
  implicit def in2[H1[_], H2[_]]: Aux[H1, In1[H2], In2[H1, H2]] =
    new Prepend[H1, In1[H2]] {
      type Out = In2[H1, H2]

      def inject[V](u: Union[In1[H2], V]): Union[In2[H1, H2], V] =
        u match {
          case Union1(e) => Union2R(e)
        }

      def decompose[V](u: Union[In2[H1, H2], V]): Union[In1[H2], V] Xor H1[V] =
        u match {
          case Union2L(e) => Xor.right(e)
          case Union2R(e) => Xor.left(Union1(e))
        }

      def member: Member.Aux[H1, Out, In1[H2]] =
        Member.Member2L[H1, H2]
    }
}

trait PrependLower1 extends PrependLower2 {

  /** prepend 3 effects together */
  implicit def in3[H1[_], H2[_], H3[_]]: Aux[H1, In2[H2, H3], In3[H1, H2, H3]] =
    new Prepend[H1, In2[H2, H3]] {
      type Out = In3[H1, H2, H3]

      def inject[V](u: Union[In2[H2, H3], V]): Union[In3[H1, H2, H3], V] =
        u match {
          case Union2L(e) => Union3M(e)
          case Union2R(e) => Union3R(e)
        }

      def decompose[V](u: Union[In3[H1, H2, H3], V]): Union[In2[H2, H3], V] Xor H1[V] =
        u match {
          case Union3L(e) => Xor.right(e)
          case Union3M(e) => Xor.left(Union2L(e))
          case Union3R(e) => Xor.left(Union2R(e))
        }

      def member: Member.Aux[H1, Out, In2[H2, H3]] =
        Member.Member3L[H1, H2, H3]
    }
}


trait PrependLower2 extends PrependLower3 {
  /** prepend 4 effects together */
  implicit def in4[H1[_], H2[_], H3[_], H4[_]]: Aux[H1, In3[H2, H3, H4], Append[In1[H1], In3[H2, H3, H4]]] =
    new Prepend[H1, In3[H2, H3, H4]] {
      type Out = Append[In1[H1], In3[H2, H3, H4]]

      def inject[V](u: Union[In3[H2, H3, H4], V]): Union[Append[In1[H1], In3[H2, H3, H4]], V] =
        UnionAppendR(u)

      def decompose[V](u: Union[Append[In1[H1], In3[H2, H3, H4]], V]): Union[In3[H2, H3, H4], V] Xor H1[V] =
        u match {
          case UnionAppendL(Union1(e)) => Xor.right(e)
          case UnionAppendR(u1)        => Xor.left(u1)
        }

      def member: Member.Aux[H1, Append[In1[H1], In3[H2, H3, H4]], In3[H2, H3, H4]] =
        Member.MemberAppendAnyL[H1, In3[H2, H3, H4]]
    }
}


trait PrependLower3 extends PrependLower4 {

  /** prepend 1 effect on top of two appended lists where the first one is In1 */
  implicit def append1[H1[_], H2[_], L]: Aux[H1, Append[In1[H2], L], Append[In2[H1, H2], L]] =
    new Prepend[H1, Append[In1[H2], L]] {
      type Out = Append[In2[H1, H2], L]

      def inject[V](u: Union[Append[In1[H2], L], V]): Union[Append[In2[H1, H2], L], V] =
        u match {
          case UnionAppendL(Union1(e)) => UnionAppendL(Union2R(e))
          case UnionAppendR(l)         => UnionAppendR(l)
        }

      def decompose[V](u: Union[Append[In2[H1, H2], L], V]): Union[Append[In1[H2], L], V] Xor H1[V] =
        u match {
          case UnionAppendL(Union2L(e)) => Xor.right(e)
          case UnionAppendL(Union2R(e)) => Xor.left(UnionAppendL(Union1(e)))
          case UnionAppendR(u1)         => Xor.left(UnionAppendR(u1))
        }

      def member: Member.Aux[H1, Out, Append[In1[H2], L]] =
        Member.MemberAppendL[H1, In2[H1, H2], L, In1[H2]]
    }
}

trait PrependLower4 extends PrependLower5 {

  /** prepend 1 effect on top of two appended lists where the first one is In2 */
  implicit def append2[H1[_], H2[_], H3[_], L]: Aux[H1, Append[In2[H2, H3], L], Append[In3[H1, H2, H3], L]] =
    new Prepend[H1, Append[In2[H2, H3], L]] {
      type Out = Append[In3[H1, H2, H3], L]

      def inject[V](u: Union[Append[In2[H2, H3], L], V]): Union[Append[In3[H1, H2, H3], L], V] =
        u match {
          case UnionAppendL(Union2L(e)) => UnionAppendL(Union3M(e))
          case UnionAppendL(Union2R(e)) => UnionAppendL(Union3R(e))
          case UnionAppendR(r)          => UnionAppendR(r)
        }

      def decompose[V](u: Union[Append[In3[H1, H2, H3], L], V]): Union[Append[In2[H2, H3], L], V] Xor H1[V] =
        u match {
          case UnionAppendL(Union3L(e)) => Xor.right(e)
          case UnionAppendL(Union3M(e)) => Xor.left(UnionAppendL(Union2L(e)))
          case UnionAppendL(Union3R(e)) => Xor.left(UnionAppendL(Union2R(e)))
          case UnionAppendR(u1)         => Xor.left(UnionAppendR(u1))
        }

      def member: Member.Aux[H1, Out, Append[In2[H2, H3], L]] =
        Member.MemberAppendL[H1, In3[H1, H2, H3], L, In2[H2, H3]]
    }

}

trait PrependLower5 {

  /** prepend 1 effect on top of two appended lists */
  implicit def append[H[_], L, R]: Prepend.Aux[H, Append[L, R], Append[Append[In1[H], L], R]] =
    new Prepend[H, Append[L, R]] {
      type Out = Append[Append[In1[H], L], R]

      def inject[V](u: Union[Append[L, R], V]): Union[Append[Append[In1[H], L], R], V] =
        u match {
          case UnionAppendL(l) => UnionAppendL(UnionAppendR(l))
          case UnionAppendR(r) => UnionAppendR(r)
        }

      def decompose[V](u: Union[Append[Append[In1[H], L], R], V]): Union[Append[L, R], V] Xor H[V] =
        u match {
          case UnionAppendL(UnionAppendL(Union1(e))) => Xor.right(e)
          case UnionAppendL(UnionAppendR(l))         => Xor.left(UnionAppendL(l))
          case UnionAppendR(r)                       => Xor.left(UnionAppendR(r))
        }

      def member: Member.Aux[H, Append[Append[In1[H], L], R], Append[L, R]] =
        Member.MemberAppendL[H, Append[In1[H], L], R, L]

    }


}
