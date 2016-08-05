package org.atnos.eff

import cats.data.Xor._
import cats.data._
import cats.std.all._
import cats.syntax.all._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.specs2.{ScalaCheck, Specification}
import org.scalacheck.Gen


class ValidateEffectSpec extends Specification with ScalaCheck { def is = s2"""

 run the validate effect                     $validateOk
 run the validate effect with nothing        $validateKo

 run is stack safe with Validate  $stacksafeRun

 wrong values can be caught and transformed to a correct value $wrongToCorrect

"""

  def validateOk = {
    type S = ValidateString |: NoEffect

    val validate: Eff[S, Int] =
      for {
        _ <- ValidateEffect.correct[S, String, Int](1)
        _ <- ValidateEffect.correct[S, String, Int](2)
        a <- EffMonad[S].pure(3)
      } yield a

    validate.runNel.run ==== Right(3)
  }

  def validateKo = {
    type S = ValidateString |: NoEffect

    val validate: Eff[S, Int] =
      for {
        _ <- ValidateEffect.correct[S, String, Int](1)
        _ <- ValidateEffect.wrong[S, String]("error!")
        a <- EffMonad[S].pure(3)
      } yield a

    validate.runNel.run ==== Left(NonEmptyList("error!"))
  }

  type ValidateString[A] = Validate[String, A]

  def stacksafeRun = {
    type E = ValidateString |: NoEffect

    val list = (1 to 5000).toList
    val action = list.traverseU(i => ValidateEffect.wrong[E, String](i.toString))

    action.runNel.run ==== NonEmptyList.fromList(list.map(_.toString)).map(Xor.left).getOrElse(Xor.right(Nil))
  }
  
  def wrongToCorrect = prop { i: Int =>
    case class TooBig(value: Int)
    type D[A] = Validate[TooBig, A]
    type E = D |: NoEffect

    val i = 7

    val value: Eff[E, Int] =
	  validateValue(i > 5, i, TooBig(i))

    val action: Eff[E, Int] = catchWrong[E, TooBig, Int](value) { case OneAnd(TooBig(k),_) =>
	  validateValue(k < 10, k, TooBig(k))
    }

    val expected: NonEmptyList[TooBig] Xor Int =
      if (i < 10) Xor.right(i) else Xor.left(NonEmptyList(TooBig(i)))

    val actual: NonEmptyList[TooBig] Xor Int =
      action.runNel.run

    actual == expected

  }.setGen(Gen.oneOf(14, 12))


}

