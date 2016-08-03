package org.atnos.eff


object EffectsTest {

  case class Option1[A](a: A)
  case class Option2[A](a: A)
  case class Option3[A](a: A)
  case class Option4[A](a: A)
  case class Option5[A](a: A)
  case class Option6[A](a: A)
  case class Option7[A](a: A)

  type S1 = Option1 |: NoEffect
  type S2 = Option1 |:: Option2
  type S3 = Option1 |:  Option2 |:: Option3
  type S4 = Option1 |:  Option2 |:  Option3 |:: Option4
  type S5 = Option1 |:  Option2 |:  Option3 |:  Option4 |:: Option5
  type S6 = Option1 |:  Option2 |:  Option3 |:  Option4 |:  Option5 |:: Option6
  type S7_ = Option1 |:  Option2 |:  Option3 |:  Option4 |:  Option5 |:  Option6 |:: Option7

  val s7 = Fx[S7_]
  type S7 = s7.Fx

  def testIn[F[_], R](a: F[Int])(implicit m1: MemberIn[F, R]): R =
    ???

  def test[F[_], R](a: F[Int])(implicit m1: Member[F, R]): R =
    ???

  def runOption1[R, U](e: Eff[R, Int])(implicit m: Member.Aux[Option1, R, U]): Eff[U, Int] =
    ???

  def runOption2[R](e: R)(implicit m: Member[Option2, R]): m.Out =
    ???

  import syntax.eff._
  def effect[R]: Eff[R, Int] = ???

  effect[In1[Option]].into[In1[Option]]
  effect[In1[Option2]].into[In2[Option1, Option2]]
  effect[In1[Option1]].into[In2[Option1, Option2]]
  effect[In2[Option1, Option2]].into[In2[Option1, Option2]]
  effect[In2[Option2, Option1]].into[In2[Option1, Option2]]

  effect[In1[Option1]].into[In3[Option1, Option2, Option3]]
  effect[In1[Option2]].into[In3[Option1, Option2, Option3]]
  effect[In1[Option3]].into[In3[Option1, Option2, Option3]]

  effect[In2[Option1, Option2]].into[In3[Option1, Option2, Option3]]
  effect[In2[Option1, Option3]].into[In3[Option1, Option2, Option3]]
  effect[In2[Option3, Option1]].into[In3[Option1, Option2, Option3]]
  effect[In2[Option2, Option1]].into[In3[Option1, Option2, Option3]]
  effect[In2[Option2, Option3]].into[In3[Option1, Option2, Option3]]
  effect[In2[Option3, Option2]].into[In3[Option1, Option2, Option3]]

  effect[In3[Option1, Option2, Option3]].into[In3[Option1, Option2, Option3]]
  effect[In3[Option2, Option1, Option3]].into[In3[Option1, Option2, Option3]]

  effect[In1[Option1]].into[Append[In1[Option1], In3[Option2, Option3, Option4]]]
  effect[In2[Option1, Option2]].into[Append[In1[Option1], In3[Option2, Option3, Option4]]]
  effect[In2[Option2, Option3]].into[Append[In1[Option1], In3[Option2, Option3, Option4]]]
  effect[In3[Option2, Option3, Option4]].into[Append[In1[Option1], In3[Option2, Option3, Option4]]]


  testIn[Option1, S1](Option1(1))
  testIn[Option2, S2](Option2(1))

  testIn[Option1, S3](Option1(1))
  testIn[Option2, S3](Option2(1))
  testIn[Option3, S3](Option3(1))

  testIn[Option1, S4](Option1(1))
  testIn[Option2, S4](Option2(1))
  testIn[Option3, S4](Option3(1))
  testIn[Option4, S4](Option4(1))

  testIn[Option1, S5](Option1(1))
  testIn[Option2, S5](Option2(1))
  testIn[Option3, S5](Option3(1))
  testIn[Option4, S5](Option4(1))
  testIn[Option5, S5](Option5(1))

  testIn[Option1, S6](Option1(1))
  testIn[Option2, S6](Option2(1))
  testIn[Option3, S6](Option3(1))
  testIn[Option4, S6](Option4(1))
  testIn[Option5, S6](Option5(1))
  testIn[Option6, S6](Option6(1))

  testIn[Option1, S7](Option1(1))
//  testIn[Option2, S7](Option2(1))
//  testIn[Option3, S7](Option3(1))
//  testIn[Option4, S7](Option4(1))
//  testIn[Option5, S7](Option5(1))
//  testIn[Option6, S7](Option6(1))
//  testIn[Option7, S7](Option7(1))

  test[Option1, S2](Option1(1))
  test[Option2, S2](Option2(1))

  test[Option1, S3](Option1(1))
  test[Option2, S3](Option2(1))
  test[Option3, S3](Option3(1))

  test[Option1, S4](Option1(1))
  test[Option2, S4](Option2(1))
  test[Option3, S4](Option3(1))
  test[Option4, S4](Option4(1))

//  runOption2(runOption1(test[Option1, S2](Option1(1))))
//  runOption1(runOption2(test[Option1, S2](Option1(1))))


//  case class Wr[A, B](b: B)
//  type W[A] = Wr[String, A]
//  type SW = W |: NoEffect
//
//  def write: Eff[SW, Unit] =
//    Eff.send(Wr[String, Unit](()))
//
//  def runOption1Eff[R, U, A](e: Eff[R, A])(implicit m: Member.Aux[W, R, U]): Eff[U, A] =
//    ???
//
//  Eff.run(runOption1Eff(write))
//  import syntax.all._
//  import all._
//
//  def functionReader[R, U, A, B](f: A => Eff[R, B])(implicit p: Prepend.Aux[Reader[A, ?], U, R], into: IntoPoly[R, U], m: MemberIn[Reader[A, ?], U]): Eff[U, B] =
//    ask[U, A].flatMap(f(_).into[U])
//
//  type S = Option |: NoEffect
//
//  type SX = Reader[String, ?] |: S
//
//  val a: Eff[In1[Option], Int] = OptionEffect.some(1)
//
//  val b = functionReader((s: String) => a.map(_ + s.size))
//
//  b.runReader("start").runOption.run


}

