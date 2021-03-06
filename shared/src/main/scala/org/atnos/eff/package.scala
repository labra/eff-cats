package org.atnos

package object eff {

  type <=[M[_], R] = Member.<=[M, R]
  type |=[M[_], R] = MemberIn.|=[M, R]


  object member {
    implicit def outMember[R, U, O[_], T[_]](implicit m: Member.Aux[O, R, U], t: MemberIn[T, R]): MemberIn[T, U] =
      m.out[T]
  }

  object eff        extends EffCreation        with EffInterpretation with Effects
  object reader     extends ReaderCreation     with ReaderInterpretation
  object writer     extends WriterCreation     with WriterInterpretation
  object state      extends StateCreation      with StateInterpretation
  object eval       extends EvalCreation       with EvalInterpretation
  object option     extends OptionCreation     with OptionInterpretation
  object list       extends ListCreation       with ListInterpretation
  object xor        extends XorCreation        with XorInterpretation
  object validate   extends ValidateCreation   with ValidateInterpretation
  object choose     extends ChooseCreation     with ChooseInterpretation
  object future     extends FutureCreation     with FutureInterpretation

  object create extends
    ReaderCreation with
    WriterCreation with
    StateCreation with
    EvalCreation with
    OptionCreation with
    ListCreation with
    XorCreation with
    ValidateCreation with
    ChooseCreation with
    FutureCreation with
    EffCreation with
    Effects

  object all extends
    ReaderEffect with
    WriterEffect with
    StateEffect with
    EvalEffect with
    OptionEffect with
    ListEffect with
    XorEffect with
    ValidateEffect with
    ChooseEffect with
    FutureEffect with
    EffInterpretation with
    EffCreation with
    EffImplicits with
    Effects

  object interpret extends
    Interpret

}
