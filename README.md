
this is my

## Programming Language Theory Playground


code here may not be complete or working at all


main stuff
* [a structural editor](gifs/editor)

   ![editor](gifs/editor/Mar-03-2017%2019-27-40.gif)
* [dependent type theory type checker and compiled (JIT'ed) normalization by evaluation](common/src/main/scala/TypeCheck.scala)
    * it also implements subtyping for dependent records, and inductive types, but the soundness is not verified yet
    * [code sample](blob/master/library/prelude.edt), you can run it in `Parser.java`
    * [tests](blob/master/common/src/main/scala/TypeCheck.scala#L973)
    * references
        * *Full reduction at full throttle*
        * *A simple type-theoretic language: Mini-TT*

smaller stuff
* [eval for untyped lambda calculus and compiled (JIT'ed) normalization by evaluation](common/src/main/scala/UntypedLambdaCalculus.scala) (*Full reduction at full throttle*)
