


* goal
    * give the user freedom to do something when they know that what they are doing
    * give the user the static expressiveness when they want to ensure something
    * good code should read as novel (good math books should read like a novel)

* be reasonable
    * proof checkers
        * but how much it can be automated is a problem
        * how much it can extract useful algorithm is also a problem
    * certificated programming -- not so much
    * normal programming language
        * how to separate?


* steps to bootstrap (as a language)
    * write a parser in Scala
    * write a emitter in Scala to llvm + gc
        * llvm
    * write eval in C
    * link the generated code with new eval in C, bootstrap a new compiler -_-
    * bootstrap the emitter, structural editor in a not type checked way
    * write the other code

* designing a general purpose language
    * mutation is hard
        * what about evaluation order? can you reorder fields? that's actually a question...
        * what about memorization?
        * what about side effects?
        * seems no matter how you think of it, splitting the language into pure/inpure part will be too constraining


* combined with a structural editor
    * the auto-completion as GUI framework, dependent type checking as the validation process - so whatever user input is correct


* ad-hoc polymorphism or type-classes








