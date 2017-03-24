


* goal
    * give the user freedom to do something when they know that what they are doing
    * give the user the static expressiveness when they want to ensure something


* steps to bootstrap
    * write a parser in Scala
    * write a emitter in Scala to llvm + gc
        * llvm
    * write eval in C
    * link the generated code with new eval in C, bootstrap a new compiler -_-
    * bootstrap the emitter, structural editor in a not type checked way
    * write the other code