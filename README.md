# ChihuahuaLang


reasons this project exists:


*  use **loosely typed** tree. the node has a potential type, but the childs and child count is not rigid
    *  *correct by construction syntax is a myth. we don't need it and it is harmful*. it is too strict
    * textural editor is bad


## design


### `Tree` and `SyntaxForm`

the central editing block as `Tree`, roughly `{content: T, childs: Seq[Tree]}`. not a specific typed `Ast`
 (abstract **syntax** tree), only a tree.

tree has a `var form: Option[SyntaxForm]`. this thing defines the preferred form of the tree content and
the relationship with childs. and this is changeable and optional

the `content` of a tree is obtained by user input, it is actually called `command`. because we need to retain
the ability to convert the content of a tree to and from an editable state

the editing of the tree is `form`-directed, but it is not rigid, you can still break out the constrains from the form.

the `SyntaxForm` has two transform: to `Widget`. which is how we render things, another is to `Ast`, which is how,
the compiler consumes things

of all the things we said above, `Tree` is a `object`, `SyntaxForm` is all constant. `Widget`s and `Ast` is value, and
is functional. so almost all the states is contained in the `Tree` object


