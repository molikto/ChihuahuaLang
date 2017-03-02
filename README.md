# ChihuahuaLang


reasons this project exists:

to experiment a new idea about strutural editor: *correct by construction syntax is a myth. we don't need it and it is harmful*. it is too strict. instead, use **loosely typed** tree. the node has a potential type, but the childs and child count is not rigid


## design


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

# analysis of traditional editing

text is a sequence of characters [0, n), when edit, their is a cursor position,[0, n]

a keyboard stroke will produce
  1. add char after the cursor, and move the cursor
  2. delete the element before/after the cursor, and move the cursor
  3. selection and copy paste

# analysis tree editing

1. selection: you can select a node,
 but sometimes it is also helpful if you can select multiple child nodes of a node
 then pasting is wired, suppose you are selecting a node, you can
   1. paste the content in current node, if you only have one node to paste
   2. paste the content as childs
   3. paste the content as sibling
1. their is no more empty string
2. cursor position has became wired

# copy pasting and re-assgin of meanings -- this has problem now

# grow point

there is more than trees.
a `GrowPoint` is a special kind of tree such that
1. a tree of form
1. it is a placeholder and doesn't interface with the semantics
1. it must be empty: no form, no childs, no content
2. it will be hidden when cursor is not on it, but movement can still move to a grow point and
you can potentially add more trees, they are always shown inside a tree


