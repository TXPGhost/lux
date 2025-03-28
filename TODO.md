# TODO List

1. Prevent mixing of named and unnamed members by splitting into two types.
2. Support recursive types and functions by "differing" the evaluation of
   identifiers. Assume the types are "never" types for type checking purposes.
3. Make separate AST, one which has types and one which has only values
4. Context identifiers need to be aware of their scope, to prevent clashes.
