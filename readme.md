# explain-instance

This is a prototype implementation of explanations for Haskell's
instance resolution and type errors during instance resolution. It
uses Template Haskell to create modified copies of typeclasses and
instances.  These classes have a single method, which provides a tree
of instance resolution information.

Please see [my blog post][] for more information.

[my blog post]: https://mgsloan.com/posts/inspecting-haskell-instance-resolution
