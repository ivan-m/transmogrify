Have you ever wanted to do something like this?

```haskell
λ> cons 'a' (1::Int, 2::Word, 3::Double) :: (Char, Int, Word, Double)
('a',1,2,3.0)
```

Or how about this?

```haskell
λ> unsnoc ('a',1::Int,2::Word,3.0::Double) :: ((Char, Int, Word), Double)
(('a',1,2),3.0)
```

Let me try to completely confuse you (and potentially give a hint as
to what I'm doing):

```haskell
λ> transmogrify ('H', 'a', 's', 'k', 'e', 'l', 'l') :: ((Char, Char), Char, (Char, Char, (Char, Char)))
(('H','a'),'s',('k','e',('l','l')))
```

One more hint:

```haskell
λ> data Foo = Bar Char Char Char deriving (Show, Generic)
λ> transmogrify ('a', 'b', 'c') :: Foo
Bar 'a' 'b' 'c'
```

You read that right
-------------------

![TRANSMOGRIFICATION!!!](https://s-media-cache-ak0.pinimg.com/originals/a5/68/15/a56815a9045b227920b4bf6a2a11b67b.gif)

What do you mean by that?
-------------------------

I've suddenly become really interested in [GHC Generics], and it
occurred to me the other day that -- since it basically decomposes
more interesting types to products and sums with lots of associated
metadata -- that it should be possible to get two different types that
are fundamentally the same shape but lots of different pesky metadata.

[GHC Generics]: https://hackage.haskell.org/package/base/docs/GHC-Generics.html

Turns out, it _is_ possible.  I've got a prototype of a
little [library](https://github.com/ivan-m/transmogrify) that
implements this on GitHub, and that's what I used for those examples
above.

How it works
------------

Basically, all metadata (constructor names, record aliases, strictness
annotations, etc.) is stripped out.  This is done recursively
throughout the entire type, stopping at fundamental types like `Int`
and `Char`.  To cap it all off, products and converted from a
tree-like implementation into an explicit list (this is even done
recursively for any products contained within products, like the
nested tuples above).

When will this be on Hackage?
-----------------------------

I doubt it will be.

The approach is a bit hacky, with various type-classes required
including type aliases, etc.  That's not _too_ bad, but there is
pretty much no type safety or inference available (hence all the
explicit annotations above).

The performance is also not great: it's fundamentally `O(n)`, and
there's no way to really fix this (at least that I can see).

There are also currently two limitations with the implementation:

1) No handling of sum-types.  This could be remedied by basically
   copying and modifying the existing handling of product types.

2) An explicit list of types is needed to be able to stop type
   recursion; this is currently limited to numeric types and `Char`.

This second limitation is the biggest fundamental problem with how to
get this to a production-ready library.  Ideally you could specify
"this type should not be examined".  Even better: if a component type
doesn't have a `Generic` instance then don't bother trying to split it
apart.

So, now what?
-------------

Well, the code is there.  If there's enough interest I might try and
clean it up and put it on Hackage regardless.

But if you think this will somehow solve all your problems, then maybe
you should re-think what you're doing ;-)
