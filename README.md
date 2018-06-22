# Liquorice

*Liquorice* is a Haskell embedded domain-specific language (eDSL) for the
algorithmic construction of maps for the computer game "Doom".
The design of *Liquorice* is heavily
influenced by [Wad Language (WadC)](https://jmtd.net/wadc/), which in turn owes
a debt to [LOGO](http://el.media.mit.edu/logo-foundation/what_is_logo/logo_programming.html).

A *Liquorice* program is a series of instructions to move a virtual "turtle" or
"pen".  The state of the pen, and the structures it has drawn, are captured in
a `Context`.

*Liquorice* was created by [Jonathan Dowland](https://jmtd.net/) and is
distributed under the terms of the GNU Public License, version 3 (see
[COPYING](COPYING)).

## Quick example

    -- simple example, triangle (for orientation); unique texture per line
    import Liquorice.Monad
    import Liquorice.Render

    main = buildWad "example1.wad" $ runWadL $ do
        mid "ZZWOLF1"
        draw 128 0
        mid "ZZWOLF2"
        draw 0 128
        turnaround
        mid "ZZWOLF3"
        draw 128 128
        rightsector 0 128 160
        turnaround
        step 64 32
        thing

## Evaluation

The commands that a typical *Liquorice* program will use are predominantly
monadic, and so make use of Haskell's "do-notation" for ordering. Internally,
the majority of these monadic functions are wrappers around pure equivalents
which transform an input `Context` type into an output, e.g.:

    xoff :: Int -> Context -> Context
    xoff x c = c { paletteXoff = x }

    place :: Int -> Int -> (Context -> Context) -> Context -> Context
    place x y fn c = c & step x y
                       & fn
                       & step (-1 * x) (-1 * y)

The pure functions can be combined using the infix operator `&` (from
`Data.Function`), or the more usual composition operator (which reads
back-to-front). However, the monadic versions are probably more user-
friendly, and so the separate pure implementations might go away or
stop being explicitly exported at some point. We also probably need to
use the monadic versions if we want to introduce randomness or debug IO
in the middle of a program.

A series of monadic Liquorice statements are converted into a final `Context`
via `runWadL`. A `Context` is written to an output PWAD file via

    buildWad :: FilePath -> Context -> IO ()

Internally, `buildWad` first converts a `Context` into an intermediate data
structure `WadMap`, which closely resembles the binary structure of a PWAD.
(`WadMap` itself is a specialisation of `Wad`, imposing the presence of map
 specific lumps)

## More detailed example

See [birds.hs](birds.hs), A re-implementation/transformation of ["Bird Cage"
for WadC](https://redmars.org/wadc/examples/#_birds_wl) for an example program
that generates a complete playable map. The map targets Doom 1 / The
Ultimate Doom (map slot E2M8), and requires a doom engine with raised
engine limits (but no other special features)

The generated PWAD, with nodes, ready to play: [birds.zip](https://redmars.org/liquorice/birds.zip)

## Pros/Cons

Pros:

* Unlike WadC, you have full access to the Context at any point in your
  program, which you can inspect or transform as you wish.
* You also have the full power of the Haskell programming language.
* In particular simply infix numeric operators are hugely useful.

Cons:

* There's no GUI.
* The Pure-versus-Monadic functions are a bit awkward and I might get rid of the
pure versions altogether in the future
* There are some slow parts (line splitting in particular)
* If you don't know Haskell, this will be very hard to use and most error messages
will make little sense.
* You need a Haskell development environment installed to use *Liquorice*.

## Code overview

The following three source files are the ones that you will want to import
as modules to your Liquorice program:

 * `Liquorice.hs` — The main `Context` definition and most basic pure operations
 * `Liquorice/Monad.hs` — monadic wrappers around the above, + some more
 * `Liquorice/Render.hs` — exports `buildWad` for exporting a PWAD

These are back-end implementation details:

 * `Liquorice/Pure.hs` — most of the "standard library", in pure functions
 * `Liquorice/Line.hs` — `Line` type, line splitting and tests
 * `Doom/Wad.hs` — Wad and WadMap definitions and binary serialisation
 * `TestMain.hs` — HTF test harness

## See also

 * [Jonathan Dowland's homepage](https://jmtd.net/)
 * [WadC](https://jmtd.net/wadc/)
 * [The Doom Wiki](https://doomwiki.org/)
