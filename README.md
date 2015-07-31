# purescript-bundle-fast

Tested with PureScript version 0.7.1.0


## Synopsis

A fast alternative to Purescript's `psc-bundle` to be used during development.


## About

One great thing about programming in JavaScript is the speed of development.
Just edit your source code file, and immediately reload your browser to
instantly see your changes.

With PureScript, you must go through a compilation process. Even if it only
takes a few seconds, the lag becomes frustrating when trying to iterate
rapidly.

But we can try to bring the compilation time down to almost nothing! This
project manages to do so for the `psc-bundle` stage of compilation. It is a
tool called `psc-bundle-fast` that replaces the official `psc-bundle` tool that
comes with PureScript.

`psc-bundle-fast` should be used only during development. For production you
should still use the official `psc-bundle` since it does dead code elimination
and will produce smaller output files.


## Benchmarks

So how much faster is it? Results for a sample project:

| Command         | Time   | Output .js Size
| --------------- | ------ | ---------------
| psc-bundle      | 1.458s |            108K
| psc-bundle-fast | 0.091s |            464K

That's 16x faster! It's bigger because it contains lots of library code that is
not being used (regular `psc-bundle` strips this out). But for local
development, the larger file size has negligible impact on load time, and no
impact on performance.

### What about browserify and webpack?

They are even slower than PureScript's `psc-bundle`. Feel free to run your own
benchmarks (and tell us the results!)


## Installation

You need GHC and cabal.

    $ cabal update
    $ cabal install purescript-bundle-fast


## Example Usage

First, use `psc` as usual to compile your program:

    $ psc     './bower_components/**/src/*.purs' \
        --ffi './bower_components/**/src/*.js' \
              './src/**/*.purs' \
        --ffi './src/**/*.js' \
        -o output

Now, just as a refresher, here is how we'd use the regular `psc-bundle`

    $ psc-bundle './output/**/*.js' -m Main --main Main -o app.js

And here is how you would use `psc-bundle-fast` instead of the previous step:

    $ psc-bundle-fast -i output -m Main --main Main -o app.js


## Differences with `psc-bundle` and limitations

Unlike `psc-bundle`, `psc-bundle-fast` does not use a real JavaScript parser.
Therefore:

1. It is not able to perform dead code elimination the way that `psc-bundle`
   does, so it will produce output files that are larger.

2. It will not detect syntax errors in `foreign.js` files. (This is actually an
   advantage since the error messages that `psc-bundle` generates are
   confusing. It's more helpful to see the error that the browser shows).

3. `foreign.js` files that use `require` to load external JavaScript
   modules/libraries will not work. These `foreign.js` files will load, but if
   they are executed then an error will be triggered. If you need to a
   PureScript library that has such `require` usage, then you will need to
   externally load the required JavaScript library, and then create a stub
   function called "require" that hooks into it. (If you succeed to do this
   then share with us how you did it!)

4. The custom parser that `psc-bundle-fast` uses is brittle and relies on the
   specific format that `psc` outputs. If `psc` ever makes (even slight)
   changes to its output then `psc-bundle-fast` will break.


## Usage

```
psc-bundle-fast - Bundles compiled PureScript modules for the browser (fast
version, for development)

Usage: psc-bundle-fast (-i|--input-dir DIR) [-o|--output FILE]
                       (-m|--module MODULE) [--main MODULE] [-n|--namespace ARG]

Available options:
  --version                Show the version number
  -h,--help                Show this help text
  -i,--input-dir DIR       The directory containing the compiled modules. This
                           directory should contain a subdirectory for each
                           compiled module(with the name of the module), and
                           within each of those there should be an index.js (and
                           optional foreign.js) file. The psc compiler usually
                           calls the desired directory "output"
  -o,--output FILE         The output .js file (Default is stdout)
  -m,--module MODULE       Entry point module name(s). All code which is not a
                           transitive dependency of an entry point module will
                           be removed.
  --main MODULE            Generate code to run the main method in the specified
                           module.
  -n,--namespace ARG       Specify the namespace that PureScript modules will be
                           exported to when running in the
                           browser. (default: "PS")
```
