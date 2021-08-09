title: Guile 3 & Guix
date: 2020-01-24 15:00
author: Ludovic Courtès
tags: Scheme API, Programming interfaces, Performance
slug: guile-3-and-guix
---

Version 3.0 of GNU Guile, an implementation of the [Scheme programming
language](https://schemers.org), [was released just last
week](https://www.gnu.org/software/guile/news/gnu-guile-300-released.html).
This is a major milestone for Guile, which gets compiler improvements
and just-in-time (JIT) native code generation, leading to significant
performance improvements over 2.2.  It’s also great news for all the
users of Guile, and in particular for Guix!

![Guile 3 logo.](https://guix.gnu.org/static/blog/img/guile-3.png)

This post discusses what it means for Guix to migrate to Guile 3 and
how that migration is already taking place.

# Guile in Guix

Most users interact with Guix through its command-line interface, and we
work hard to make it as approachable as possible.  As any user quickly
notices, Guix uses the Scheme programming language uniformly for its
configuration—from
[channels](https://guix.gnu.org/manual/devel/en/html_node/Channels.html)
to
[manifests](https://guix.gnu.org/manual/devel/en/html_node/Invoking-guix-package.html#profile_002dmanifest)
and [operating
systems](https://guix.gnu.org/manual/devel/en/html_node/Using-the-Configuration-System.html)—and
anyone who starts packaging software knows that [package
definitions](https://guix.gnu.org/manual/devel/en/html_node/Defining-Packages.html)
are in fact Scheme code as well.

This is a significant departure from many other, and in particular from
[Nix](https://nixos.org/nix/).  While Nix defines several
domain-specific languages (DSLs) for these aspects—the [Nix
language](https://nixos.org/nix/manual/#chap-writing-nix-expressions)
but also specific
[configuration](https://nixos.org/nix/manual/#sec-conf-file)
[languages](https://nixos.org/nix/manual/#chap-distributed-builds)—Guix
chooses Scheme as the single language for all this, [together with the
definition of high-level embedded domain-specific languages
(EDSLs)](https://hal.inria.fr/hal-00824004/en).

It goes beyond that: in Guix System, all the things traditionally
implemented in C or as a set of Perl or shell scripts are implemented in
Scheme.  That includes [the init
system](https://www.gnu.org/software/shepherd/), [package
builds](https://guix.gnu.org/manual/en/html_node/Build-Systems.html),
[the initial RAM disk
(initrd)](https://guix.gnu.org/manual/en/html_node/Initial-RAM-Disk.html),
[system tests](https://guix.gnu.org/blog/2016/guixsd-system-tests/), and
more.  Because this leads to several layers of Scheme code, executed at
different points in time, Guix includes a [_code staging_
mechanism](https://hal.inria.fr/hal-01580582/en) built upon the nice
properties of Scheme.

Why do that?  The arguments, right from the start, were twofold: using a
general-purpose language allows us to benefit from its implementation
tooling, and having interfaces for “everything” in Scheme makes it easy
for users to navigate their distro or OS code and to reuse code to build
new features or applications.  Guix developers benefit from the ease of
code reuse every day; demonstrative examples include the [use of Guix
container facilities in the init
system](https://guix.gnu.org/blog/2017/running-system-services-in-containers/),
the development of
[many](https://guix.gnu.org/manual/devel/en/html_node/Development.html)
[tools](https://guix.gnu.org/manual/devel/en/html_node/Utilities.html)
providing facilities around packages, the implementation of additional
[user](https://emacs-guix.gitlab.io/website/)
[interfaces](https://github.com/UMCUGenetics/hpcguix-web/), and work on
applications that use Guix as a library such as the [Guix Workflow
Language](https://www.guixwl.org/) and
[Guix-Jupyter](https://hpc.guix.info/blog/2019/10/towards-reproducible-jupyter-notebooks/).

As for the benefits of the host general-purpose language, these are
rather obvious: Guix developers benefit from an expressive language, an
optimizing compiler, a debugger, a powerful [read-eval-print loop
(REPL)](https://www.gnu.org/software/guile/manual/html_node/Using-Guile-Interactively.html),
an [interactive development environment](https://nongnu.org/geiser/),
and all sorts of libraries.  Moving to Guile 3 should add to that better
performance, essentially for free.  To be comprehensive, Guile 3 may
well come with a set of brand new bugs too, but so far we seem to be
doing OK!

# Migrating to Guile 3

What does it mean for Guix to migrate to Guile 3?  We’ve seen above
different ways in which Guix relies on Guile.  In short, we can say that
migration is threefold:

  1. Guix is a distro that ships Guile-related packages.  Like any other
     distro, it will have to upgrade its `guile` package to 3.0 and to
     ensure packages that depend on it and updated as well.
  2. Guix is a program written in Guile.  As such, we need to make sure
     that all its dependencies (half a dozen of Guile libraries) work
     with Guile 3 and that Guix itself runs fine with Guile 3.
  3. Guix ties together operating system components.  In particular, the
     init system (the Shepherd) and other boot-time facilities will also
     migrate.

## The packages

Updating the distro is the boring part, but it’s best to get it right.
Guix makes it possible to have unrelated versions of variants of
packages in different environments or different profiles, which is very
nice.  We’ll have performed a smooth transition if users and tools see
that the packages named `guile` and `guile-ssh` (say) transparently move
from Guile 2.2 to 3.0, _in lockstep_.

Put differently, most of the upgrade work upon a programming language
version bump deals with conventions, and in particular package names.
Currently, `guile` corresponds to the 2.2 stable series and all the
`guile-*` packages are built against it.  In the meantime, the package
for Guile 3 is named `guile-next` and packages built against it are
called `guile3.0-*`.  Over the last few weeks we created `guile3.0-`
variants for most Guile packages, something that’s [easily achieved with
Guix](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=89a99d53f56c7c383659d821c28286b6d71e458d).

The big switch will consist in renaming all current `guile-*` packages
to `guile2.2-*` packages, for use with the legacy 2.2 series, and
renaming all the `guile3.0-*` packages to `guile-*`.  We will switch
soon, but before getting there, we’re making sure important packages are
available for 3.0.

## Guix-the-program

A more interesting part is “porting” Guix itself from Guile 2.2 to
Guile 3.  It seems that developers have become wary of 2-to-3
transitions for programming languages.  Fear not!  Switching from
Guile 2 to Guile 3 turned out to be an easy task.  In fact, very little
changed in the language itself; what did change—e.g., semantics on fine
points of the module system, support for structured exceptions—is either
optional or backwards-compatible.

As Guile 2.9 pre-releases trickled in, we started testing all the Guile
libraries Guix relies on against 2.9.  For the vast majority of them,
all we had to do was to [update their `configure.ac` to allow builds
with
3.0](https://gitlab.com/gnutls/gnutls/commit/763e31d351933222281bf9c11ff0bddb89bb701d).

Guix itself was a bit more work, mostly because it’s a rather large code
base with a picky test suite.  The bit that required most work has to do
with the introduction of [_declarative
modules_](https://www.gnu.org/software/guile/manual/html_node/Declarative-Modules.html),
an optional semantic change in modules to support more compiler
optimizations.  We had several [“white-box
tests”](https://en.wikipedia.org/wiki/White-box_testing) where tests
would happily peek at private module bindings through [the magical-evil
`@@`
operator](https://www.gnu.org/software/guile/manual/html_node/Using-Guile-Modules.html#index-_0040_0040).
Because we chose to enable declarative modules, we also had to adjust
our tests to no longer do that.  And well, that’s about it!

At that point, we were able to [create a `guile3.0-guix` package
variant](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=da7651806102d637253cb9f5677b96d6a178fc05),
primarily for testing purposes.  Soon after, we told [`guix
pull`](https://guix.gnu.org/manual/devel/en/html_node/Invoking-guix-pull.html)
to [build Guix with 3.0 instead of
2.2](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=8234fe653e61d0090138cbd4c48d877568355439).
Thus, Guix users who upgrade will transparently find themselves running
Guix on Guile 3.0.

The main benefit is improved performance.  Guile 3 is known to be [up to
32 times faster than
Guile 2.2](https://www.gnu.org/software/guile/news/gnu-guile-300-released.html)
on some micro-benchmarks.  Assessing the performance gains on a
“real-world” application like Guix is the real test.  What would be a
relevant benchmark?  At its core, Guix is essentially a compiler from
high-level descriptions of packages, operating systems, and the like, to
low-level build instructions
([_derivations_](https://guix.gnu.org/manual/devel/en/html_node/Derivations.html)).
Thus, a good benchmark is a command that exercises little more than this
compilation step: 

```
guix build libreoffice ghc-pandoc guix --dry-run --derivation
```

or:

```
guix system build config.scm --dry-run --derivation
```

On x86_64, the `guix build` command above on Guile 3 is 7% faster than
on Guile 2.2, and `guix system build`, which is more
computing-intensive, is 10% faster (heap usage is ~5% higher).  This is
lower than the skyrocketing speedups observed on some microbenchmarks,
but it’s probably no surprise: these `guix` commands are short-lived (a
couple of seconds) and they’re rather I/O- and GC-intensive—something
JIT compilation cannot help with.

On 32-bit ARM, we temporarily disabled JIT [due to a
bug](https://issues.guix.gnu.org/issue/39208); there we observe a slight
_slowdown_ compared to 2.2.  This can be explained by the fact that
[virtual machine (VM) instructions in 3.0 are lower-level than in
2.2](https://wingolog.org/archives/2018/01/17/instruction-explosion-in-guile)
and will hopefully be more than compensated for when JIT is re-enabled.

## Gluing it all together

The last part of the Guile 3 migration has to do with how Guix, and in
particular Guix System, glues things together.  As explained above, Guix
manipulates several stages of Scheme code that will run a different
points in time.

Firstly, the code that runs package builds, such as [the one that runs
`./configure && make && make
install`](https://git.savannah.gnu.org/cgit/guix.git/tree/guix/build/gnu-build-system.scm),
is Guile code.  Currently that code runs on Guile 2.2, but on the next
major rebuild-the-world upgrade, we will switch to Guile 3.

Additionally, Guix produces Scheme code consumed by [the
Shepherd](https://www.gnu.org/software/shepherd), by
[GNU mcron](https://www.gnu.org/software/mcron), and for [the graphical
installer](https://guix.gnu.org/manual/en/html_node/Guided-Graphical-Installation.html).
These will soon switch to Guile 3 as well.  This kind of change is made
easy by the fact that both the package definitions and the staged code
that depends on those packages live in the same repository.

# Long live Guile 3!

Migrating Guix to Guile 3 is a bit of work because of the many ways Guix
interacts with Guile and because of the sheer size of the code base.
For a “2-to-3” transition though, it was easy.  And fundamentally, it
remains a cheap transition compared to what it brings: better
performance and new features.  That’s another benefit of using a
general-purpose language.  

Thumbs up to everyone involved in its development, and long live
Guile 3!

#### About GNU Guix

[GNU Guix](https://guix.gnu.org) is a transactional package
manager and an advanced distribution of the GNU system that [respects
user
freedom](https://www.gnu.org/distros/free-system-distribution-guidelines.html).
Guix can be used on top of any system running the kernel Linux, or it
can be used as a standalone operating system distribution for i686,
x86_64, ARMv7, and AArch64 machines.

In addition to standard package management features, Guix supports
transactional upgrades and roll-backs, unprivileged package management,
per-user profiles, and garbage collection.  When used as a standalone
GNU/Linux distribution, Guix offers a declarative, stateless approach to
operating system configuration management.  Guix is highly customizable
and hackable through [Guile](https://www.gnu.org/software/guile)
programming interfaces and extensions to the
[Scheme](http://schemers.org) language.
