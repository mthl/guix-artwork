title: Guix Reduces Bootstrap Seed by 50%
date: 2019-10-08 00:00
author: Jan Nieuwenhuizen
tags: Bootstrapping, Reproducible builds, Security
---
We are delighted to announce that the first reduction by 50% of the
Guix _bootstrap binaries_ has now been officially released!

This is a very important step because the ~250MB _seed_ of binary code
was practically non-auditable, which makes it hard to establish what
source code produced them.

Every unauditable binary also leaves us vulnerable to compiler
backdoors as described by Ken Thompson in the 1984 paper [Reflections
on Trusting
Trust](https://www.archive.ece.cmu.edu/~ganger/712.fall02/papers/p761-thompson.pdf)
and beautifully explained by Carl Dong in his [Bitcoin Build System
Security](https://www.youtube.com/watch?v=I2iShmUTEl8) talk.

It is therefore equally important that we continue towards our final
goal: A _Full Source_ bootstrap; removing all unauditable binary
seeds.

#### Guix’ Rigorous Regular Bootstrap

GNU Guix takes a rigorous approach to bootstrapping.  Bootstrapping in
this context refers to how the distribution gets built _from nothing_.

The GNU system is primarily made of C code, with glibc at its core.
The GNU build system itself assumes the availability of a Bourne shell
and command-line tools provided by the Core GNU Utilities and Awk,
Grep, Make, Sed, and some others.

The build environment of a package is a container that contains
nothing but the package’s declared inputs.  To be able to build
anything at all in this container, Guix needs [pre-built
binaries](https://guix.gnu.org/manual/en/html_node/Bootstrapping.html)
of Guile, GCC, Binutils, glibc, and the other tools mentioned above.

So there is an obvious chicken-and-egg problem: How does the first
package get built?  How does the first compiler get compiled?

```
                            gcc-final
                                ^
                                |
                          cross-gcc-boot
                                ^
                                |
                        gcc-boot0 (5.5.0)
                                ^
                                |
                 binutils-boot0, libstdc++-boot0
                                ^
                                |
           diffutils-boot0, findutils-boot0, file-boot0
                                ^
                                |
                           make-boot0
                                ^
                                |
                                *
        bootstrap-binutils, bootstrap-gcc, bootstrap-glibc (~130MB)
      bootstrap-bash, bootstrap-coreutils&co, bootstrap-guile (~120MB)
```
[full graph](/static/blog/img/gcc-final-bag.svg)

The answer to this starts with _bootstrap binaries_.  The first
package that gets built with these bootstrap binaries is `make`, next
are `diffutils`, `findutils`, and `file`.  Eventually a `gcc-final` is
built: the compiler used to build regular packages.

#### i686/x86_64 Reduced Binary Seed bootstrap

The Guix development branch we just merged introduces a _reduced
binary seed_ bootstrap for x86_64 and i686, where the bottom of the
dependency graph looks like this:

```
                         gcc-mesboot (4.9.4)
                                ^
                                |
                        glibc-mesboot (2.16.0)
                                ^
                                |
                         gcc-mesboot1 (4.7.4)
                                ^
                                |
                        binutils-mesboot (2.20.1a)
                                ^
                                |
                         gcc-mesboot0 (2.95.3)
                                ^
                                |
                         glibc-mesboot0 (2.2.5)
                                ^
                                |
                        gcc-core-mesboot (2.95.3)
                                ^
                                |
    make-mesboot0, diffutils-mesboot, binutils-mesboot0 (2.20.1a)
                                ^
                                |
                            tcc-boot
                                ^
                                |
                            tcc-boot0
                                ^
                                |
                            mes-boot
                                ^
                                |
                                *
              bootstrap-mescc-tools, bootstrap-mes (~10MB)
      bootstrap-bash, bootstrap-coreutils&co, bootstrap-guile (~120MB)
```
[full graph](/static/blog/img/gcc-final-bag-rbsb.svg)

The new _Reduced Binary Seed_ bootstrap removes Binutils, GCC, and
glibc and replaces them by GNU Mes and MesCC Tools.  This reduces
the trusted binary seed by ~120MB - half of it!

As a user, it means your package manager has a formal description of
how to build all your applications, in a reproducible way, starting
from nothing but this ~120MB seed.  It means you can rebuild any of
those software artifacts locally [without trusting a single binary
provider](https://guix.gnu.org/blog/2015/reproducible-builds-a-means-to-an-end/).

For comparison, _traditional_ distros often have an informally
specified bootstrap story, usually relying on much bigger binary
seeds.  We estimate those seeds to weigh in at ~550MB (the size of
`debootstrap --arch=i386
--include=build-essential,dpkg-dev,debhelper,gcc,libc6-dev,make,texinfo
bullseye ./bullseye-chroot http://deb.debian.org/debian`, with
`bullseye-chroot/var/cache/apt/archives` removed) in the case of
Debian—ignoring [cycles that show up higher in the
graph](https://bootstrap.debian.net/botch-native/amd64/stats.html).

These bootstrap binaries can now be re-created by doing

```
    guix build bootstrap-binaries
```

Work started [three years
ago](https://lists.gnu.org/archive/html/guile-user/2016-06/msg00061.html)
with a simple LISP-1.5 interpreter.

A year later, [Mes
0.5](https://lists.gnu.org/archive/html/guile-user/2017-04/msg00056.html)
had become a tiny Scheme interpreter written in simple subset of C
that came with a simple C compiler in Scheme.  And yes, these were
mutual self-hosting.

The next step was to find a path towards compiling Guix’s default GCC
(5.5.0).  Sadly, bootstrapping GCC compilers has been becoming
increasingly difficult over the years.  We looked at GCC 1.42: not
easy to bootstrap (100,000 LOC) and it depends on Bison.  Reluctantly,
we started looking for non-GNU alternatives
[8cc](https://github.com/rui314/8cc), [pcc](http://pcc.ludd.ltu.se/),
[cc500](https://web.archive.org/web/20160402225843/http://homepage.ntlworld.com/edmund.grimley-evans/cc500/)
but finally settled on [TinyCC](https://bellard.org/tcc/).  TinyCC
(TCC) can compile GCC (4.7.4) which is currently the most recent
release of GCC that can be built without a C++ compiler.

Another year later, [Mes
0.13](https://lists.gnu.org/archive/html/guile-user/2018-04/msg00033.html)
has grown its own tiny C library and compiles a heavily patched and
simplified TCC.  This looked very promising and [we
suggested](https://lists.nongnu.org/archive/html/tinycc-devel/2017-09/msg00019.html)
for TinyCC to help our bootstrapping effort by moving towards a
simplified C subset.
[Instead](https://lists.nongnu.org/archive/html/tinycc-devel/2017-09/msg00020.html)
[we](https://lists.nongnu.org/archive/html/tinycc-devel/2017-09/msg00024.html)
[were](https://lists.nongnu.org/archive/html/tinycc-devel/2017-09/msg00037.html)
[encouraged](https://lists.nongnu.org/archive/html/tinycc-devel/2017-09/msg00040.html)
to make MesCC a full blown C99 compliant compiler.  That felt as a
setback but it gave us the perspective of removing TCC from the
bootstrap later on.  Using
[Nyacc](https://savannah.nongnu.org/projects/nyacc), the amazing
parser framework with C99 parser by Matt Wette, has even made that a
feasible perspective.

It took only half a year to mature into [Mes
0.19](https://lists.gnu.org/archive/html/guile-user/2018-12/msg00081.html)
so that building TinyCC (25,000 LOC) now only takes ~8min instead of
the initial 5h.

With a bootstrapped TCC we tried building some versions of GCC (1.4,
2.6.3, 2.95.3, 3.0, 3.1, 3.2.3, 3.4.0, 3.4.6, 4.1.1, 4.1.2) to try to
build some versions of glibc (1.06.4, 1.09.1, 2.0.1. 2.1.3, 2.3,
2.3.6, 2.2.5, 2.12.2, 2.15, 2.16.0, 2.28) using slightly less versions
of Binutils (1.9, 2.5.1, 2.5.2, 2.6, 2.7, 2.10.1, 2.14, 2.14a, 2.15a,
2.17a, 2.18a, 2.20.1a, 2.23.2, 2.25, 2.28).  There were many
interesting dependencies, tradeoffs, patching of generated Autotools
outputs, especially if you are using your own tiny C library and
headers.

Typically, newer versions of the tool chain fix all kinds of bugs in
the build system and C code compliance, which is great.  However,
simultaneously new features are introduced or dependencies are added
that are not necessary for bootstrapping, increasing the bootstrap
hurdle.  Sometimes, newer tools are more strict or old configure
scripts do not recognise newer tool versions.

Also, can you spot the triplets of tool versions that combine into
integral versions of the tool chain? ;-)


#### Scheme-only Bootstrap

Our next target will be another reduction by ~50%; the Scheme-only
bootstrap will replace the Bash, Coreutils, etc. binaries by
[Gash](https://savannah.nongnu.org/projects/gash) and Gash Core Utils.

Gash is a work-in-progress implementation of a POSIX shell in Scheme,
that is already capable-enough to interpret Autoconf-generated
`configure` scripts.  It can run on Guile but it is designed to also
run on Mes, meaning that we can use it early on during bootstrap.

We are excited that the [Nlnet Foundation](https://nlnet.nl) is now
[sponsoring this work](https://nlnet.nl/project/GNUMes)!

#### Creating a GNU Tool Chain Bootstrap Story

The Reduced Binary Seed bootstrap starts by building ancient GNU
software, notably GCC (2.95.3), glibc (2.2.5).

This amazing achievement is mirrored only by its terrible clumsiness.
Is this really how we want to secure the bootstrap of our GNU system?

```
                         gcc-mesboot (4.6.4)
                                ^
                                |
                            tcc-boot
                                ^
                                |
                            mes-boot
                                ^
                                |
                                *
```

Maybe if we could go straight from TinyCC to GCC (4.6.4) we need no
longer depend on an ancient GNU tool chain and have a somewhat more
modern and more maintainable bootstrap path.

Now that we have shown it can be done, we think it is time for GNU
tool chain developers to step in and help create a better version of
our tool chain bootstrap story.

#### Towards a Full Source Bootstrap

We expect many interesting challenges before we approach this lofty
target.

The [stage0](https://savannah.nongnu.org/projects/stage0) project by
Jeremiah Orians starts everything from ~512 bytes; virtually nothing.
Have a look at this incredible project if you haven’t already done so.

Jeremiah is also leading the
[Mes-M2](https://github.com/oriansj/mes-m2.git) effort that is about
bootstrapping Mes from stage0.  The Mes Scheme interpreter is being
rewritten in an even more simple subset of C, without preprocessor
macros even.  That C-like language is called M2-Planet, after its
transpiler.

#### About Bootstrappable Builds and GNU Mes

Software is bootstrappable when it does not depend on a binary seed
that cannot be built from source.  Software that is not
bootstrappable - even if it is free software - is a serious security
risk
[for](https://www.ece.cmu.edu/~ganger/712.fall02/papers/p761-thompson.pdf)
[a](https://manishearth.github.io/blog/2016/12/02/reflections-on-rusting-trust/)
[variety](https://www.quora.com/What-is-a-coders-worst-nightmare/answer/Mick-Stute)
[of](http://blog.regehr.org/archives/1241)
[reasons](https://www.alchemistowl.org/pocorgtfo/pocorgtfo08.pdf).
The [Bootstrappable Builds](https://bootstrappable.org/) project aims
to reduce the number and size of binary seeds to a bare minimum.

[GNU Mes](https://www.gnu.org/software/mes/) is closely related to the
Bootstrappable Builds project.  Mes aims to create an entirely
source-based bootstrapping path for the Guix System and other
interested GNU/Linux distributions.  The goal is to start from a
minimal, easily inspectable binary (which should be readable as
source) and bootstrap into something close to R6RS Scheme.

Currently, Mes consists of a mutual self-hosting scheme interpreter
and C compiler.  It also implements a C library.  Mes, the scheme
interpreter, is written in about 5,000 lines of code of simple C.
MesCC, the C compiler, is written in scheme.  Together, Mes and MesCC
can compile [a lightly patched
TinyCC](http://gitlab.com/janneke/tinycc) that is self-hosting.  Using
this TinyCC and the Mes C library, it is possible to bootstrap the
entire Guix System for i686-linux and x86_64-linux.

#### About GNU Guix

[GNU Guix](https://www.gnu.org/software/guix) is a transactional package
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
