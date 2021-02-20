title: Guix Further Reduces Bootstrap Seed to 25%
date: 2020-06-15 12:00
author: Jan (janneke) Nieuwenhuizen
tags: Bootstrapping, Reproducible builds, Security
---
We are delighted to announce that the second reduction by 50% of the
Guix _bootstrap binaries_ has now been officially released!

The initial set of binaries from which packages are built now weighs
in at approximately 60~MiB, a quarter of what it used to be.

In [a previous blog
post](https://guix.gnu.org/blog/2019/guix-reduces-bootstrap-seed-by-50/)
we elaborate on why this reduction and bootstrappability in general is
so important.  One reason is to eliminate---or greatly reduce the
attack surface of---a [“trusting
trust”](https://www.archive.ece.cmu.edu/~ganger/712.fall02/papers/p761-thompson.pdf)
attack.  Last summer at the [Breaking Bitcoin
conference](https://breaking-bitcoin.com), Carl Dong gave a [fun and
remarkably gentle
introduction](http://diyhpl.us/wiki/transcripts/breaking-bitcoin/2019/bitcoin-build-system)
and at [FOSDEM2020](https://fosdem.org/2020) I also gave [a short talk
](https://fosdem.org/2020/schedule/event/gnumes/) about this.  If you
choose to believe that building from source is the proper way to do
computing, then it follows that the “trusting trust” attack is only a
symptom of an incomplete or missing bootstrap story.

#### Further Reduced Binary Seed bootstrap

Last year, the first reduction removed the GCC, glibc and Binutils
binary seeds.  The new _Further Reduced Binary Seed_ bootstrap, merged
in Guix `master` last month, removes the “static-binaries tarball”
containing GNU Awk, Bash, Bzip2, the GNU Core Utilities, Grep, Gzip,
GNU Make, Patch, sed, Tar, and Xz.  It replaces them by [Gash and Gash
Core Utils](https://savannah.nongnu.org/projects/gash).  Gash is a
minimalist POSIX shell written in Guile Scheme, while Gash Core Utils
is a Scheme implementation for most of the tools found in
GNU Coreutils, as well as the most essential bits of Awk, grep and
sed.

After three new [GNU Mes](https://gnu.org/s/mes) releases with
numerous Mes C Library updates and fixes, a major update of Gash and
the first official Gash Utils release, and the [delicate balancing of
17 new bootstrap source packages and
versions](https://bugs.gnu.org/38390), the bottom of the package graph
now looks like this (woohoo!):

```
                              gcc-mesboot (4.9.4)
                                      ^
                                      |
                                    (...)
                                      ^
                                      |
               binutils-mesboot (2.14), glibc-mesboot (2.2.5),
                          gcc-core-mesboot (2.95.3)
                                      ^
                                      |
            bash-mesboot (2.05), bzip2-mesboot, gawk-mesboot (3.0.0)
       diffutils-mesboot (2.7), patch-mesboot (2.5.9), sed-mesboot (1.18)
                                      ^
                                      |
                             gnu-make-mesboot (3.82)
                                      ^
                                      |
                                gzip-mesboot (1.2.4)
                                      ^
                                      |
                                  tcc-boot
                                      ^
                                      |
                                  mes-boot
                                      ^
                                      |
                          gash-boot, gash-utils-boot
                                      ^
                                      |
                                      *
                 bootstrap-mescc-tools, bootstrap-mes (~12 MiB)
                            bootstrap-guile (~48 MiB)
```
[full graph](/static/blog/img/gcc-core-mesboot0-graph.svg)

We are excited that the [Nlnet Foundation](https://nlnet.nl) has
[sponsored this work](https://nlnet.nl/project/GNUMes)!

However, we aren't done yet; far from it.

### Lost Paths

The idea of reproducible builds and bootstrappable software [is not
very
new](https://lists.reproducible-builds.org/pipermail/rb-general/2017-January/000309.html).
Much of that was implemented for the GNU tools in the early 1990s.
Working to recreate it in present time shows us much of that practice
was forgotten.

Readers who are familiar with the GNU toolchain may have
noticed the version numbers of the ``*-mesboot`` source packages in
this great new bootstrap: They are ancient!  That's a problem.

Typically, newer versions of the tool chain fix all kinds of bugs,
make the software easier to build and add support for new CPU
architectures, which is great.  However---more often than not---
simultaneously new features are introduced or dependencies are added
that are not necessary for bootstrapping and may increase the
bootstrap hurdle.  Sometimes, newer tools are more strict or old
configure scripts do not recognise newer tool versions.

A trivial example is GNU sed.  In the current bootstrap we are using
version 1.18, which was released in 1993.  Until recently the latest
version of sed we could hope to bootstrap was sed-4.2.2 (2012).  Newer
releases ship as `xz`-compressed tarballs only, and `xz` is
notoriously difficult to bootstrap (it needs a fairly recent GCC and
try building that without sed).

Luckily, the sed maintainers (Jim Meyering) [were happy to
correct](https://lists.gnu.org/archive/html/sed-devel/2020-01/msg00016.html)
this mistake and starting from release
[sed-4.8](http://ftp.gnu.org/pub/gnu/sed/sed-4.8.tar.gz) (2020) also
`gzip`-compressed tarballs will be shipped.  Similar for the GNU Core
Utils: Releases made between 2011 and 2019 will probably be useless
for bootstrapping.  Confronted with this information, also the
coreutils maintainers (Pádraig Brady) [were happy
to](https://lists.gnu.org/archive/html/coreutils/2020-02/msg00043.html)
release
[coreutils-8.32](http://ftp.gnu.org/pub/gnu/coreutils/coreutils-8.32.tar.gz)
also in `gzip` compression from now on.

Even these simple cases show that solving bootstrap problems can only
be done together: For GNU it really is a project-wide responsibility
that needs to be addressed.

Most bootstrap problems or loops are not so easy to solve and
sometimes there are no obvious answers, for example:

* In 2013, the year that [Reproducible
  Builds](https://reproducible-builds.org) started to gain some
  traction, the GNU Compiler Collection [released
  gcc-4.8.0](http://gcc.gnu.org/gcc-4.8/changes.html),
  making C++ a build requirement, and

* Even more recently (2018), the GNU C Library [glibc-2.28 adds Python
  as a build
  requirement](https://sourceware.org/git/?p=glibc.git;a=commit;h=c6982f7efc1c70fe2d6160a87ee44d871ac85ab0),

and while these examples make for a delightful puzzle from a
bootstrappability perspective, we would love to see the maintainers of
GNU softwares to consider bootstrappability and start taking more
responsibility for the bootstrap story of their packages.

#### Towards a Universal, Full Source Bootstrap

Our next target will be a third reduction by ~50%; the Full-Source
bootstrap will replace the MesCC-Tools and GNU Mes binaries by
[Stage0](https://savannah.nongnu.org/projects/stage0) and
[M2-Planet](https://github.com/oriansj/m2-planet).

The Stage0 project by Jeremiah Orians starts everything from ~512
bytes; virtually nothing.  Have a look at this incredible project if
you haven’t already done so.

We are most grateful and excited that the [Nlnet
Foundation](https://nlnet.nl) has [again decided to sponsor this
work](https://nlnet.nl/project/GNUMes-fullsource/)!

While the reduced bootstrap currently only applies to the i686-linux
and x86_64-linux architectures, we are thrilled that ARM will be
joining soon.  The [Trusted ARM bootstrapping
work](https://nlnet.nl/project/GNUMes-arm/) is progressing nicely, and
GNU Mes is now passing its entire mescc test suite on native ARMv7,
and passing nigh its entire gcc test suite on native ARMv7.  Work is
underway to compile tcc using that GNU Mes.  Adding this second
architecture is a very important one towards the creation of a
universal bootstrap!

Upcoming releases of Gash and Gash-Utils will allow us to clean up the
bottom of the package graph and remove many of the “vintage” packages.
In particular, the next version of Gash-Utils will be sophisticated
enough to build everything up to `gcc-mesboot` using only old versions
of GNU Make and Gzip.  This is largely thanks to improvements to the
implementation of Awk, which now includes nearly all of the standard
features.

Looking even further into the future, we will likely have to remove
the “vintage” GCC-2.95.3 that was such a helpful stepping stone and
reach straight for GCC-4.6.4.  Interesting times ahead!

#### About Bootstrappable Builds and GNU Mes

Software is bootstrappable when it does not depend on a binary seed
that cannot be built from source.  Software that is not
bootstrappable---even if it is free software---is a serious security
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
