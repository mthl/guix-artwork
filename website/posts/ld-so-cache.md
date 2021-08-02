title: Taming the ‘stat’ storm with a loader cache
author: Ludovic Courtès
tags: Scheme API, Performance
date: 2021-08-02 15:00:00
---

It was one of these days where some of us on IRC were rehashing that old
problem—that application startup in Guix causes a
“[`stat`](https://linux.die.net/man/2/stat) storm”—and lamenting the
lack of a solution when suddenly, Ricardo
[proposes](https://logs.guix.gnu.org/guix/2020-11-24.log#183934) what,
in hindsight, looks like an obvious solution: “maybe we could use a
per-application ld cache?”.  A moment where collective thinking exceeds
the sum of our individual thoughts.  The result is one of the many
features that made it in the `core-updates` branch, slated to be merged
in the coming weeks, one that reduces application startup time.

# ELF files and their dependencies

Before going into detail, let’s look at what those “`stat` storms” look
like and where they come from.  Loading an
[ELF](https://en.wikipedia.org/wiki/Executable_and_Linkable_Format)
executable involves loading the shared libraries (the `.so` files, for
“shared objects”) it depends on, recursively.  This is the job of the
*loader* (or *dynamic linker*), `ld.so`, which is part of the GNU C
Library (glibc) package.  What shared libraries an executable like that
of Emacs depends on?  The `ldd` command answers that question:

```
$ ldd $(type -P .emacs-27.2-real)
        linux-vdso.so.1 (0x00007fff565bb000)
        libtiff.so.5 => /gnu/store/l1wwr5c34593gqxvp34qbwdkaf7xhdbd-libtiff-4.2.0/lib/libtiff.so.5 (0x00007fd5aa2b1000)
        libjpeg.so.62 => /gnu/store/5khkwz9g6vza1n4z8xlmdrwhazz7m8wp-libjpeg-turbo-2.0.5/lib/libjpeg.so.62 (0x00007fd5aa219000)
        libpng16.so.16 => /gnu/store/3x2kak8abb6z2klch72kfff2qxzv00pj-libpng-1.6.37/lib/libpng16.so.16 (0x00007fd5aa1e4000)
        libz.so.1 => /gnu/store/rykm237xkmq7rl1p0nwass01p090p88x-zlib-1.2.11/lib/libz.so.1 (0x00007fd5aa1c2000)
        libgif.so.7 => /gnu/store/bpw826hypzlnl4gr6d0v8m63dd0k8waw-giflib-5.2.1/lib/libgif.so.7 (0x00007fd5aa1b8000)
        libXpm.so.4 => /gnu/store/jgdsl6whyimkz4hxsp2vrl77338kpl0i-libxpm-3.5.13/lib/libXpm.so.4 (0x00007fd5aa1a4000)
[…]
$ ldd $(type -P .emacs-27.2-real) | wc -l
89
```

(If you’re wondering why we’re looking at `.emacs-27.2-real` rather than
`emacs-27.2`, it’s because in Guix the latter is a tiny shell wrapper
around the former.)

To load a graphical program like Emacs, the loader needs to load more
than 80 shared libraries!  Each is in its own `/gnu/store` sub-directory
in Guix, one directory per package.

But how does `ld.so` know where to find these libraries in the first
place?  In Guix, during the link phase that produces an ELF file
(executable or shared library), we tell the
[linker](https://en.wikipedia.org/wiki/Linker_%28computing%29) to
populate the `RUNPATH` entry of the ELF file with the list of
directories where its dependencies may be found.  This is done by
passing
[`-rpath`](https://sourceware.org/binutils/docs/ld/Options.html#index-_002drpath_003ddir)
options to the linker, which Guix’s [“linker
wrapper”](https://git.savannah.gnu.org/cgit/guix.git/tree/gnu/packages/ld-wrapper.in)
takes care of.  The `RUNPATH` is the *run-time library search path*:
it’s a colon-separated list of directories where `ld.so` will look for
shared libraries when it loads an ELF file.  We can look at the
`RUNPATH` of our Emacs executable like this:

```
$ objdump -x $(type -P .emacs-27.2-real) | grep RUNPATH
  RUNPATH              /gnu/store/fa6wj5bxkj5ll1d7292a70knmyl7a0cr-glibc-2.31/lib:/gnu/store/01b4w3m6mp55y531kyi1g8shh722kwqm-gcc-7.5.0-lib/lib:/gnu/store/l1wwr5c34593gqxvp34qbwdkaf7xhdbd-libtiff-4.2.0/lib:/gnu/store/5khkwz9g6vza1n4z8xlmdrwhazz7m8wp-libjpeg-turbo-2.0.5/lib:[…]
```

This `RUNPATH` has 39 entries, which roughly corresponds to the number
of direct dependencies of the executable—dependencies are listed as
`NEEDED` entries in the ELF file:

```
$ objdump -x $(type -P .emacs-27.2-real) | grep NEED | head
  NEEDED               libtiff.so.5
  NEEDED               libjpeg.so.62
  NEEDED               libpng16.so.16
  NEEDED               libz.so.1
  NEEDED               libgif.so.7
  NEEDED               libXpm.so.4
  NEEDED               libgtk-3.so.0
  NEEDED               libgdk-3.so.0
  NEEDED               libpangocairo-1.0.so.0
  NEEDED               libpango-1.0.so.0
$ objdump -x $(type -P .emacs-27.2-real) | grep NEED | wc -l
52
```

(Some of these `.so` files live in the same directory, which is why
there are more `NEEDED` entries than directories in the `RUNPATH`.)

A system such as Debian that follows the [file system hierarchy
standard](https://en.wikipedia.org/wiki/Filesystem_Hierarchy_Standard)
(FHS), where all libraries are in `/lib` or `/usr/lib`, does not have to
bother with `RUNPATH`: all `.so` files are known to be found in one of
these two “standard” locations.  Anyway, let’s get back to our initial
topic: the “`stat` storm”.

# Walking search paths

As you can guess, when we run Emacs, the loader first needs to locate
and load the 80+ shared libraries it depends on.  That’s where things
get pretty inefficient: the loader will search each `.so` file Emacs
depends on in one of the 39 directories listed in its `RUNPATH`.
Likewise, when it finally finds `libgtk-3.so`, it’ll look for its
dependencies in each of the directories in its `RUNPATH`.  We can see
that at play by tracing system calls with the
[`strace`](https://strace.io/) command:

```
$ strace -c emacs --version
GNU Emacs 27.2
Copyright (C) 2021 Free Software Foundation, Inc.
GNU Emacs comes with ABSOLUTELY NO WARRANTY.
You may redistribute copies of GNU Emacs
under the terms of the GNU General Public License.
For more information about these matters, see the file named COPYING.
% time     seconds  usecs/call     calls    errors syscall
------ ----------- ----------- --------- --------- ----------------
 55.46    0.006629           3      1851      1742 openat
 16.06    0.001919           4       422           mmap
 11.46    0.001370           2       501       477 stat
  4.79    0.000573           4       122           mprotect
  3.84    0.000459           4       111           read
  2.45    0.000293           2       109           fstat
  2.34    0.000280           2       111           close
[…]
------ ----------- ----------- --------- --------- ----------------
100.00    0.011952           3      3325      2227 total
```

For this simple `emacs --version` command, the loader and `emacs` probed
for more than 2,200 files, with the
[`openat`](https://linux.die.net/man/2/openat) and
[`stat`](https://linux.die.net/man/2/stat) system calls, and most of
these probes were unsuccessful (counted as “errors” here, meaning that
the call returned an error).  The fraction of “erroneous” system calls
is no less than 67% (2,227 over 3,325).  We can see the desperate search
of `.so` files by looking at individual calls:

```
$ strace -e openat,stat emacs --version
[…]
openat(AT_FDCWD, "/gnu/store/fa6wj5bxkj5ll1d7292a70knmyl7a0cr-glibc-2.31/lib/libpng16.so.16", O_RDONLY|O_CLOEXEC) = -1 ENOENT (No such file or directory)
openat(AT_FDCWD, "/gnu/store/01b4w3m6mp55y531kyi1g8shh722kwqm-gcc-7.5.0-lib/lib/libpng16.so.16", O_RDONLY|O_CLOEXEC) = -1 ENOENT (No such file or directory)
openat(AT_FDCWD, "/gnu/store/l1wwr5c34593gqxvp34qbwdkaf7xhdbd-libtiff-4.2.0/lib/libpng16.so.16", O_RDONLY|O_CLOEXEC) = -1 ENOENT (No such file or directory)
openat(AT_FDCWD, "/gnu/store/5khkwz9g6vza1n4z8xlmdrwhazz7m8wp-libjpeg-turbo-2.0.5/lib/libpng16.so.16", O_RDONLY|O_CLOEXEC) = -1 ENOENT (No such file or directory)
openat(AT_FDCWD, "/gnu/store/3x2kak8abb6z2klch72kfff2qxzv00pj-libpng-1.6.37/lib/tls/haswell/x86_64/libpng16.so.16", O_RDONLY|O_CLOEXEC) = -1 ENOENT (No such file or directory)
stat("/gnu/store/3x2kak8abb6z2klch72kfff2qxzv00pj-libpng-1.6.37/lib/tls/haswell/x86_64", 0x7ffe428a1c70) = -1 ENOENT (No such file or directory)
openat(AT_FDCWD, "/gnu/store/3x2kak8abb6z2klch72kfff2qxzv00pj-libpng-1.6.37/lib/tls/haswell/libpng16.so.16", O_RDONLY|O_CLOEXEC) = -1 ENOENT (No such file or directory)
stat("/gnu/store/3x2kak8abb6z2klch72kfff2qxzv00pj-libpng-1.6.37/lib/tls/haswell", 0x7ffe428a1c70) = -1 ENOENT (No such file or directory)
openat(AT_FDCWD, "/gnu/store/3x2kak8abb6z2klch72kfff2qxzv00pj-libpng-1.6.37/lib/tls/x86_64/libpng16.so.16", O_RDONLY|O_CLOEXEC) = -1 ENOENT (No such file or directory)
stat("/gnu/store/3x2kak8abb6z2klch72kfff2qxzv00pj-libpng-1.6.37/lib/tls/x86_64", 0x7ffe428a1c70) = -1 ENOENT (No such file or directory)
openat(AT_FDCWD, "/gnu/store/3x2kak8abb6z2klch72kfff2qxzv00pj-libpng-1.6.37/lib/tls/libpng16.so.16", O_RDONLY|O_CLOEXEC) = -1 ENOENT (No such file or directory)
stat("/gnu/store/3x2kak8abb6z2klch72kfff2qxzv00pj-libpng-1.6.37/lib/tls", 0x7ffe428a1c70) = -1 ENOENT (No such file or directory)
openat(AT_FDCWD, "/gnu/store/3x2kak8abb6z2klch72kfff2qxzv00pj-libpng-1.6.37/lib/haswell/x86_64/libpng16.so.16", O_RDONLY|O_CLOEXEC) = -1 ENOENT (No such file or directory)
stat("/gnu/store/3x2kak8abb6z2klch72kfff2qxzv00pj-libpng-1.6.37/lib/haswell/x86_64", 0x7ffe428a1c70) = -1 ENOENT (No such file or directory)
openat(AT_FDCWD, "/gnu/store/3x2kak8abb6z2klch72kfff2qxzv00pj-libpng-1.6.37/lib/haswell/libpng16.so.16", O_RDONLY|O_CLOEXEC) = -1 ENOENT (No such file or directory)
stat("/gnu/store/3x2kak8abb6z2klch72kfff2qxzv00pj-libpng-1.6.37/lib/haswell", 0x7ffe428a1c70) = -1 ENOENT (No such file or directory)
openat(AT_FDCWD, "/gnu/store/3x2kak8abb6z2klch72kfff2qxzv00pj-libpng-1.6.37/lib/x86_64/libpng16.so.16", O_RDONLY|O_CLOEXEC) = -1 ENOENT (No such file or directory)
stat("/gnu/store/3x2kak8abb6z2klch72kfff2qxzv00pj-libpng-1.6.37/lib/x86_64", 0x7ffe428a1c70) = -1 ENOENT (No such file or directory)
openat(AT_FDCWD, "/gnu/store/3x2kak8abb6z2klch72kfff2qxzv00pj-libpng-1.6.37/lib/libpng16.so.16", O_RDONLY|O_CLOEXEC) = 3
[…]
```

Above is the sequence where we see `ld.so` look for `libpng16.so.16`,
searching in locations where we *know* it’s not going to find it.  A bit
ridiculous.  How does this affect performance?  The impact is small in
the most favorable case—on a hot cache, with fast solid state device
(SSD) storage.  But it likely has a visible effect in other cases—on a
cold cache, with a slower spinning hard disk drive (HDD), on a network
file system (NFS).

# Enter the per-package loader cache

The idea that Ricardo submitted, using a loader cache, makes a lot of
sense: we know from the start that `libpng.so` may only be found in
`/gnu/store/…-libpng-1.6.37`, no need to look elsewhere.  In fact, it’s
not new: glibc has had such a cache “forever”; it’s the
`/etc/ld.so.cache` file you can see on FHS distros and which is
typically created by running
[`ldconfig`](https://linux.die.net/man/8/ldconfig) when a package has
been installed.  Roughly, the cache maps library `SONAME`s, such as
`libpng16.so.16`, to their file name on disk, say
`/usr/lib/libpng16.so.16`.

The problem is that this cache is inherently system-wide: it assumes
that there is only *one* `libpng16.so` on the system; any binary that
depends on `libpng16.so` will load it from its one and only location.
This models perfectly matches the FHS, but it’s at odds with the
flexibility offered by Guix, where several variants or versions of the
library can coexist on the system, used by different applications.
That’s the reason why Guix and other non-FHS distros such as NixOS or
GoboLinux typically [turn
off](https://git.savannah.gnu.org/cgit/guix.git/tree/gnu/packages/base.scm?id=a92dfbce30777de6ca05031e275410cf9f56c84c#n716)
that feature altogether… and pay the cost of those `stat` storms.

The insight we gained on that Tuesday evening IRC conversation is that
we could *adapt* glibc’s loader cache to our setting: instead of a
system-wide cache, we’d have a *per-application loader cache*.  As one
of the last package [build
phases](https://guix.gnu.org/manual/en/html_node/Build-Phases.html),
we’d run `ldconfig` to create `etc/ld.so.cache` within that package’s
`/gnu/store` sub-directory.  We then need to modify the loader so it
would look for `${ORIGIN}/../etc/ld.so.cache` instead of
`/etc/ld.so.cache`, where `${ORIGIN}` is the location of the ELF file
being loaded.  A discussion of these changes is [in the issue
tracker](https://issues.guix.gnu.org/44899); you can see [the glibc
patch](https://git.savannah.gnu.org/cgit/guix.git/tree/gnu/packages/patches/glibc-dl-cache.patch?h=core-updates&id=0236013cd0fc86ff4a042885c735e3f36a7f5c25)
and the new [`make-dynamic-linker-cache` build
phase](https://git.savannah.gnu.org/cgit/guix.git/tree/guix/build/gnu-build-system.scm?h=core-updates&id=0236013cd0fc86ff4a042885c735e3f36a7f5c25#n735).
In short, the `make-dynamic-linker-cache` phase computes the set of
direct and indirect dependencies of an ELF file using the
[`file-needed/recursive`](https://git.savannah.gnu.org/cgit/guix.git/tree/guix/build/gremlin.scm?id=0236013cd0fc86ff4a042885c735e3f36a7f5c25#n265)
procedure and derives from that the library search path, creates a
temporary `ld.so.conf` file containing this search path for use by
`ldconfig`, and finally runs `ldconfig` to actually build the cache.

How does this play out in practice?  Let’s try an `emacs` build that
uses this new loader cache:

```
$ strace -c /gnu/store/ijgcbf790z4x2mkjx2ha893hhmqrj29j-emacs-27.2/bin/emacs --version
GNU Emacs 27.2
Copyright (C) 2021 Free Software Foundation, Inc.
GNU Emacs comes with ABSOLUTELY NO WARRANTY.
You may redistribute copies of GNU Emacs
under the terms of the GNU General Public License.
For more information about these matters, see the file named COPYING.
% time     seconds  usecs/call     calls    errors syscall
------ ----------- ----------- --------- --------- ----------------
 28.68    0.002909          26       110        13 openat
 25.13    0.002549          26        96           read
 20.41    0.002070           4       418           mmap
  9.34    0.000947          10        90           pread64
  6.60    0.000669           5       123           mprotect
  4.12    0.000418           3       107         1 newfstatat
  2.19    0.000222           2        99           close
[…]
------ ----------- ----------- --------- --------- ----------------
100.00    0.010144           8      1128        24 total
```

Compared to what we have above, the total number of system calls has
been divided by 3, and the fraction of erroneous system calls goes from
67% to 0.2%.  Quite a difference!  We count on you, dear users, to [let
us know](https://guix.gnu.org/en/contact) how this impacts load time for
you.

# Flexibility without `stat` storms

With [GNU Stow](https://www.gnu.org/software/stow) in the 1990s, and
then Nix, Guix, and other distros, the benefits of flexible file layouts
rather than the rigid Unix-inherited FHS have been demonstrated—nowadays
I see it as an antidote to opaque and bloated application bundles à la
Docker.  Luckily, few of our system tools have FHS assumptions baked in,
probably in large part thanks to GNU’s insistence on a [rigorous
installation directory
categorization](https://www.gnu.org/prep/standards/html_node/Directory-Variables.html)
in the early days rather than hard-coded directory names.  The loader
cache is one of the few exceptions.  Adapting it to a non-FHS context is
fruitful for Guix and for the other distros and packaging tools in a
similar situation; perhaps it could become an option in glibc proper?

This is not the end of `stat` storms, though.  Interpreters and language
run-time systems rely on search paths—`GUILE_LOAD_PATH` for Guile,
`PYTHONPATH` for Python, `OCAMLPATH` for OCaml, etc.—and are equally
prone to stormy application startups.  Unlike ELF, they do not have a
mechanism akin to `RUNPATH`, let alone a run-time search path cache.  We
have yet to find ways to address these.


#### About GNU Guix

[GNU Guix](https://guix.gnu.org) is a transactional package manager and
an advanced distribution of the GNU system that [respects user
freedom](https://www.gnu.org/distros/free-system-distribution-guidelines.html).
Guix can be used on top of any system running the Hurd or the Linux
kernel, or it can be used as a standalone operating system distribution
for i686, x86_64, ARMv7, AArch64 and POWER9 machines.

In addition to standard package management features, Guix supports
transactional upgrades and roll-backs, unprivileged package management,
per-user profiles, and garbage collection.  When used as a standalone
GNU/Linux distribution, Guix offers a declarative, stateless approach to
operating system configuration management.  Guix is highly customizable
and hackable through [Guile](https://www.gnu.org/software/guile)
programming interfaces and extensions to the
[Scheme](http://schemers.org) language.
