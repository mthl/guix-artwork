title: Reproducible Builds Summit, 5th edition
date: 2019-11-12 14:00
author: Ludovic Courtès, YOUR NAME HERE!
tags: Reproducible builds
---

[For](https://guix.gnu.org/blog/2018/reproducible-builds-summit-4th-edition/)
[several](https://www.gnu.org/software/guix/blog/2015/reproducible-builds-a-means-to-an-end/)
[years](https://guix.gnu.org/blog/2016/reproducible-build-summit-2nd-edition/),
the Reproducible Builds Summit has become this pleasant and fruitful
retreat where we Guix hackers like to go and share, brainstorm, and hack
with people from free software projects and companies who share this
interest in [reproducible builds](https://reproducible-builds.org/) and
related issues.  This year, several of us had the chance to be in
Marrakesh for [the fifth Reproducible Builds
Summit](https://reproducible-builds.org/events/Marrakesh2019/), which
was attended by about thirty people.

This blog post summarizes the (subjective!) takeaways from the different
sessions we attended, and introduces some of the cool hacks that came to
life on the roof top of the lovely riad that was home to the summit.

# Java

  - F-Droid
  - Maven
  - ?

# Verifying and sharing build results

# Bootstrapping

# _Extreme_ bootstrapping!

# More cool hacks

  - Ten Years reproducibility challenge

During the summit, support for [_system provenance tracking_ in `guix
system`](https://issues.guix.gnu.org/issue/38441) landed in Guix.  This
allows a deployed system to embed the information needed to rebuild it:
its
[channels](https://guix.gnu.org/manual/devel/en/html_node/Channels.html)
and its [configuration
file](https://guix.gnu.org/manual/devel/en/html_node/Using-the-Configuration-System.html).
In other words, the result is what we could call a _source-carrying
system_, which could also be thought of as [a sort of
Quine](https://en.wikipedia.org/wiki/Quine_(computing)).  For users it’s
a convenient way to map a running system or virtual machine image back
to its source, or to verify that its binaries are genuine by rebuiling
it.

The [`guix
challenge`](https://guix.gnu.org/manual/devel/en/html_node/Invoking-guix-challenge.html)
command started its life [shortly before first
summit](https://guix.gnu.org/blog/2015/reproducible-builds-a-means-to-an-end/).
During this year’s hacking sessions, it [gained a `--diff`
option](https://issues.guix.gnu.org/issue/38518) that automates the
steps of downloading, decompressing, and diffing non-reproducible
archives, possibly with [Diffoscope](https://diffoscope.org/).  The idea
[came up some time ago](https://issues.guix.gnu.org/issue/35621), and
it’s good that we can cross that line from our to-do list.

# Thanks!

We are grateful to everyone who made this summit possible: Gunner and
Evelyn of Aspiration, Hannes, Holger, Lamby, Mattia, and Vagrant, as well as our
kind hosts at Priscilla.  And of course, thanks to all fellow
participants whose openmindedness and focus made this both a productive
and a pleasant experience!

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
