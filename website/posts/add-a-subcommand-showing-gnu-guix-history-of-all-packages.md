title: Add a subcommand showing GNU Guix history of all packages
date: 2020-12-04 12:00:00
author: Magali Lemes
tags: Outreachy
---
Hello, everyone! I'm Magali and for the next three months, I'll be an
[Outreachy](https://www.outreachy.org/) intern in the GNU Guix
community. As part of my Outreachy application process, I made my first
ever contribution to Free Software adding a package to Guix, and since
then I'm eager to begin contributing even more.

My task for this three-month period is to add a subcommand showing the
history of all packages. Although Guix makes it possible to install and
have an older version of a package, it isn't as easy to find, for
example, the commit related to these versions.

The subcommand I'll implement will be something like `guix git log `.
The idea is that, for instance, when the user invokes 
`guix git log --oneline | grep msmtp`, a list with all the commits, 
one per line, related to msmtp, will be shown.

In order to accomplish my task, I have to sharpen up my Scheme skills,
learn more about the guile-git, which is a Guile library that provides
bindings to [libgit2](https://libgit2.org/). So, to begin with, I'll
dive into the Guix code and see how commands are built.

By the end of this internship, I hope to learn much more than just
programming. I also expect to gain meaninful experience and improve my
communication skills.

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
