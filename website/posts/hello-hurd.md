title: A “Hello World” virtual machine running the Hurd
date: 2020-04-08 17:50
author: Jan Nieuwenhuizen, Ludovic Courtès
tags: GNU/Hurd
---
Hello GNU World!

There’s been a bit of speculation as to whether our [April 1st
post](https://guix.gnu.org/blog/2020/deprecating-support-for-the-linux-kernel/)
was a joke.  Part of it was a joke: we’re _not_ deprecating Linux-libre,
fear not!  But when we published it, it was already April 2nd in Eastern
parts of the world and thus, not surprisingly, the remainder of the post
was less of a joke.

# Getting to a bootable system

For all you who tried our April 1st image and ran `guix` we sure hope
you had a good laugh.  We set out to cross-build that virtual machine
(VM) image using Guix and while we made some good progress on Wednesday,
in the end we decided to cheat to make the release deadline.

What we got stuck on for a while was to get past the ext2fs
[_translator_](https://www.gnu.org/software/hurd/doc/hurd_6.html#SEC43)
(the user-land process that implements the ext2 file system) seemingly
freezing on boot, saying:

```
start ext2fs:
```

and then nothing...  Running `ext2fs` cross-built with Guix on
Debian GNU/Hurd would hang similarly.  The kernel debugger would show an
intriguing backtrace in `ext2fs` suggesting that `ext2fs` was not
handling [page fault
messages](https://www.gnu.org/software/hurd/gnumach-doc/Memory-Object-Server.html).
Long story short: we eventually realized that the server interfaces were
compiled with a 64-bit [MiG](https://www.gnu.org/software/mig) whereas
we were targeting a 32-bit platform.  From there on, we embarked on a
delightful hacking journey ensuring the Hurd boot process would
correctly run in our VM up to a proper login prompt.

Today we have a humble gift for you: On the `wip-hurd-vm` branch
(_Update: this has now been
[merged](https://git.savannah.gnu.org/cgit/guix.git/log?h=core-updates&id=5084fd38541a5fc233f3299e10a33c3a38a7173f)!_)
we have an [initial
hurd.scm](https://git.savannah.gnu.org/cgit/guix.git/tree/gnu/system/hurd.scm?h=core-updates&id=5084fd38541a5fc233f3299e10a33c3a38a7173f)
system description that can be used to cross build a VM running the
Hurd.

Running:

```
./pre-inst-env guix build -f gnu/system/hurd.scm
```

cross-compiles all the relevant packages for GNU/Hurd—specifically the
`i586-pc-gnu`
[triplet](https://www.gnu.org/savannah-checkouts/gnu/autoconf/manual/autoconf-2.69/html_node/Specifying-Target-Triplets.html)—and produces a VM image:

```
/gnu/store/yqnabv1zmlkviwzikc23w9qvfnyfwvj7-qemu-image
```

You can build it and start it from your GNU/Linux machine with this
command:

```
qemu-system-i386 -enable-kvm -m 512 -snapshot -hda \
  $(./pre-inst-env guix build -f gnu/system/hurd.scm)
```

and voilà:

![Initial Guix VM running the Hurd](https://guix.gnu.org/static/blog/img/hello-hurd.gif)

Woohoo!  (Actually we already have more stuff not shown here, such as
`guix` itself running… for a future post!  :-))

# Why bother?

Why bother with the Hurd anyway?  Isn’t it a pipe dream or “vaporware”,
depending on one’s perspective?  There’s some unquestionable truth in
that: we know that Hurd development started in the early 90’s, months
before Linux development started, and yet it still lacks so much in
terms of hardware support, even though significant progress was made in
recent years in particular with the use of [Rump
kernels](http://rumpkernel.org/).

The more we witness how new features are retrofitted in the kernel
Linux, the more we think the Hurd’s design is better suited to today’s
needs.  [Linux
namespaces](http://man7.org/linux/man-pages/man7/namespaces.7.html), the
foundation of “containers”, are such an example of an afterthought;
unprivileged user namespaces, which allow unprivileged users to benefit
from lightweight “container” virtualization, are still often disabled by
distros due to a lack of confidence.  This is in sharp contrast with the
Hurd’s inherent unrestricted support for fine-grain virtualization: a
PID namespace is just another `proc` server, and file system name space
is just another root file system server, and so on.  Container-like
lightweight virtualization is _native_ on the Hurd.

Last but not least, with an eye on the security and transparency of free
software systems, a microkernel-based systems seems to naturally lend
itself well to bootstrapping from a reduced trusted base.  This is one
of the topics [we discussed on the last Reproducible Builds
Summit](https://guix.gnu.org/blog/2019/reproducible-builds-summit-5th-edition/).

The question is not so much whether 2020 or 2021 will be the year of the
Hurd.  It’s more about the kind of systems we want to _build_.  A lot of
work remains to be done, but we think, in 2020 more than ever, that this
is a promising approach for the betterment of the security of our
systems and the freedom of users.

We also have to admit that this is an amazing system to hack on, even
more so when combined with Guix, so… happy hacking!  :-)

#### About GNU Guix

[GNU Guix](https://www.gnu.org/software/guix) is a transactional package
manager and an advanced distribution of the GNU system that [respects
user
freedom](https://www.gnu.org/distros/free-system-distribution-guidelines.html).
Guix can be used on top of any system running the Hurd or the Linux
kernel, or it can be used as a standalone operating system distribution
for i686, x86_64, ARMv7, and AArch64 machines.

In addition to standard package management features, Guix supports
transactional upgrades and roll-backs, unprivileged package management,
per-user profiles, and garbage collection.  When used as a standalone
GNU/Linux distribution, Guix offers a declarative, stateless approach to
operating system configuration management.  Guix is highly customizable
and hackable through [Guile](https://www.gnu.org/software/guile)
programming interfaces and extensions to the
[Scheme](http://schemers.org) language.

#### About the GNU Hurd

[The GNU Hurd](https://www.gnu.org/software/hurd) is the GNU project's
replacement for the Unix kernel.  It is a collection of servers that
run on the Mach microkernel to implement file systems, network
protocols, file access control, and other features that are
implemented by the Unix kernel or similar kernels (such as Linux).
[More
info](https://www.gnu.org/software/hurd/hurd/documentation.html).

The [mission of the GNU
Hurd](https://www.gnu.org/software/hurd/community/weblogs/antrik/hurd-mission-statement.html)
project is to create a general-purpose kernel suitable for the GNU
operating system, which is viable for everyday use, and gives users
and programs as much control over their computing environment as
possible.
