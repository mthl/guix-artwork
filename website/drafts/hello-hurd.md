title: A “Hello World” virtual machine running the Hurd
date: 2020-04-06 09:00
author: Jan Nieuwenhuizen, Ludovic Courtès
tags: GNU Hurd
---
Hello GNU World!

There’s been a bit of speculation as to whether our [April 1st
post](https://guix.gnu.org/blog/2020/deprecating-support-for-the-linux-kernel/)
was a joke.  Part of it was a joke: we’re _not_ deprecating Linux-libre,
fear not!  But when we published it, it was already April 2nd in Eastern
part of the world and thus, not surprisingly, the remainder of the post
was less of a joke.

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

Today we have a humble gift for you: On the [wip-hurd-vm
branch](http://git.savannah.gnu.org/cgit/guix.git/log/?h=wip-hurd-vm)
we have an [initial
hurd.scm](http://git.savannah.gnu.org/cgit/guix.git/tree/gnu/system/hurd.scm?h=wip-hurd-vm)
system description that can be used to cross build a VM running the
Hurd.

Running

```
./pre-inst-env guix build -f gnu/system/hurd.scm
```

produces a VM image


```
/gnu/store/yqnabv1zmlkviwzikc23w9qvfnyfwvj7-qemu-image
```

that you can start like so

```
cp /gnu/store/yqnabv1zmlkviwzikc23w9qvfnyfwvj7-qemu-image hello-hurd.img
chmod +w hello.img
guix environment --ad-hoc qemu -- \
    qemu-system-i386 -drive file=hello-hurd.img,cache=writeback -m 1G
```

and voilà:

![Initial Guix VM running the Hurd](../../../static/blog/img/guix-hello-hurd.png)

And that's about it right now: No `guix` executable, `herd`, or
`ssh-daemon` yet.  Also, the current bootstrap goes like this

```
/hurd/startup -> /libexec/runsystem -> /hurd/init ->
    /libexec/runsystem.hurd ->
        /libexec/rc
        /libexec/runttys -> /libexec/getty
```

where `runsystem` and `runsystem.hurd` are upstream bash scripts and
only `rc` is written in Guile right now.

Happy hacking!

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
