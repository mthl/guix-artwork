title: Guix builds a `Hello World' VM running the Hurd
date: 2020-04-06 09:00
author: Jan Nieuwenhuizen, Ludovic Courtès
tags: GNU Hurd
---
Hello GNU World!

For all you who tried our April 1st image and ran `guix` we sure hope
you had a good laugh!

We wanted to produce that image using Guix but in the end we had to
cheat to make the release deadline.  Sorry about that.

Today we have a much more humble gift for you: On the [wip-hurd-vm
branch](http://git.savannah.gnu.org/cgit/guix.git/tree/?h=wip-hurd-vm) we
have an [initial
hurd.scm](http://git.savannah.gnu.org/cgit/guix.git/tree/gnu/system/hurd.scm?h=wip-hurd-vm)
system description that can be used to cross build a VM running the Hurd.

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
