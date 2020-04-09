title: Deprecating support for the Linux kernel
date: 2020-04-01 23:00
author: Jan (janneke) Nieuwenhuizen, Ludovic (civodul) Courtès, Marius (mbakke) Bakke, Ricardo (rekado) Wurmus
tags: GNU/Hurd, Linux
---

> _Hey, this post was published on April 1st, so take it with a grain of
> salt!  [Read the
> followup](https://guix.gnu.org/blog/2020/a-hello-world-virtual-machine-running-the-hurd/)
> for some clarifications._

After [years in the
making](https://lists.gnu.org/archive/html/guix-devel/2015-08/msg00379.html),
Guix [recently gained support](https://lists.gnu.org/archive/html/guix-devel/2020-03/msg00081.html)
for running natively on the [GNU/Hurd operating system](https://www.gnu.org/software/hurd/).
That means you will soon be able to replace...

```
(kernel linux-libre)
```

with

```
(kernel hurd)
(initial-herd hurd)
```

...in your [operating-system
declaration](https://guix.gnu.org/manual/en/guix.html#operating_002dsystem-Reference)
and reboot into the future!

Running on the Hurd was always a goal for Guix, and supporting multiple
kernels is a huge maintenance burden.  As such it is expected that the
upcoming Guix 1.1 release will be the last version featuring the
Linux-Libre kernel.  Future versions of Guix System will run
exclusively on the Hurd, and we expect to remove Linux-Libre
entirely by Guix 2.0.

The Linux kernel will still be supported when using Guix on "foreign"
distributions, but it will be on a best-effort basis.  We hope that
other distributions will follow suit and adopt the Hurd in order to
increase security and freedom for their users.

We provide a [pre-built virtual machine image with the Hurd for
download](https://guix.gnu.org/guix-hurd-20200401.img.tar.xz) with
SHA256
`056e69ae4b5fe7a062b954a5be333332152caa150359c20253ef77152334c662`.

Here is how to get started:

```
wget https://guix.gnu.org/guix-hurd-20200401.img.tar.xz
tar xf guix-hurd-20200401.img.tar.xz
guix environment --ad-hoc qemu -- \
    qemu-system-i386 -enable-kvm -drive file=guix-hurd-20200401.img,cache=writeback -m 1G
```

Log in as `root` without password.  Then try `hello`, `guix describe`,
or `guix install linux-libre` to run Linux in userspace...  We are
looking forward to your feedback!


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
