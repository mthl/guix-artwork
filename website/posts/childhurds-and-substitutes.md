title: Childhurds and GNU/Hurd substitutes
date: 2020-10-08 14:15
author: Jan (janneke) Nieuwenhuizen, Ludovic Courtès, Mathieu Othacehe
slug: childhurds-and-substitutes
tags: GNU/Hurd, Reproducible builds
---

A lot has happened since our [Hello Hurd
post](https://guix.gnu.org/en/blog/2020/a-hello-world-virtual-machine-running-the-hurd/)
beginning of April.
No, not nearly as much as [we joked on April 1st
](https://guix.gnu.org/blog/2020/deprecating-support-for-the-linux-kernel/),
but more than enough to share and be proud of.

# Building a Hurd virtual machine

As some of you noticed, the previous hacks to build a Hurd virtual
machine (VM) were
removed and no longer work; using Guix you can now build a GNU/Hurd VM
just like you would build a GNU/Linux VM:

```
guix system disk-image -t hurd-raw bare-hurd.tmpl
```

This cross-compiles all the relevant packages for GNU/Hurd—specifically the
`i586-pc-gnu`
[triplet](https://www.gnu.org/savannah-checkouts/gnu/autoconf/manual/autoconf-2.69/html_node/Specifying-Target-Triplets.html)—and produces a VM image:

```
/gnu/store/n7jkfajw0fzp975hv0b9v18r9bbr961q-disk-image
```

You can build it and start it from your GNU/Linux machine with this
command:

```
qemu-system-i386 -enable-kvm -m 512 -snapshot -hda \
  $(guix system disk-image -t hurd-raw bare-hurd.tmpl)
```

We are using this ready-made, minimal GNU/Hurd operating system
description
[gnu/system/examples/bare-hurd.tmpl](https://git.savannah.gnu.org/cgit/guix.git/tree/gnu/system/examples/bare-hurd.tmpl)
that looks suprisingly familiar:

```scheme
(use-modules (gnu) (gnu system hurd) (guix utils))
(use-service-modules ssh)
(use-package-modules ssh)

(define %hurd-os
  (operating-system
    (inherit %hurd-default-operating-system)
    (bootloader (bootloader-configuration
                 (bootloader grub-minimal-bootloader)
                 (target "/dev/sdX")))
    (file-systems (cons (file-system
                          (device (file-system-label "my-root"))
                          (mount-point "/")
                          (type "ext2"))
                        %base-file-systems))
    (host-name "guixygnu")
    (timezone "Europe/Amsterdam")
    (packages (cons openssh-sans-x %base-packages/hurd))
    (services (cons (service openssh-service-type
                             (openssh-configuration
                              (openssh openssh-sans-x)
                              (use-pam? #f)
                              (port-number 2222)
                              (permit-root-login #t)
                              (allow-empty-passwords? #t)
                              (password-authentication? #t)))
               %base-services/hurd))))

%hurd-os
```

and it can be customized just like a GNU/Linux operating system
description.  The end result is a full-blown Guix System with [the
Shepherd](https://gnu.org/software/shepherd) managing system services
and all that—finally we can run
[`herd`](https://www.gnu.org/software/shepherd/manual/html_node/Invoking-herd.html)
on the Hurd.

A lot of things had to be in place to support this, we worked on

 * Adding [multiboot support](https://en.wikipedia.org/wiki/Multi-booting)
   to [`grub-minimal-bootloader`](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=1244491a0d5334e1589159a2ff67bbc967b9648b); a nice standard that Linux sadly does
   not support,
 * Support for the Hurd and cross-building of the core packages listed in
   [`%base-packages/hurd`](https://git.savannah.gnu.org/cgit/guix.git/tree/gnu/system/hurd.scm#n62),
 * Support for the Hurd and cross-building of the system services in
   [`%base-services/hurd`](https://git.savannah.gnu.org/cgit/guix.git/tree/gnu/system/hurd.scm#n67),
   - Add [`hurd-console-service`](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=f9c04580bf5462bb088f47ad8fc6c3136649cbd6),
   - Add [`hurd-getty-service`](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=7ccd471c71d650055e99cd02381bc8dcd86d5313),
   - Add [`hurd-default-essential-services`](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=45b2cb439deaa2f438aed3893ee8fc80445d5563),
   - Add Hurd support to [`%boot-service`](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=b37c544196898cc3dfa3da07ed344fbe11abc120)
   - Add a [`hurd-startup-service`](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=68d8c094659565fe19abc1c433a17337ce5cacb7),
   - Add Hurd support to [`activation-service`](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=c3fd2df705695a0dc9f393545606360be1ea6104),
   - Add Hurd support for “syscalls”:
   [`define-c-struct`](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=86f5decd2066889bf2e60df388d6c812aede0917),
   [`ioctl`](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=598be42dfa3aa1f6a92b5562397742b3fa96a3e0),
   [`readdir*`](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=82d8959e5d137b2061a68878d78a8f74a238ac44),
   [`sockaddr-in`](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=0d371c633f7308cfde2432d6119d386a5c63198c),
   [`getxattr`](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=df05842332be80ed7f53022402b95cf711163b41),
   [`setxattr`](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=d155c9d93496ae620829fbc33b5694e74cda9683),

counting some ~200 patches by ten people over six months; including
generic cross-compilation fixes and support, and Hurd fixes and
support.

Also, we finished the passive translator settings over extended
attributes (_xattrs_) [for the
Hurd](https://git.savannah.gnu.org/cgit/hurd/hurd.git/commit/?id=a04c7bf83172faa7cb080fbe3b6c04a8415ca645)
and [for
Linux](https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=88ee9d571b6d8ed345f877e05f685814412e359b).

You may notice that we are using the new `disk-image` command rather
than the old `vm-image`.  One of the big hurdles in producing a VM
image was [the way Guix produces VM
images](https://issues.guix.gnu.org/41350): it would run a target
QEMU, e.g. `qemu-arm`.  That does not work for the Hurd, as there is
no `qemu-hurd`.  Without going into the hairy details, when Ludo and
Janneke were—three patch sets, 50 messages and 13 days later—almost
ready to give up, Mathieu came to the rescue with his [brand-new
implementation](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=f19cf27c2b9ff92e2c0fd931ef7fde39c376adaa)
of the `disk-image` command.  At the time, Hurd work was done on the
`wip-hurd` branch and the disk-image work on `wip-disk-image`.  Soon
after, Mathieu proposed an [_explosive
mix_](https://lists.gnu.org/archive/html/bug-guix/2020-04/msg00610.html)
of the two branches; we managed to create the first Hurd system that
really felt like Guix System.

The new implementation of the `disk-image` command was followed by the
introduction of an `--image-type` or `-t` option.  This option allows to
produce disk images targeting different supports. The `hurd-raw` and
`hurd-qcow2` image types, producing respectively a raw Hurd disk-image and a
Hurd QCOW2 disk-image were introduced. They can be used this way:

```
guix system disk-image -t hurd-raw bare-hurd.tmpl
guix system disk-image -t hurd-qcow2 bare-hurd.tmpl
```

This mechanism providing much more flexibility in the Guix System image
generation will be described in a future blog post.

We also offer downloads of continuously built (actually cross-built)
[Guix System on
GNU/Hurd](https://ci.guix.gnu.org/search/latest/image?query=spec:guix-master+status:success+system:x86_64-linux+hurd-barebones.qcow2)
(~350 MiB).

# Substitutes

While amazing to be able to just run the Hurd in a VM, development without
[substitutes](https://guix.gnu.org/manual/en/html_node/Substitutes.html)
is a real pain; a Guix system needs substitutes.  How to
go about that?  We have a build farm but currently Hurd only runs on
ancient hardware: not an option.  We need a machine running Guix/Hurd
to
[offload](https://guix.gnu.org/manual/devel/en/html_node/Daemon-Offload-Setup.html)
Hurd build jobs to.

The proposed solution was to automate the building and running of a
Guix VM into a new Guix service: the so-called `hurd-vm` or `childhurd`
service.  We would add something like:

```scheme
(service hurd-vm-service-type)
```

to a couple of our build nodes to add a hird of virtual Hurd machines
to our build farm.

# The Childhurd service

The Hurd—being based on a microkernel—has some beautiful built-in
"virtualization" possibilites that have their own naming:
[neighborhurd](https://www.gnu.org/software/hurd/hurd/neighborhurd.html)s,
[subhurd](https://www.gnu.org/software/hurd/hurd/subhurd.html)s.
Similarly, we are adding
[_childhurds_](https://guix.gnu.org/manual/devel/en/html_node/Virtualization-Services.html#Childhurd)
to this mix: a childhurd is a GNU/Hurd VM running on GNU/Linux and
managed by Guix.

When you are running Guix System, building a Hurd VM manually is no
longer necessary.  Just add [the `hurd-vm`
service](https://guix.gnu.org/manual/devel/en/html_node/Transparent-Emulation-with-QEMU.html#The-Hurd-in-a-Virtual-Machine)
to your operating
system description:

```scheme
(service hurd-vm-service-type
         (hurd-vm-configuration
          (disk-size (* 12 (expt 2 30))) ;12GiB
          (memory-size 1024)))           ; 1GiB
```

and this will build a childhurd for you when you reconfigure your
system.  This childhurd can be stopped and started just like any other
service:

```bash
herd stop hurd-vm
herd start childhurd
```
(`childhurd` is an alias for `hurd-vm`).

*WARNING*

![This Hurd is fully operational](/static/blog/img/hurd-substitutes.gif)

It is highly addictive.

Having shaved this yak, let’s not lose sight that our initial goal was to
offload builds to those GNU/Hurd VMs.  The childhurd service does all
the heavy lifting.  To offload from GNU/Linux to my childhurd, I added
this to my `/etc/guix/machines.scm`:

```scheme
 (build-machine
  (name "localhost")
  (systems (list "i586-gnu"))
  (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHZsrZ63zs+AhWbVJgYq6j1h2rgQGrWKCokpR2/Q/Jzy root@guixygnu")
  (port 10022)  ;the Hurd VM has SSH listening on that port
  (user "root")
  (private-key "/home/janneke/.ssh/id_rsa_childhurd"))
```

That can be used to transparently offload builds from GNU/Linux to the
childhurd:

```bash
$ uname -o
GNU/Linux
$ guix build hello -s i586-gnu
The following derivation will be built:
   /gnu/store/jqdvjhcxxnbq370y8i2c973c9zfiqrgl-hello-2.10.drv
[…]
guix offload: sending 1 store item (12 MiB) to 'localhost'...
offloading '/gnu/store/jqdvjhcxxnbq370y8i2c973c9zfiqrgl-hello-2.10.drv' to 'localhost'...
[…]
/gnu/store/803q5wapfnmr91ag8d9dzwabkbdxz3ay-hello-2.10
$ file -L $(guix build hello -s i586-gnu)/bin/hello
/gnu/store/803q5wapfnmr91ag8d9dzwabkbdxz3ay-hello-2.10/bin/hello: ELF 32-bit LSB executable, Intel 80386, version 1 (SYSV), dynamically linked, interpreter /gnu/store/9vs3gkp6svam82zw7vjlml7iiarcs11c-glibc-2.31/lib/ld.so.1, for GNU/Hurd 0.0.0, not stripped
```

Hurrah!

# Hurd substitutes

Last Friday we produced the very first [GNU Hello
substitute](https://ci.guix.gnu.org/803q5wapfnmr91ag8d9dzwabkbdxz3ay.narinfo)
for the Hurd:

```
StorePath: /gnu/store/803q5wapfnmr91ag8d9dzwabkbdxz3ay-hello-2.10
URL: nar/gzip/803q5wapfnmr91ag8d9dzwabkbdxz3ay-hello-2.10
Compression: gzip
FileSize: 61822
URL: nar/lzip/803q5wapfnmr91ag8d9dzwabkbdxz3ay-hello-2.10
Compression: lzip
FileSize: 52887
NarHash: sha256:0g4k1kppjs5148ynm4zw4x1kpaby67npc3ws6s7y7hf0il1cgryk
NarSize: 204328
References: 803q5wapfnmr91ag8d9dzwabkbdxz3ay-hello-2.10 9vs3gkp6svam82zw7vjlml7iiarcs11c-glibc-2.31 bwvd5338kfm0vsc4i9xvh48vdxr5ywrz-gcc-7.5.0-lib
System: i586-gnu
Deriver: jqdvjhcxxnbq370y8i2c973c9zfiqrgl-hello-2.10.drv
Signature: 1;berlin.guix.gnu.org;KHNpZ25hdHVyZSAKIChkYXRhIAogIChmbGFncyByZmM2OTc5KQogIChoYXNoIHNoYTI1NiAjMDVGOEY5NjMxRUU5QzcxM0REQUNBRTYwNUNCNjJBNzlDNUY4NEVFQTIwMjc5OERBNTQ3NURCOUU2Q0FBRDMwMSMpCiAgKQogKHNpZy12YWwgCiAgKGVjZHNhIAogICAociAjMDVBMzkzMTgwOUY1RkQyMTdGMDM4MUVDMTJEODYyNzIyOEYyNjJGRDA4MTcxQjREMzZBNEM0RjBBNjZEQkY4NSMpCiAgIChzICMwQTc2RjZGNENCOTMzQTczNzA4QkNGMzRGREExMzkyOTRGQTQxREQzQTUwQkEwOUE0ODRCQUQyOTA4MjQ5ODIxIykKICAgKQogICkKIChwdWJsaWMta2V5IAogIChlY2MgCiAgIChjdXJ2ZSBFZDI1NTE5KQogICAocSAjOEQxNTZGMjk1RDI0QjBEOUE4NkZBNTc0MUE4NDBGRjJEMjRGNjBGN0I2QzQxMzQ4MTRBRDU1NjI1OTcxQjM5NCMpCiAgICkKICApCiApCg==
```

For development, porting and fixing of packages, you can use a
Childhurd configuration like this:

```scheme
(use-modules (srfi srfi-1) (guix packages) (guix records))
(use-package-modules base compression file gawk gdb hurd less m4 package-management)

(define guix-packages
  (filter-map input->package
              (fold alist-delete (package-direct-inputs guix)
                    '("glibc-utf8-locales" "graphviz" "po4a"))))

(operating-system
  (inherit %hurd-vm-operating-system)
  (users ...)
  (packages
    (cons* diffutils file findutils gawk gdb-minimal git-minimal
           gnu-make grep gzip less m4 openssh-sans-x tar xz
           (append
            guix-packages
            (delete guile-3.0 %base-packages/hurd)))))
```

… which allows working from a Git clone:

```bash
15:51:05 janneke@dundal:~/src/guix/master [env]
$ ssh -A janneke@childhurd
Last login: Sat Oct  3 15:31:55 2020 from 10.0.2.2


  This is the GNU Hurd.  Welcome.

janneke@childhurd ~$ git config --global url."git+ssh://git.sv.gnu.org/srv/git/".insteadOf gnu:
janneke@childhurd ~$ git clone gnu:guix
Cloning into 'guix'...
remote: Counting objects: 394436, done.
remote: Compressing objects: 100% (85728/85728), done.
remote: Total 394436 (delta 309572), reused 392294 (delta 307893)
Receiving objects: 100% (394436/394436), 137.05 MiB | 1.18 MiB/s, done.
Resolving deltas: 100% (309572/309572), done.
Updating files: 100% (2199/2199), done.
```

but before we continue let's first check the
[weather](https://guix.gnu.org/development/manual/en/html_node/Invoking-guix-weather.html#Invoking-guix-weather):

```bash
janneke@childhurd ~$ guix weather
computing 11079 package derivations for i586-gnu...
looking for 11521 store items on https://ci.guix.gnu.org...
updating substitutes from 'https://ci.guix.gnu.org'... 100.0%
https://ci.guix.gnu.org
  1.5% substitutes available (169 out of 11521)
  at least 443.3 MiB of nars (compressed)
  966.7 MiB on disk (uncompressed)
  0.012 seconds per request (142.2 seconds in total)
  81.0 requests per second
[..]
```

this gives an idea of how young this project is.  Any day now, more
[Hurd substitutes](https://ci.guix.gnu.org/jobset/hurd-master) will
follow.  Now let's configure and build Guix:

```bash
janneke@childhurd ~$ cd guix
janneke@childhurd ~/guix$ guix environment --bootstrap \
  --ad-hoc gcc-toolchain@7 libgcrypt zlib
substitute: updating substitutes from 'https://ci.guix.gnu.org'... 100.0%
The following derivations will be built:
   /gnu/store/gxq5flc8kwpn999dw9xxvldy9xfd0q2x-profile.drv
   /gnu/store/vfkjnwgl4ckyklrl2z4q8x2vnlrwwyfr-gcc-toolchain-7.5.0.drv
   /gnu/store/zh6snj49ayrpw24jn7whpzygj1fpy9cm-module-import-compiled.drv

66.3 MB will be downloaded
[..]
building profile with 3 packages...
janneke@childhurd ~/guix [env]$ ./bootstrap
[..]
janneke@childhurd ~/guix [env]$ ./configure --with-courage\
  --localstatedir=/var --sysconfdir=/etc
[..]
janneke@childhurd ~/guix [env]$ make
[..]
janneke@childhurd ~/guix [env]$ ./pre-inst-env guix build hello
substitute: updating substitutes from 'https://ci.guix.gnu.org'... 100.0%
0.1 MB will be downloaded:
   /gnu/store/803q5wapfnmr91ag8d9dzwabkbdxz3ay-hello-2.10
substituting /gnu/store/803q5wapfnmr91ag8d9dzwabkbdxz3ay-hello-2.10...
downloading from https://ci.guix.gnu.org/nar/lzip/803q5wapfnmr91ag8d9dzwabkbdxz3ay-hello-2.10 ...
 hello-2.10  52KiB                    258KiB/s 00:00 [##################] 100.0%

/gnu/store/803q5wapfnmr91ag8d9dzwabkbdxz3ay-hello-2.10
```

just like we are used to do…almost.  We are using `--bootstrap` and a
targeted `--ad-hoc` to avoid dependencies like `libx11`,
`python-minimal`, and other packages that do not build yet.

# Isolated build environments

To help achieve [reproducible
builds](https://reproducible-builds.org/docs/definition/), Guix builds
packages in _isolated build environments_: build environments contain
nothing but the inputs explicitly declared in the package
definition—[not even
`/bin/sh`](https://lists.gnu.org/archive/html/bug-guix/2013-01/msg00041.html).
Build environments also lack network access.  On GNU/Linux this is
achieved by running builds in separate _namespaces_.  Besides, the
environment contains device nodes and “special” file systems that are
usually expected to be available: `/dev/null`, `/dev/pts`, `/dev/shm`,
`/proc`, the loopback networking device, and so on.  (The exact contents
[are
documented](https://guix.gnu.org/manual/en/html_node/Build-Environment-Setup.html).)

On GNU/Linux, these special files and file systems are implemented by
the kernel.  Guix only cares about user-land software, meaning that
these devices “leak” from the host kernel instead of being an explicit
“input” of
[derivations](https://guix.gnu.org/manual/en/html_node/Derivations.html),
but that’s OK, that’s the deal: the kernel and hardware are considered
outside of Guix’s control.

What about GNU/Hurd, though?  In GNU/Hurd, `/dev/null`, a
Linux-compatible `/proc`, the TCP/IP stack necessary to implement the
loopback device, and even support for pipes are all implemented in
user-land: writing to `/dev/null` amounts to talking to the `/hurd/null`
service (or _translator_), operations on `AF_INET` sockets translate to
remote procedure calls (RPCs) to `/servers/socket/2`, which the
`/hurd/pfinet` program listens to, and so on.

That raises an interesting question: what should the build environment
contain on GNU/Hurd?  So far our GNU/Hurd builds were made in
non-isolated environments; we have just [started
implementing](https://issues.guix.gnu.org/43857) support for isolated
builds but we’ll have to answer that question first.  If we stick to our
approach—every piece of user-land software must be an explicit input of
the build process—then code that implements TCP/IP, `/dev/null`, or even
`pipe` should be an explicit input of any build process that needs those
facilities.

This principled approach can push the notion of controlled, reproducible
build environments to a whole new level.  For example, we’ve
[had](https://issues.guix.gnu.org/21280#7)
[cases](https://issues.guix.gnu.org/20877#5) where the choice of the
root file system—e.g., ext4 vs. Btrfs—has an observable effect on
software behavior, leading to concrete issues such as test failures in
one case and not in the other.  On GNU/Hurd, build processes could run
their own root file system, doing away with this kind of discrepancy.

On the other hand, there are practical issues that cannot be ignored:
virtually all build processes need these facilities so they’ll need to
be set up one way or another.  Also, one could argue that things like
`/dev/null` have a well-defined interface that’s set in stone and that,
consequently, how they’re implemented does not matter at all.  Can we
say the same of the TCP/IP stack though?  Maybe not.  A line needs to be
drawn somewhere.

We have yet to decide where to draw the line and to precisely define
what the build environment contains on GNU/Hurd.  These questions are
closely related to bootstrapping issues we notably [discussed at the
2019 Reproducible Builds
Summit](https://guix.gnu.org/blog/2019/reproducible-builds-summit-5th-edition/).
Tricky, but exciting.

# What's next?

In [an earlier
post](https://guix.gnu.org/en/blog/2020/a-hello-world-virtual-machine-running-the-hurd/)
we tried to answer the question “Why bother with the Hurd anyway?” An
obvious question because it is [all too easy to get
discouraged](https://xkcd.com/1508), to downplay or underestimate the
potential social impact of GNU and the Hurd.

We tried to make Hurd development as easy and as pleasant as we could.
As you have seen, things start to work pretty nicely and there is
still plenty of work to do in Guix.  But in a way this is “merely
packaging” the amazing work of others.  Some of the real work that
needs to be done and which is being discussed and is in progress
right now includes:

   * [user-space driver/modern hardware
support](https://lists.gnu.org/archive/html/bug-hurd/2020-07/msg00042.html)
   * [audio support](https://nlnet.nl/project/Hurd-Audio/) (this is
sponsored by [NLnet](https://nlnet.nl), thanks!),
   * [SMP
support](https://lists.gnu.org/archive/html/bug-hurd/2020-07/msg00048.html),
   * [64-bit](https://github.com/etienne02/gnumach/tree/master/x86_64)/
[x86_64](http://richtlijn.be/~larstiq/hurd/hurd-2020-03-28) support
(!), as well as porting to non-Intel CPUs.

All these tasks look daunting, and indeed that’s a lot of work ahead.
But the development environment is certainly an advantage.  Take an
example: surely anyone who’s hacked on device drivers or file systems
before would have loved to be able to GDB into the code, restart it, add
breakpoints and so on—that’s exactly the experience that the Hurd
offers.  As for Guix, it will make it easy to test changes to the
micro-kernel and to the Hurd servers, and that too has the potential to
speed up development and make it a very nice experience.

Join `#guix` and `#hurd` on irc.freenode.net or the [mailing
lists](https://guix.gnu.org/en/contact) and get involved!

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
