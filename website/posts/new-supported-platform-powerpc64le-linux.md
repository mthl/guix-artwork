title: New Supported Platform: powerpc64le-linux
date: 2021-04-12 00:00
author: Chris Marusich and Léo Le Bouter
tags: POWER9, Bootstrapping, Cross-compilation, Reproducible builds
---

It is a pleasure to announce that support for powerpc64le-linux
(PowerISA v.2.07 and later) has now been
[merged](https://issues.guix.gnu.org/47182) to the master branch of
GNU Guix!

This means that GNU Guix can be used immediately on this platform
[from a Git
checkout](https://guix.gnu.org/manual/en/html_node/Building-from-Git.html).
Starting with the next release (Guix v1.2.1), you will also be able to
[download a copy of Guix pre-built for
powerpc64le-linux](https://guix.gnu.org/manual/en/html_node/Binary-Installation.html#Binary-Installation).
Regardless of how you get it, you can run the new powerpc64le-linux
port of GNU Guix on top of any existing powerpc64le GNU/Linux
distribution.

This new platform is available as a "technology preview".  This means
that although it is supported,
[substitutes](https://guix.gnu.org/manual/en/html_node/Substitutes.html)
are not yet available from the build farm, and some packages may fail
to build.  Although powerpc64le-linux support is nascent, the Guix
community is actively working on improving it, and this is a great
time to [get
involved](https://guix.gnu.org/manual/en/html_node/Contributing.html)!

### Why Is This Important?

This is important because it means that GNU Guix now works on the
[Talos II, Talos II Lite, and Blackbird
mainboards](https://www.raptorcs.com/content/base/products.html) sold
by [Raptor Computing Systems](https://www.raptorcs.com/).  This
modern, performant hardware uses [IBM
POWER9](https://wiki.raptorcs.com/wiki/POWER9) processors, and it is
designed to respect your freedom.  The Talos II and Talos II Lite have
[recently received Respects Your Freedom (RYF)
certification](https://www.fsf.org/news/talos-ii-mainboard-and-talos-ii-lite-mainboard-now-fsf-certified-to-respect-your-freedom)
from the FSF, and Raptor Computing Systems is currently pursuing RYF
certification for the more affordable Blackbird, too.  All of this
hardware [can run without any non-free
code](https://wiki.raptorcs.com/wiki/Platform_Comparison), even the
bootloader and firmware.  In other words, this is a freedom-friendly
hardware platform that aligns well with GNU Guix's commitment to
software freedom.

How is this any different from existing RYF hardware, you might ask?
One reason is performance.  The existing RYF
[laptops](https://ryf.fsf.org/products?category=1&vendor=All&sort_by=created&sort_order=DESC),
[mainboards](https://ryf.fsf.org/products?category=5&vendor=All&sort_by=created&sort_order=DESC),
and
[workstations](https://ryf.fsf.org/products?category=30&vendor=All&sort_by=created&sort_order=DESC)
can only really be used with Intel Core Duo or AMD Opteron processors.
Those processors were released over 15 years ago.  Since then,
processor performance has increased drastically.  People should not
have to choose between performance and freedom, but for many years
that is exactly what we were forced to do.  However, the POWER9
machines sold by Raptor Computing Systems have changed this: the free
software community now has an RYF-certified option that [can compete
with the performance of modern Intel and AMD
systems](https://www.phoronix.com/scan.php?page=article&item=power9-threadripper-core9&num=1).

Although the performance of POWER9 processors is competitive with
modern Intel and AMD processors, the real advantage of the Talos II,
Talos II Lite, and Blackbird is that they were designed from the start
to respect your freedom.  Modern processors from [both Intel and AMD
include back
doors](https://www.fsf.org/blogs/sysadmin/the-management-engine-an-attack-on-computer-users-freedom)
over which you are given no control.  Even though the back doors can
be removed [with significant effort on older hardware in some
cases](https://www.fsf.org/news/libreboot-x200-laptop-now-fsf-certified-to-respect-your-freedom),
this is an obstacle that nobody should have to overcome just to
control their own computer.  Many of the existing RYF-certified
options (e.g., the venerable Lenovo x200) use hardware that can only
be considered RYF-certified after someone has gone through the extra
effort of removing those back doors.  No such obstacles exist when
using the Talos II, Talos II Lite, or Blackbird.  In fact, although
[Intel](https://arstechnica.com/gadgets/2020/10/in-a-first-researchers-extract-secret-key-used-to-encrypt-intel-cpu-code/)
and
[AMD](https://www.extremetech.com/computing/292722-amds-secure-processor-firmware-is-now-explorable-thanks-to-new-tool)
both go out of their way to keep you from understanding what is going
on in your own computer, Raptor Computing Systems releases [all of the
software and firmware used in their
boards](https://git.raptorcs.com/git/) as free software.  They even
include circuit diagrams when they ship you the machine!

Compared to the existing options, the Talos II, Talos II Lite, and
Blackbird are a breath of fresh air that the free software community
really deserves.  Raptor Computing Systems' commitment to software
freedom and owner control is an inspiring reminder that it **is**
possible to ship a great product while still respecting the freedom of
your customers.  And going forward, the future looks bright for the
open, royalty-free Power ISA stewarded by the OpenPOWER Foundation,
[which is now a Linux Foundation
project](https://www.linuxfoundation.org/press-release/2019/08/the-linux-foundation-announces-new-open-hardware-technologies-and-collaboration/)
(see also: [the same announcement from the OpenPOWER
Foundation](https://openpowerfoundation.org/the-next-step-in-the-openpower-foundation-journey/).

In the rest of this blog post, we will discuss the steps we took to
port Guix to powerpc64le-linux, the issues we encountered, and the
steps we can take going forward to further solidify support for this
exciting new platform.

### Bootstrapping powerpc64le-linux: A Journey

To build software, you need software.  How can one port Guix to a
platform before support for that platform exists?  This is a
[bootstrapping
problem](https://guix.gnu.org/manual/en/html_node/Bootstrapping.html).

In Guix, all software for a given platform (e.g., powerpc64le-linux)
is built starting from a small set of "bootstrap binaries".  These are
binaries of Guile, GCC, Binutils, libc, and a few other packages,
pre-built for the relevant platform.  It is intended that the
bootstrap binaries are the only pieces of software in the entire
package collection that Guix cannot build from source.  In practice,
[additional bootstrap roots are
possible](https://lists.gnu.org/archive/html/guix-devel/2015-02/msg00814.html),
but introducing them in Guix is highly discouraged, and our community
[actively](https://guix.gnu.org/en/blog/2019/guix-reduces-bootstrap-seed-by-50/)
[works](https://guix.gnu.org/en/blog/2020/guix-further-reduces-bootstrap-seed-to-25/)
to [reduce](https://guix.gnu.org/en/blog/2018/bootstrapping-rust/) our
overall bootstrap footprint.  There is one set of bootstrap binaries
for each platform that Guix supports.

This means that to port Guix to a new platform, you must first build
the bootstrap binaries for that platform.  In theory, you can do this
in many ways.  For example, you might try to manually compile them on
an existing system.  However, Guix has [package
definitions](https://git.savannah.gnu.org/cgit/guix.git/tree/gnu/packages/make-bootstrap.scm?id=5d8c2c00d60196c46a32b68c618ccbe2b3aa48f4)
that you can use to build them - using Guix, of course!

Commonly, the first step in [porting Guix to a new
platform](https://guix.gnu.org/manual/en/html_node/Porting.html) is to
use Guix to cross-compile the bootstrap binaries for that new platform
from a platform on which Guix is already supported. This can be done
by running a command like the following on a system where Guix is
already installed:

```scheme
guix build --target=powerpc64le-linux-gnu bootstrap-tarballs
```

This is the route that we took when building the powerpc64le-linux
bootstrap binaries, as described in commit
[8a1118a](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=8a1118a96c9ae128302c3d435ae77cb3dd693aea).
You might wonder why the target above is "powerpc64le-linux-gnu" even
though the new Guix platform is called "powerpc64le-linux".  This is
because "powerpc64le-linux-gnu" is a GNU
[triplet](https://wiki.osdev.org/Target_Triplet) identifying the new
platform, but "powerpc64le-linux" is the name of a "system" (i.e., a
platform) in Guix.  Guix contains code that converts between the two
as needed (see `nix-system->gnu-triplet` and `gnu-triplet->nix-system`
in
[`guix/utils.scm`](https://git.savannah.gnu.org/cgit/guix.git/tree/guix/utils.scm?id=83991a34d5c1d4985e54dd029a81412277ad062a).
When cross-compiling, you only need to specify the GNU triplet.

Note that before you can even do this, you must first update the
`glibc-dynamic-linker` and `system->linux-architecture` procedures in
Guix's code, as described in
[Porting](https://guix.gnu.org/manual/en/html_node/Porting.html).  In
addition, the versions of packages in Guix that make up the GNU
toolchain (gcc, glibc, etc.) must already support the target platform.
This pre-existing toolchain support needs to be good enough so that
Guix can (1) build, on some already-supported platform, a
cross-compilation toolchain for the target platform, (2) use, on the
already-supported platform, the cross-compilation toolchain to
cross-compile the bootstrap binaries for the target platform, and (3)
use, on the target platform, the bootstrap binaries to natively build
the rest of the Guix package collection.  The above [`guix
build`](https://guix.gnu.org/manual/en/html_node/Invoking-guix-build.html#Invoking-guix-build)
command takes care of steps (1) and (2) automatically.

Step (3) is a little more involved.  Once the bootstrap binaries for
the target platform have been built, they must be published online for
anyone to download.  After that, Guix's code must be updated so that
(a) it recognizes the "system" name (e.g., "powerpc64le-linux") that
will be used to identify the new platform and (b) it fetches the new
platform's bootstrap binaries from the right location.  After all that
is done, you just have to try building things and see what breaks.
For example, you can run `./pre-inst-env guix build hello` from your
Git checkout to try building GNU Hello.

The actual bootstrap binaries for powerpc64le-linux are stored on the
[alpha.gnu.org FTP
server](https://alpha.gnu.org/gnu/guix/bootstrap/powerpc64le-linux/20210106/).
Chris Marusich built these bootstrap binaries in an x86_64-linux Guix
System VM which was running on hardware owned by Léo Le Bouter.  Chris
then signed the binaries and provided them to Ludovic Courtès, who in
turn verified their authenticity, signed them, and [uploaded them to
alpha.gnu.org](https://debbugs.gnu.org/cgi/bugreport.cgi?bug=41669#125).
After that, we updated the code to use the newly published bootstrap
binaries in commit
[8a1118a](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=8a1118a96c9ae128302c3d435ae77cb3dd693aea).
Once all that was done, we could begin bootstrapping the rest of the
system - or trying to, at least.

There were many stumbling blocks.  For example, to resolve some test
failures, we had to update the code in Guix that enables it to make
[certain](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=b57de27d0331198c9cafb09a1cf8a5fa4f691e36)
[syscalls](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=c29bfbfc78ccd9e5c10d38faf3d269eafed12854)
from scheme.  In another example, we had to [patch GCC so that it
looks for the 64-bit libraries in
/lib](https://debbugs.gnu.org/cgi/bugreport.cgi?bug=46253), rather
than /lib64, since that is where Guix puts its 64-bit libraries by
convention.  In addition, some packages required in order to build
Guix failed to build, so we had to debug those build failures, too.

For a list of all the changes, see [the patch
series](https://debbugs.gnu.org/cgi/bugreport.cgi?bug=47182) or the
actual commits, which are:

```
$ git log --oneline --no-decorate 8a1118a96c9ae128302c3d435ae77cb3dd693aea^..65c46e79e0495fe4d32f6f2725d7233fff10fd70
65c46e79e0 gnu: sed: Make it build on SELinux-enabled kernels.
93f21e1a35 utils: Fix target-64bit? on powerpc64le-linux.
8d9aece8c4 ci: %cross-targets: Add powerpc64le-linux-gnu.
c29bfbfc78 syscalls: Fix RNDADDTOENTCNT on powerpc64le-linux.
b57de27d03 syscalls: Fix clone on powerpc64le-linux.
a16eb6c5f9 Add powerpc64le-linux as a supported Guix architecture.
b50f426803 gnu: libelf: Fix compilation for powerpc64le-linux.
1a0f4013d3 gnu: texlive-latex-base: Fix compilation on powerpc64le*.
e9938dc8f0 gnu: texlive-bin: Fix compilation on powerpc64le*.
69b3907adf gnu: guile-avahi: Fix compilation on powerpc64le-linux.
4cc2d2aa59 gnu: bdb-4.8: Fix configure on powerpc64le-linux.
be4b1cf53b gnu: binutils-final: Support more Power architectures.
060478c32c gnu: binutils-final: Provide bash for binary on powerpc-linux.
b2135b5d57 gnu: gcc-boot0: Enable 128-bit long double for POWER9.
6e98e9ca92 gnu: glibc: Fix ldd path on powerpc*.
cac88b28b8 gnu: gcc-4.7: On powerpc64le, fix /lib64 references.
fc7cf0c1ec utils: Add target-powerpc? procedure.
8a1118a96c gnu: bootstrap: Add support for powerpc64le-linux.
```

In the end, through the combined efforts of multiple people, we slowly
worked through the issues until we reached a point where we could do
all of the following things successfully:

- Build Guix manually on a [Debian GNU/Linux
  ppc64el](https://wiki.debian.org/ppc64el) machine (this is Debian's
  name for a system using the powerpc64le-linux-gnu triplet), and
  verify that its `make check` tests passed.
- Build GNU Hello using Guix and run it.
- Run [`guix
  pull`](https://guix.gnu.org/manual/en/html_node/Invoking-guix-pull.html#Invoking-guix-pull)
  to build and install the most recent version of Guix, with
  powerpc64le-linux support.
- Build a release binary tarball for powerpc64le-linux via: `make
  guix-binary.powerpc64le-linux.tar.xz`
- Use that binary to install a version of Guix that could build/run
  GNU Hello and run `guix pull` successfully.

This was an exciting moment!  But there was still more work to be
done.

Originally, we did this work on the
[wip-ppc64le](https://git.savannah.gnu.org/cgit/guix.git/log/?h=wip-ppc64le)
branch, with the intent of merging it into core-updates.  By
convention, the "core-updates" branch in Guix is [where changes are
made if they cause too many
rebuilds](https://guix.gnu.org/manual/en/html_node/Submitting-Patches.html).
Since we were updating package definitions so deep in the dependency
graph of the package collection, we assumed it wouldn't be possible to
avoid rebuilding the world.  For this reason, we had based the
wip-ppc64le branch on core-updates.

However, Efraim Flashner proved us wrong!  He created a separate
branch, wip-ppc64le-for-master, where he adjusted some of the
wip-ppc64le commits to avoid rebuilding the world on other platforms.
Thanks to his work, we were able to merge the changes directly to
master!  This meant that we would be able to include it in the next
release (Guix v.1.2.1).

In short, the initial porting work is done, and it is now possible for
anyone to easily try out Guix on this new platform.  Because `guix
pull` works, too, it is also easy to iterate on what we have and work
towards improving support for the platform.  It took a lot of
cooperation and effort to get this far, but there are multiple people
actively contributing to this port in the Guix community who want to
see it succeed.  We hope you will join us in exploring the limits of
this exciting new freedom-friendly platform!

### Other Porting Challenges

Very early in the porting process, there were some other problems that
stymied our work.

First, we actually thought we would try to port to powerpc64-linux
(big-endian).  However, this did not prove to be any easier than the
little-endian port.  In addition, other distributions (e.g.,
[Debian](https://www.debian.org/ports/) and
[Fedora](https://fedoraproject.org/wiki/Architectures)) have recently
dropped their big-endian powerpc64 ports, so the little-endian variant
is more likely to be tested and supported in the community.  For these
reasons, we decided to focus our efforts on the little-endian variant,
and so far we haven't looked back.

In both the big-endian and little-endian case, we were saddened to
discover that the bootstrap binaries are not entirely reproducible.
This fact is documented in [bug
41669](https://debbugs.gnu.org/cgi/bugreport.cgi?bug=41669), along
with our extensive investigations.

In short, if you build the bootstrap binaries on two separate machines
without using any substitutes, you will find that the derivation which
cross-compiles %gcc-static (the bootstrap GCC, version 5.5.0) produces
different output on the two systems.  However, if you build
%gcc-static twice on the same system, it builds reproducibly.  This
suggests that something in the transitive closure of inputs of
%gcc-static is perhaps contributing to its non-reproducibility.  There
is an interesting graph [toward the end of the bug
report](https://debbugs.gnu.org/cgi/bugreport.cgi?bug=41669#137),
shown below:

![Differing
Derivations](https://debbugs.gnu.org/cgi/bugreport.cgi?att=1;msg=137;filename=nonreproducible-drvs-small.png;bug=41669)

This graph shows the derivations that produce differing outputs across
two Guix System machines, when everything is built without
substitutes.  It starts from the derivation that cross-compiles
%gcc-static for powerpc64-linux-gnu (from x86_64-linux) using Guix at
commit 1ced8379c7641788fa607b19b7a66d18f045362b.  Then, it walks the
graph of derivation inputs, recording only those derivations which
produce differing output on the two different machines.  If the
non-reproducibility (across systems) of %gcc-static is caused by a
non-reproducible input, then it is probably caused by one or more of
the derivations shown in this graph.

At some point, you have to cut your losses and move on.  After months
of investigation without resolving the reproducibility issue, we
finally decided to move forward with the bootstrap binaries produced
earlier.  If necessary, we can always go back and try to fix this
issue.  However, it seemed more important to get started with the
bootstrapping work.

Anyone who is interested in solving this problem is welcome to comment
on the bug report and help us to figure out the mystery.  We are very
interested in solving it, but at the moment we are more focused on
building the rest of the Guix package collection on the
powerpc64le-linux platform using the existing bootstrap binaries.

### Next Steps

It is now possible to install Guix on a powerpc64le-linux system and
use it to build some useful software - in particular, Guix itself.  So
Guix is now "self-hosted" on this platform, which gives us a
comfortable place to begin further work.

The following tasks still need to be done.  Anyone can help, so please
get in touch if you want to contribute!

- Solve [the GCC bootstrap binary reproducibility
  issue](https://debbugs.gnu.org/cgi/bugreport.cgi?bug=41669)
  described above.
- Get [Guix
  System](https://guix.gnu.org/manual/en/html_node/System-Installation.html)
  to work on powerpc64le-linux.
- Get CI infrastructure to work
  ([Cuirass](https://guix.gnu.org/en/cuirass/) (see also: [Cuirass in
  the Guix
  manual](https://guix.gnu.org/manual/en/html_node/Continuous-Integration.html),
  [guix-build-coordinator](https://git.cbaines.net/guix/build-coordinator/)
  (see also: [Guix Build Coordinator in the Guix
  manual](https://guix.gnu.org/manual/en/html_node/Guix-Services.html),
  [substitutes](https://guix.gnu.org/manual/en/html_node/Substitutes.html),
  etc.)
- Try to build your favorite packages using Guix, [report
  problems](https://guix.gnu.org/manual/en/html_node/Tracking-Bugs-and-Patches.html),
  [try to fix
  them](https://guix.gnu.org/manual/en/html_node/Contributing.html),
  and [ask for help](https://guix.gnu.org/en/help/) if you're feeling
  stuck or not sure how to start.
- Try building rust, and if it works, judiciously re-introduce the
  librsvg dependency for powerpc64le-linux in gtk+ and gtk+-2, since
  [it is currently
  missing](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=5d2863dfe4613d5091e61800fcd5a48922c8ce4e).
- Upgrade the default GCC to 8 on core-updates, try to build guix
  (e.g., `./pre-inst-env guix build guix`), and report/fix whatever
  issues occur.  We want to upgrade GCC to 8 because, on the
  core-updates branch, glibc has been upgraded from 2.31 to 2.32.
  Unfortunately, on powerpc64le-linux, upgrading glibc from 2.31 to
  2.32 without also upgrading the default GCC (it's currently 7.5.0)
  causes a lot of problems.  Right now, we believe the best path
  forward is probably just to upgrade to GCC 8 on core-updates.
- Merge core-updates to master after that.

#### About GNU Guix

[GNU Guix](https://guix.gnu.org) is a transactional package manager and
an advanced distribution of the GNU system that [respects user
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
