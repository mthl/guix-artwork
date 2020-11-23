title: GNU Guix 1.2.0 released
date: 2020-11-23 15:45:00
author: Ludovic Courtès
slug: gnu-guix-1.2.0-released
tags: Releases, Security
---
![Image of a flight of the Guix.](https://guix.gnu.org/static/blog/img/flight-of-the-guix.jpg)

We are pleased to announce the release of GNU Guix version 1.2.0, right
in time to celebrate [the eighth anniversary of
Guix](https://lists.gnu.org/archive/html/gnu-system-discuss/2012-11/msg00000.html)!

The release comes with [ISO-9660 installation
images](https://guix.gnu.org/manual/en/html_node/System-Installation.html),
a [virtual machine
image](https://guix.gnu.org/manual/en/html_node/Running-Guix-in-a-VM.html),
and with tarballs to install the package manager on top of your
GNU/Linux distro, either [from
source](https://guix.gnu.org/manual/en/html_node/Requirements.html) or
[from
binaries](https://guix.gnu.org/manual/en/html_node/Binary-Installation.html).
Guix users can update by running `guix pull`.

It’s been almost 7 months since the last release, during which 200
people contributed code and packages, and a number of people contributed
to other important tasks—code review, system administration,
translation, web site updates, Outreachy mentoring, you name it!

There’s been more than 10,200 commits in that time frame and it is the
challenge of these release notes to summarize all that activity.

> Before reading any further, sit back and play [this very special
> release tune, _Ode to One Two
> Oh_](https://guix.gnu.org/audio/ode-to-one-two-oh.ogg)
> ([lyrics](https://guix.gnu.org/audio/ode-to-one-two-oh.txt)) brought
> to you by your friendly Guix team—see credits below!

### Security

A major highlight in this release is the ability to *authenticate
channels*, which probably makes Guix one of the safest ways to deliver
complete operating systems today.  This was the missing link in our
“software supply chain” and we’re glad it’s now fixed.  The end result
is that `guix pull` and related commands now cryptographically
authenticate channel code that they fetch; you cannot, for instance,
retrieve unauthorized commits to the official Guix repository.  We
[detailed the design and
implementation](https://guix.gnu.org/en/blog/2020/securing-updates/)
back in July.  The manual explains [what you need to know as a
user](https://guix.gnu.org/manual/en/html_node/Channel-Authentication.html)
and [as a channel
author](https://guix.gnu.org/manual/en/html_node/Specifying-Channel-Authorizations.html).
There’s also a new [`guix git authenticate`
command](https://guix.gnu.org/manual/en/html_node/Invoking-guix-git-authenticate.html)
to use this authentication mechanism for arbitrary Git repositories!

![Example commit graph.](https://guix.gnu.org/static/blog/img/commit-graph.svg)

Coupled to that, `guix pull` and `guix system reconfigure` now *detect
potential system downgrades or Guix downgrades* and raise an error.
This ensures you cannot be tricked into downgrading the software in your
system, which could potentially reintroduce exploitable vulnerabilities
in the software you run.

With these safeguards in place, we have added an [*unattended upgrade
service*](https://guix.gnu.org/manual/en/html_node/Unattended-Upgrades.html)
that, in a nutshell, runs `guix pull && guix system reconfigure`
periodically.  Unattended upgrades _and_ peace of mind.

Another important change from a security perspective that we’re proud of
is [*the reduction of binary seeds to
60 MiB*](https://guix.gnu.org/en/blog/2020/guix-further-reduces-bootstrap-seed-to-25/)
on x86_64 and i686, thanks to tireless work on
[GNU Mes](https://www.gnu.org/software/mes),
[Gash](https://savannah.nongnu.org/projects/gash), and related software.

On the same security theme, the build daemon and [`origin`
programming
interface](https://guix.gnu.org/manual/en/html_node/origin-Reference.html)
now accept *new cryptographic hash functions* (in particular SHA-3 and
BLAKE2s) for [“fixed-output
derivations”](https://guix.gnu.org/manual/en/html_node/Derivations.html)—so
far we were unconditionally using SHA256 hashes for source code.

### User experience

We want Guix to be accessible and useful to a broad audience and that
has again been a guiding principle for this release.  The [graphical
system
installer](https://guix.gnu.org/en/videos/system-graphical-installer/)
and the [script to install Guix on another
distro](https://guix.gnu.org/manual/en/html_node/Binary-Installation.html)
have both received bug fixes and usability improvements.  First-time
users will appreciate the fact that `guix help` now gives a clear
overview of the available commands, that `guix` commands are less
verbose by default (they no longer display a lengthy list of things that
they’ll download), and that `guix pull` displays a progress bar as it
updates its Git checkout.  `guix search`, `guix system search`, and
similar commands now invoke a pager automatically (`less` by default),
addressing an oft-reported annoyance.

*Performance improved in several places*.  Use of the new [“baseline
compiler” that landed in
Guile 3.0.4](https://wingolog.org/archives/2020/06/03/a-baseline-compiler-for-guile)
leads to reduced build times for Guix itself, which in turn means that
`guix pull` is much less resource-hungry.  Performance got better in
[several](https://issues.guix.gnu.org/44053#9)
[other](https://issues.guix.gnu.org/41702#2)
[areas](https://issues.guix.gnu.org/43340), and more work is yet to
come.

We’re giving users more flexibility on the command line, with the
addition of three [*package transformation
options*](https://guix.gnu.org/manual/en/html_node/Package-Transformation-Options.html):
`--with-debug-info` ([always debug in good
conditions](https://guix.gnu.org/manual/en/html_node/Rebuilding-Debug-Info.html)!),
`--with-c-toolchain`, and `--without-tests`.  Transformations are now
recorded in the profile and replayed upon `guix upgrade`.  Furthermore,
those options now operate on the whole dependency graph, including
“implicit” inputs, allowing for transformations not possible before,
such as:

```
guix install --with-input=python=python2 python-itsdangerous
```

Last, the new `(guix transformations)` module provides [an interface
to the transformation options available at the command
line](https://guix.gnu.org/manual/en/html_node/Defining-Package-Variants.html),
which is useful if you want to use such transformations in a manifest.

The *reference manual* has been expanded: there’s a new [“Getting
Started”
section](https://guix.gnu.org/manual/en/html_node/Getting-Started.html),
the [“Programming Interface”
section](https://guix.gnu.org/manual/en/html_node/Programming-Interface.html)
contains more info for packagers.  We added code examples in many
places; in the on-line copy of the manual, identifiers in those code
snippets are clickable, linking to the right place in the Guix or Guile
manuals.

Last but not least, *the manual is fully translated* into
[French](https://guix.gnu.org/manual/fr/html_node/),
[German](https://guix.gnu.org/manual/de/html_node/), and
[Spanish](https://guix.gnu.org/manual/es/html_node/), with partial
translations in [Russian](https://guix.gnu.org/manual/ru/html_node/) and
[Chinese](https://guix.gnu.org/manual/zh-cn/html_node/).  Guix itself is
fully translated in those three languages and partially translated in
eleven other languages.

### Packs, GNU/Hurd, disk images, services, …

But there’s more!  If you’re interested in bringing applications from
Guix to Guix-less machines, [`guix pack
-RR`](https://guix.gnu.org/manual/en/html_node/Invoking-guix-pack.html#Invoking-guix-pack)
now supports a *new ‘fakechroot’ execution engine* for relocatable
packs, and the ability to choose among different engines at run time
with the `GUIX_EXECUTION_ENGINE` variable.  The `fakechroot` engine
[improves performance compared to the `proot`
engine](https://hpc.guix.info/blog/2020/05/faster-relocatable-packs-with-fakechroot/),
for hosts that do not support unprivileged user namespaces.

Support for *whole-system cross-compilation*—as in
`guix system build --target=arm-linux-gnueabihf config.scm`—has been
improved.  That, together with a lot of porting work both for packages
and for the Guix System machinery, brings [the `hurd-vm`
service](https://guix.gnu.org/manual/en/html_node/Virtualization-Services.html#index-hurd_002dvm_002dservice_002dtype)—a
cross-compiled Guix GNU/Hurd system [running as a virtual machine under
GNU/Linux](https://guix.gnu.org/en/blog/2020/childhurds-and-substitutes/).
This in turn has let us start work on native GNU/Hurd support.

Related to this, the new `(gnu image)` module implements a flexible
interface to operating system images; from the command line, it is
accessible _via_ [`guix system disk-image
--image-type=TYPE`](https://guix.gnu.org/manual/en/html_node/Invoking-guix-system.html).
Several _image types_ are supported: compressed ISO-9660, qcow2
containing ext4 partitions, ext2 with Hurd options, and so on.  This is
currently implemented using
[`genimage`](https://github.com/pengutronix/genimage).

In addition to those already mentioned, a dozen of new system services
are available, including services for
[Ganeti](https://guix.gnu.org/en/blog/2020/running-a-ganeti-cluster-on-guix/),
[LXQt](https://guix.gnu.org/manual/en/html_node/Desktop-Services.html#index-lxqt_002ddesktop_002dservice_002dtype),
[R Shiny](https://guix.gnu.org/manual/en/html_node/Miscellaneous-Services.html#index-rshiny_002dservice_002dtype),
[Gemini](https://guix.gnu.org/manual/en/html_node/Web-Services.html#index-gmnisrv),
and [Guix Build
Coordinator](https://guix.gnu.org/manual/en/html_node/Guix-Services.html).

2,000 packages have been added, for a total of [more than 15K
packages](https://guix.gnu.org/en/packages); 3,652 were upgraded.  The
distribution comes with GNU libc 2.31, GCC 10.2, GNOME 3.34,
Xfce 4.14.2, Linux-libre 5.9.3, and LibreOffice 6.4.6.2 to name a few.
There’s also a new [build system for packages built with
Maven](https://guix.gnu.org/manual/en/html_node/Build-Systems.html#index-maven_002dbuild_002dsystem)
(bootstrapping Maven in Guix was the topic of [a talk at the Guix Days
last week](https://guix.gnu.org/en/blog/2020/online-guix-day-announce-2/)).

The [`NEWS`
file](https://git.savannah.gnu.org/cgit/guix.git/tree/NEWS?h=version-1.2.0&id=a099685659b4bfa6b3218f84953cbb7ff9e88063)
lists additional noteworthy changes and bug fixes you may be interested
in.

### Try it!

You can go ahead and [download this new
version](https://guix.gnu.org/en/download/) and [get in touch with
us](https://guix.gnu.org/en/contact/).

Speaking of which, our Debian ambassador [told
us](https://lists.gnu.org/archive/html/guix-devel/2020-11/msg00254.html)
that [you will soon be able to `apt install
guix`](https://packages.debian.org/guix) if you’re on Debian or a
derivative distro!

Enjoy!

### Credits

> Ricardo Wurmus (grand stick, synthesizer, drums, vocals, lyrics) —
> Luis Felipe (illustration) — Vagrant Cascadian (Debian packaging,
> lyrics) — Festival (back vocals)

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
