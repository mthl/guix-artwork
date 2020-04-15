title: GNU Guix 1.1.0 released
date: 2020-04-15 15:00:00
author: Ludovic Courtès, Marius Bakke
slug: gnu-guix-1.1.0-released
tags: Releases, System tests
---
We are pleased to announce the release of GNU Guix version 1.1.0!

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

If you wonder what installing Guix System is like, this video gives an
overview of the guided installation process:

![Video of the system installation process.](https://guix.gnu.org/guix-videos/guix-system-install-1.1.0.webm)

There are more [“getting started” videos](https://guix.gnu.org/videos/).

It’s been 11 months since the previous release, during which 201 people
contributed code and packages.  This is a long time for a release, which
is in part due to the fact that bug fixes and new features are
continuously delivered to our users _via_ `guix pull`.  However, a
number of improvements, in particular in the installer, will greatly
improve the experience of first-time users.

It’s hard to summarize more than 14,000 commits!  Here are some
highlights as far as tooling is concerned:

  - The new [`guix
    deploy`](https://guix.gnu.org/manual/devel/en/html_node/Invoking-guix-deploy.html)
    tool allows you to deploy several machines at once, [be it remote
    machines over
    SSH](https://guix.gnu.org/blog/2019/towards-guix-for-devops/) or
    [machines at a virtual private server
    (VPS)](https://guix.gnu.org/blog/2019/managing-servers-with-gnu-guix-a-tutorial/).
  - Channel authors can now write news entries for their users, [which
    are readily readable using `guix pull
    --news`](https://guix.gnu.org/blog/2019/spreading-the-news/).  As a
    result, if you were already using Guix, you’ve probably already read
    these news!
  - The new [`guix system
    describe`](https://guix.gnu.org/manual/en/html_node/Invoking-guix-system.html)
    command tells you which commits of which channels were used to
    deploy your system, and also contains a link to your operating
    system configuration file.  Precise provenance tracking that gives
    users and admins the ability to know _exactly_ what changed between
    two different system instances!  This feature builds upon the new
    [`provenance`](https://guix.gnu.org/manual/en/html_node/Service-Reference.html#index-provenance_002dservice_002dtype) service.
  - [`guix
    pack`](https://guix.gnu.org/manual/en/html_node/Invoking-guix-pack.html)
    has improved support for generating Singularity and Docker images,
    notably with the `--entry-point` option.
  - There’s a new [`guix time-machine`
    command](https://guix.gnu.org/manual/en/html_node/Invoking-guix-time_002dmachine.html)
    that does what you would expect :-), and it nicely benefits from
    improved Software Heritage integration through [`guix
    lint`](https://guix.gnu.org/manual/en/html_node/Invoking-guix-lint.html).
  - [`guix
    challenge`](https://guix.gnu.org/manual/en/html_node/Invoking-guix-challenge.html)
    can now show diffs on the fly—one of the outcomes of the recent
    [Reproducible Build
    Summit](https://guix.gnu.org/blog/2019/reproducible-builds-summit-5th-edition/).
  - Guix can now publish and download lzip-compressed substitutes,
    [which significantly reduces bandwidth
    requirement](https://guix.gnu.org/blog/2019/substitutes-are-now-available-as-lzip/).
  - [`guix
    system`](https://guix.gnu.org/manual/en/html_node/Invoking-guix-system.html)
    supports a `--target` option providing some support for the
    cross-compilation of complete systems.  More on that in [this FOSDEM
    talk](https://fosdem.org/2020/schedule/event/ggaaattyp/).
  - Guix now [runs on
    Guile 3](https://guix.gnu.org/blog/2020/guile-3-and-guix/), which
    improves performance.
  - The manual now includes a
    "[cookbook](https://guix.gnu.org/cookbook/en/html_node/index.html)" that
    contains tutorials and explorations of topics not covered by the manual alone.

On the distro side:

  - The big change is that the package dependency graph is [rooted in a
    reduced set of “binary
    seeds”](https://guix.gnu.org/blog/2019/guix-reduces-bootstrap-seed-by-50/)—a
    huge step towards a fully auditable
    [bootstrap](https://guix.gnu.org/manual/en/html_node/Bootstrapping.html).
    There’s [more to come
    soon](https://fosdem.org/2020/schedule/event/gnumes/)!
  - The [graphical installer for Guix
    System](https://guix.gnu.org/manual/en/html_node/Guided-Graphical-Installation.html)
    benefited from many bug fixes and improvements.  Following the [bugs
    found in
    1.0.0](https://guix.gnu.org/blog/2019/gnu-guix-1.0.1-released/), we
    developed an [automated testing framework for the installer
    itself](https://issues.guix.gnu.org/issue/39729).  Continuous
    integration runs automated tests of the installer for different
    configurations (encrypted root, non-encrypted root, with or without
    a desktop environment, etc.).
  - 3,514 packages were added, for a total of more than [13K
    packages](https://guix.gnu.org/packages).  3,368 packages were
    upgraded.  The distribution comes with GNU libc 2.29, GCC 9.3,
    GNOME 3.32, MATE 1.24.0, Xfce 4.14.0, Linux-libre 5.4.28, and
    LibreOffice 6.4.2.2 to name a few.
  - 19 new services were added, notably providing support for running
    [NFS
    servers](https://guix.gnu.org/manual/devel/en/html_node/Network-File-System.html#index-nfs_002dservice_002dtype),
    configuring the [nftables
    firewall](https://guix.gnu.org/manual/devel/en/html_node/Networking-Services.html#index-nftables_002dservice_002dtype),
    or even a high-level Web service like
    [Patchwork](https://guix.gnu.org/manual/devel/en/html_node/Web-Services.html#index-patchwork_002dservice_002dtype).
  - Build systems for Node, Julia, and Qt were added, making it easier to
    write package definitions for these ecosystems.  In addition there is a
    new `copy-build-system` that does what you might expect.

At the programming interface level and under the hood, many things
changed as well, notably:

  - The new `with-build-handler` form allows us to better support
    _dynamic dependencies_ as introduced by
    [grafts](https://guix.gnu.org/manual/en/html_node/Security-Updates.html).
    More on that in a future post, but suffice to say that it fixes [a
    longstanding user
    interface](https://issues.guix.gnu.org/issue/28310) and [performance
    issue](https://issues.guix.gnu.org/issue/22990).
  - The `remote-eval` procedure in [`(guix
    remote)`](https://git.savannah.gnu.org/cgit/guix.git/tree/guix/remote.scm)
    supports remote execution of Scheme code as G-expressions after
    having first built and _deployed_ any code it relies on.  This
    capability was key to allowing code sharing between `guix deploy`,
    which operates on remote hosts, and `guix system reconfigure`.
    Similarly, there’s a new
    [`eval/container`](https://git.savannah.gnu.org/cgit/guix.git/tree/gnu/system/linux-container.scm#n226)
    procedure to run code in an automatically-provisioned container.
  - The new `lower-gexp` procedure returns a low-level intermediate
    representation of a G-expression.  `remote-eval`, `eval/container`,
    and `gexp->derivation` are expressed in terms of `lower-gexp`.
  - The
    [`with-parameters`](https://guix.gnu.org/manual/en/html_node/G_002dExpressions.html)
    form allows you, for instance, to _pin_ objects such as packages to a
    specific system or cross-compilation target.
  - Performance
    [was](https://lists.gnu.org/archive/html/guix-devel/2019-10/msg00350.html)
    [improved](https://lists.gnu.org/archive/html/guix-devel/2019-10/msg00650.html)
    for common low-level operations.

That’s a long list!  The [`NEWS`
file](https://git.savannah.gnu.org/cgit/guix.git/tree/NEWS?h=v1.1.0&id=d62c9b2671be55ae0305bebfda17b595f33797f2)
lists additional noteworthy changes and bug fixes you may be interested
in.

Enjoy!

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
