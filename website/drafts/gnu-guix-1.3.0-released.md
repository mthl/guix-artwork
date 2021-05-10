title: GNU Guix 1.3.0 released
date: 2021-05-11 14:00:00
author: Ludovic Courtès, Maxim Cournoyer
slug: gnu-guix-1.3.0-released
tags: Releases, Security
---
![Image of a Guix test pilot.](https://guix.gnu.org/static/blog/img/test-pilot.png)

We are pleased to announce the release of GNU Guix version 1.3.0!

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

It’s been almost 6 months since the last release, during which 212
people contributed code and packages, and a number of people contributed
to other important tasks—code review, system administration,
translation, web site updates, Outreachy mentoring, and more.

There’s been more than 8,300 commits in that time frame, which we’ll
humbly try to summarize in these release notes.

### User experience

A distinguishing Guix feature is its support for _declarative
deployment_: instead of running a bunch of `guix install` and `guix
remove` commands, you run `guix package --manifest=manifest.scm`, where
`manifest.scm` lists the software you want to install in a snippet that
looks like this:

```scheme
;; This is 'manifest.scm'.
(specifications->manifest
  (list "emacs" "guile" "gcc-toolchain"))
```

Doing that installs exactly the packages listed.  You can have that file
under version control and share it with others, which is convenient.
Until now, one would have to write the manifest by hand—not
insurmountable, but still a barrier to someone willing to migrate to the
declarative model.

The [new `guix package --export-manifest`
command](https://guix.gnu.org/manual/en/html_node/Invoking-guix-package.html)
(and its companion `--export-channels` option) produces a manifest based
on the contents _of an existing profile_.  That makes it easy to
transition from the classic “imperative” model, where you run `guix
install` as needed, to the more formal declarative model.  This was long
awaited!

Users who like to always run the latest and greatest pieces of the free
software commons will love [the new `--with-latest` package
transformation
option](https://guix.gnu.org/manual/en/html_node/Package-Transformation-Options.html).
Using the same code as [`guix
refresh`](https://guix.gnu.org/manual/en/html_node/Invoking-guix-refresh.html),
this option looks for the latest _upstream_ release of a package,
fetches it, authenticates it, and builds it.  This is useful in cases
where the new version is not yet packaged in Guix.  For example, the
command below, if run today, will (attempt to) install QEMU 6.0.0:

```
$ guix install qemu --with-latest=qemu 
The following package will be upgraded:
   qemu 5.2.0 → 6.0.0

Starting download of /tmp/guix-file.eHO6MU
From https://download.qemu.org//qemu-6.0.0.tar.bz2...
 …0.tar.bz2  123.3MiB                                                                                                                      28.2MiB/s 00:04 [##################] 100.0%

Starting download of /tmp/guix-file.9NRlvT
From https://download.qemu.org//qemu-6.0.0.tar.bz2.sig...
 …tar.bz2.sig  310B                                                                                                                         1.2MiB/s 00:00 [##################] 100.0%
gpgv: Signature made Thu 29 Apr 2021 09:28:25 PM CEST
gpgv:                using RSA key CEACC9E15534EBABB82D3FA03353C9CEF108B584
gpgv: Good signature from "Michael Roth <michael.roth@amd.com>"
gpgv:                 aka "Michael Roth <mdroth@utexas.edu>"
gpgv:                 aka "Michael Roth <flukshun@gmail.com>"
The following derivation will be built:
   /gnu/store/ypz433vzsbg3vjp5374fr9lhsm7jjxa4-qemu-6.0.0.drv

…
```

There’s one obvious caveat: this is not guaranteed to work.  If the new
version has a different build system, or if it requires extra
dependencies compared to the version currently packaged, the build
process will fail.  Yet, it provides users with additional flexibility
which can be convenient at times.  For developers, it’s also a quick way
to check whether a given package successfully builds against the latest
version of one of its dependencies.

Several changes were made here and there to improve user experience.  As
an example, a [new `--verbosity`
level](https://guix.gnu.org/manual/devel/en/html_node/Common-Build-Options.html)
was added.  By default (`--verbosity=1`), fewer details about downloads
get printed, which matches the expectation of most users.

Another handy improvement is _suggestions_ when making typos:

```
$ guix package --export-manifests
guix package: error: export-manifests: unrecognized option
hint: Did you mean `export-manifest'?

$ guix remve vim
guix: remve: command not found
hint: Did you mean `remove'?

Try `guix --help' for more information.
```

People setting up [build offloading over
SSH](https://guix.gnu.org/manual/en/html_node/Daemon-Offload-Setup.html)
will enjoy the simplified process, where the `guile` executable no
longer needs to be in `PATH`, with appropriate `GUILE_LOAD_PATH`
settings, on target machines.  Instead, offloading now channels all its
operations through `guix repl`.

### Developer tools

We have good news for packagers!  First, [`guix
import`](https://guix.gnu.org/manual/en/html_node/Invoking-guix-import.html)
comes with a new Go recursive importer, that can create package
definitions or templates thereof for whole sets of Go packages.  The
`guix import crate` command, for Rust packages, now honors “semantic
versioning” when used in recursive mode.

The [`guix refresh`
command](https://guix.gnu.org/manual/en/html_node/Invoking-guix-refresh.html)
now includes new “updaters”: `sourceforge`, for code hosted on
SourceForge, and `generic-html` which, as the name implies, is a generic
update that works by scanning package home pages.  This greatly improves
`guix refresh` coverage.

Packages and developers may also like the new [`--with-patch` package
transformation
option](https://guix.gnu.org/manual/en/html_node/Package-Transformation-Options.html),
which provides a way to build a bunch of packages with a patch applied
to one or several of them.

  - ‘guix system image’
  - GUIX_EXTENSIONS_PATH for GWL

### Performance

One thing you will hopefully notice is that substitute installation
(downloading pre-built binaries) became faster, [as we explained
before](https://guix.gnu.org/en/blog/2021/getting-bytes-to-disk-more-quickly/).
This is in part due to the opportunistic use of zstd compression, which
has a high decompression throughput.  The daemon and [`guix
publish`](https://guix.gnu.org/manual/en/html_node/Invoking-guix-publish.html)
support zstd as an additional compression method, next to gzip and lzip.

Another change that can help fetch substitutes more quickly is _local
substitute server discovery_.  The [new `--discover` option of
`guix-daemon`](https://guix.gnu.org/manual/en/html_node/Invoking-guix_002ddaemon.html)
instructs it to discover and use substitute servers on the local-area
network (LAN) advertised with the mDNS/DNS-SD protocols, using Avahi.
Similarly, `guix publish` has a [new `--advertise`
option](https://guix.gnu.org/manual/en/html_node/Invoking-guix-publish.html)
to advertise itself on the LAN.

On Guix System, you can run `herd discover guix-daemon on` to turn
discovery on temporarily, or you can [enable it in your system
configuration](https://guix.gnu.org/manual/en/html_node/Base-Services.html#index-guix_002dconfiguration).
Opportunistic use of neighboring substitute servers _is entirely safe_,
[thanks to reproducible
builds](https://guix.gnu.org/en/blog/2017/reproducible-builds-a-status-update/).

In other news, [`guix system init` has been
optimized](https://issues.guix.gnu.org/44760#4), which contributes to
making Guix System installation faster.

  - channel-with-substitutes-available

### POWER9 support, packages, services, and more!

POWER9 support is now available as a technology preview, thanks to the
tireless work of those who helped [porting Guix to that
platform](https://guix.gnu.org/blog/2021/new-supported-platform-powerpc64le-linux/).
There aren't many POWER9 binary substitutes available yet, due to the
limited POWER9 capacity of our build farm, but if you are not afraid
of building many packages from source, we'd be thrilled to hear back
from your experience!

The package collection has been augmented with approximately 2000 new
packages, bringing its total count to 17670 packages; 3100 packages
were also updated.

The services catalog has also seen new additions such as
[wireguard](https://guix.gnu.org/manual/en/html_node/VPN-Services.html#index-wireguard_002dservice_002dtype),
[syncthing](https://guix.gnu.org/manual/en/html_node/Networking-Services.html#index-syncthing),
[ipfs](https://guix.gnu.org/manual/en/html_node/Networking-Services.html#index-IPFS),
and more!  You can search for services via the `guix system search`
facility.

The [`NEWS`
file](https://git.savannah.gnu.org/cgit/guix.git/tree/NEWS?h=version-1.3.0&id=2b76179ecd951172288f5f6f78402d9304d2da41)
lists additional noteworthy changes and bug fixes you may be
interested in.

### Try it!

The [installation script](https://guix.gnu.org/install.sh) has been
improved to allow for more automation. For example, if you are in a
hurry, you could run it with:

```
# yes | ./install.sh
```

to proceed to install the Guix binary on your system without any
prompt!

You may also be interested in trying the [Guix System demonstration VM
image](https://ftp.gnu.org/gnu/guix/guix-system-vm-image-1.3.0.x86_64-linux.qcow2)
which now supports clipboard integration with the host and dynamic
resizing thanks to the SPICE protocol, which we hope will improve the
user experience.

To review all the installation options at your disposal, consult [the
download page](https://guix.gnu.org/en/download/) and don't hesitate
to [get in touch with us](https://guix.gnu.org/en/contact/).

Enjoy!

### Credits

> Luis Felipe (illustration)

#### About GNU Guix

[GNU Guix](https://guix.gnu.org) is a transactional package manager and
an advanced distribution of the GNU system that [respects user
freedom](https://www.gnu.org/distros/free-system-distribution-guidelines.html).
Guix can be used on top of any system running the Hurd or the Linux
kernel, or it can be used as a standalone operating system distribution
for i686, x86_64, ARMv7, AArch64 and POWER9 machines.

In addition to standard package management features, Guix supports
transactional upgrades and roll-backs, unprivileged package management,
per-user profiles, and garbage collection.  When used as a standalone
GNU/Linux distribution, Guix offers a declarative, stateless approach to
operating system configuration management.  Guix is highly customizable
and hackable through [Guile](https://www.gnu.org/software/guile)
programming interfaces and extensions to the
[Scheme](http://schemers.org) language.
