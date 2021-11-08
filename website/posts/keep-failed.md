title: Risk of local privilege escalation via guix-daemon (CVE-2021-27851)
date: 2021-03-18 13:00
author: Ludovic Courtès and Leo Famulari
tags: Security Advisory
---

A security vulnerability that can lead to local privilege escalation has been
found in
[`guix-daemon`](https://guix.gnu.org/manual/en/html_node/Invoking-guix_002ddaemon.html).
It affects multi-user setups in which `guix-daemon` runs locally.

It does _not_ affect multi-user setups where `guix-daemon` runs on a
separate machine and is accessed over the network via
`GUIX_DAEMON_SOCKET`, as is customary on [cluster
setups](https://hpc.guix.info/blog/2017/11/installing-guix-on-a-cluster/).
Exploitation is more difficult, but not impossible, on machines where
the Linux [protected
hardlinks](https://sysctl-explorer.net/fs/protected_hardlinks/) feature
is enabled, which is common — this is the case when the contents of
`/proc/sys/fs/protected_hardlinks` are `1`.

# Vulnerability

The attack consists in having an unprivileged user spawn a build process, for
instance with `guix build`, that makes its build directory world-writable.  The
user then creates a hardlink to a root-owned file such as `/etc/shadow` in that
build directory.  If the user passed the `--keep-failed` option and the build
eventually fails, the daemon changes ownership of the whole build tree,
including the hardlink, to the user.  At that point, the user has write access
to the target file.

This is [CVE-2021-27851](https://www.cve.org/CVERecord?id=CVE-2021-27851).

# Fix

This [bug](https://issues.guix.gnu.org/47229) has been
[fixed](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=ec7fb669945bfb47c5e1fdf7de3a5d07f7002ccf).
See below for upgrade instructions.

The fix consists in adding a root-owned “wrapper” directory in which the build
directory itself is located.  If the user passed the `--keep-failed` option and
the build fails, the `guix-daemon` first changes ownership of the build
directory, and then, in two stages, moves the build directory into the location
where users expect to find failed builds, roughly like this:

1. `chown -R USER /tmp/guix-build-foo.drv-0/top`
2. `mv /tmp/guix-build-foo.drv-0{,.pivot}`
3. `mv /tmp/guix-build-foo.drv-0.pivot/top /tmp/guix-build-foo.drv-0`

In step #1, `/tmp/guix-build-foo.drv-0` remains root-owned, with permissions of
`#o700`.  Thus, only root can change directory into it or into `top`.  Likewise in
step #2.

The build tree becomes accessible to the user once step #3 has succeeded, not
before.  These steps are performed after the package build scripts have stopped
running.

# Upgrading

On multi-user systems, we recommend upgrading the `guix-daemon` now.

To upgrade the daemon on Guix System, run something like:

```
guix pull
sudo guix system reconfigure /run/current-system/configuration.scm
sudo herd restart guix-daemon
```

On other distros, use something like this:

```
sudo --login guix pull
sudo systemctl restart guix-daemon.service
```

# Conclusions

One of the flagship features of GNU Guix is to enable unprivileged package
management, which includes building packages.  Building occurs in an [isolated
build environment](https://guix.gnu.org/manual/en/html_node/Build-Environment-Setup.html).
This environment is isolated from the rest of the system not only to control the
build process to implement the [functional packaging
model](https://guix.gnu.org/manual/en/html_node/Managing-Software-the-Guix-Way.html),
but also to protect the system from package build scripts.

Despite our best efforts, there is always the possibility that we have
overlooked something, as in this case.

This issue is tracked as
[bug #47229](https://issues.guix.gnu.org/47229); you can read the thread
for more information.

We are grateful to Nathan Nye of WhiteBeam Security for reporting this bug.

Please report any issues you may have to
[`guix-devel@gnu.org`](https://guix.gnu.org/en/contact/).  See the
[security web page](https://guix.gnu.org/en/security/) for information
on how to report security issues.

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
