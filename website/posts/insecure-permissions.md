title: Insecure permissions on profile directory (CVE-2019-18192)
date: 2019-10-17 22:15
author: Ludovic Courtès
tags: Security
---
We have become aware of a security issue for Guix on multi-user systems
[that we have just fixed](https://issues.guix.gnu.org/issue/37744)
([CVE-2019-18192](https://nvd.nist.gov/vuln/detail?vulnId=CVE-2019-18192)).
Anyone running Guix on a multi-user system is encouraged to upgrade
`guix-daemon`—see below for instructions.

# Context

The default user profile, `~/.guix-profile`, points to
`/var/guix/profiles/per-user/$USER`.  Until now,
`/var/guix/profiles/per-user` was world-writable, allowing the `guix`
command to create the `$USER` sub-directory.

On a multi-user system, this allowed a malicious user to create and
populate that `$USER` sub-directory for another user that had not yet
logged in.  Since `/var/…/$USER` is in `$PATH`, the target user could
end up running attacker-provided code.  See
https://issues.guix.gnu.org/issue/37744 for more information.

This issue was initially [reported by Michael Orlitzky for
Nix](https://www.openwall.com/lists/oss-security/2019/10/09/4)
([CVE-2019-17365](https://nvd.nist.gov/vuln/detail?vulnId=CVE-2019-17365)).

# Fix

The [fix](https://issues.guix.gnu.org/issue/37744) consists in letting
`guix-daemon` create these directories on behalf of users and removing
the world-writable permissions on `per-user`.

For [cluster
setups](https://hpc.guix.info/blog/2017/11/installing-guix-on-a-cluster/)
where clients connect to the daemon over TCP ([thanks to the `--listen`
option of
`guix-daemon`](https://guix.gnu.org/manual/en/html_node/Invoking-guix_002ddaemon.html)),
the fix _requires_ `guix-daemon` to be able to resolve user names so
that it can create `/var/…/per-user/$USER` with the right ownership.
Note also that the `guix` command prior to this fix would not
communicate the user name it’s running under to the daemon, thereby
preventing it from creating that directory on its behalf.

# Upgrading

On multi-user systems, we recommend upgrading the daemon now.

To upgrade the daemon On Guix System, run:

```
guix pull
sudo guix system reconfigure /etc/config.scm
sudo herd restart guix-daemon
```

On other distros, run something along these lines:

```
sudo guix pull
sudo systemctl restart guix-daemon.service
```

Once you’ve run `guix build hello` or any other `guix` command, you
should see that `/var/guix/profiles/per-user` is no longer
world-writable:

```
$ ls -ld /var/guix/profiles/per-user
drwxr-xr-x 5 root root 4096 Jun 23  2017 /var/guix/profiles/per-user
```

Please report any issues you may have to
[`guix-devel@gnu.org`](https://guix.gnu.org/contact/).  See the
[security web page](https://guix.gnu.org/security/) for information on
how to report security issues.

#### About GNU Guix

[GNU Guix](https://www.gnu.org/software/guix) is a transactional package
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
