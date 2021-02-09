title: Risk of local privilege escalation via setuid programs
date: 2021-02-09 16:00
author: Ludovic Courtès
tags: Security Advisory
---

On Guix System, [setuid
programs](https://guix.gnu.org/manual/en/html_node/Setuid-Programs.html)
were, until now, installed as setuid-root _and_ setgid-root (in the
`/run/setuid-programs` directory).  However, most of these programs are
meant to run as setuid-root, but not setgid-root.  Thus, this setting
posed a risk of local privilege escalation (users of Guix on a “foreign
distro” are unaffected).

This bug has been
[fixed](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=aa8de806252e3835d57fab351b02d13db762deac)
and users are advised to upgrade their system, with commands along the
lines of:

```
guix pull
sudo guix system reconfigure /run/current-system/configuration.scm
```

This issue is tracked as [bug
#46305](https://issues.guix.gnu.org/46395); you can read the thread for
more information.  There are no known exploitation of this issue to
date.  Many thanks to Duncan Overbruck for reporting it.

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
