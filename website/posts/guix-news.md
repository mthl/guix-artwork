title: Spreading the news
date: 2019-11-12 14:00
author: Ludovic Courtès
tags: Community, Scheme API
---

Like most free software projects, Guix has no shortage of communication
channels: there’s [this blog](https://guix.gnu.org/blog), the [`NEWS`
file](https://git.savannah.gnu.org/cgit/guix.git/tree/NEWS) for release
notes, a bunch of [mailing lists](https://guix.gnu.org/contact), an IRC
channel, there’s also an [unofficial
sub-Reddit](https://www.reddit.com/r/GUIX/) and certainly more.  Yet, as
developers, we often find it hard to communicate important changes to
our users.  Starting from a few weeks ago, `guix pull --news` tells
users what’s new, and it already feels very helpful!  This post is about
our motivations and the implementation of this new feature.

# Getting the word out

Developers keep adding crazy features, fixing bugs, and generally
improving things.  But how good is it if users aren’t aware of these new
things?  As an example, since June, our build farm has been [offering
lzip-compressed
binaries](https://guix.gnu.org/blog/2019/substitutes-are-now-available-as-lzip/),
which results in better performance when installing software.  But to
take advantage of that, users need to be aware of its existence, and
they need to upgrade their Guix daemon.  Likewise, how do we get people
to learn about [the new `guix deploy`
command](https://guix.gnu.org/blog/2019/managing-servers-with-gnu-guix-a-tutorial/)
that’s now available at their fingertips, about [security issues that
were fixed](https://guix.gnu.org/blog/tags/security-advisory/), about
important [infrastructure
changes](https://lists.gnu.org/archive/html/info-guix/2019-06/msg00001.html),
new options added to existing commands, and so forth?

Our (frustrating!) experience has been that release notes, blog posts,
and mailing list announcements aren’t quite enough to get the word out.
There’s always people who’ll miss important info and realize when it’s
already late, sometimes too late.  Hence this simple idea: wouldn’t it
be nice if important information would reach users right in their
terminal?

# `guix pull` news

Alright, that’s not exactly a novel idea!  In Debian for example,
[`apt-listchanges`](https://manpages.debian.org/testing/apt-listchanges/apt-listchanges.1.en.html)
shows news at the level of individual packages, taken from the
`NEWS.Debian` or `changelog.Debian` files that package maintainers
update.  In addition, `apt dist-upgrade` and similar commands typically
display dialog boxes and menus when special actions need to be taken
when upgrading.  That’s more or less what we’re looking for.

The situation in Guix is a little different: all of it lives in [a
single Git repository](https://git.savannah.gnu.org/cgit/guix.git/) that
contains the core, the command-line interfaces, as well as package
definitions and operating system service definitions.  The [`guix
pull`](https://guix.gnu.org/manual/devel/en/html_node/Invoking-guix-pull.html)
command is sort-of equivalent to `apt update`, except that it updates
not only the set of available packages but also the `guix` tools
themselves and all the [operating system
interfaces](https://guix.gnu.org/manual/devel/en/html_node/Using-the-Configuration-System.html).
Under the hood, `guix pull` essentially does `git pull` and consequently
updates all of this.  Guix very much follows a “rolling release” model.

For some time already, `guix pull` has been able to extract information
about new and upgraded packages and [to present it to the
user](https://guix.gnu.org/blog/2018/multi-dimensional-transactions-and-rollbacks-oh-my/).
Our goal was to complement it with high-level information about
important changes written with users in mind.  Clearly, showing the Git
commit log is not an option: commit logs are meant for developers and
there’s roughly a thousand commits per month—way too much information.
We needed high-level news entries, explicitly written for users.

The end result is this: `guix pull` now displays, in addition to a
summary of the new and upgraded packages, the headlines of applicable
news entries contributed by developers.  Users can view the details by
running `guix pull --news`:

!['guix pull' displaying news.](https://guix.gnu.org/static/blog/img/guix-pull-news.gif)

Users can no longer miss the news, for the benefit of both users and
developers!

# Under the hood

How does this all work?  There were several goals and constraints.
First, like commit logs, our high-level news entries should be anchored
in the Git history.  Second, unlike commit logs, it should be possible
to amend them—to fix typos, provide additional info, and so on.  Third,
the project has been paying a lot of attention to internationalization,
with translations available for [user interface
messages](https://translationproject.org/domain/guix.html), for [package
descriptions](https://translationproject.org/domain/guix-packages.html),
and for the [user manual](https://guix.gnu.org/manual/)—it’s one of
these things that helps free software reach out to more people; thus, we
naturally wanted news to be internationalized.  Last, since Guix
supports third-party
[“channels”](https://guix.gnu.org/manual/devel/en/html_node/Channels.html),
which are extensions of the official `guix` channel, why not provide
channel authors access to that news feature?

With all these things in mind, we designed a simple [news
format](https://guix.gnu.org/manual/devel/en/html_node/Channels.html#Writing-Channel-News).
In essence, channel authors, including Guix developers, can provide a
news file that looks [like
this](https://git.savannah.gnu.org/cgit/guix.git/tree/etc/news.scm):

```scheme
(channel-news
 (version 0)

 (entry (commit "3e962e59d849e4300e447d94487684102d9d412e")
        (title (en "@command{guix graph} now supports package
transformations")
               (de "@command{guix graph} unterstützt nun Paketumwandlungen"))
        (body
         (en "The @command{guix graph} command now supports the common package
transformation options (see @command{info \"(guix) Package Transformation
Options\"}).  This is useful in particular to see the effect of the
@option{--with-input} dependency graph rewriting option.")
         (de "Der Befehl @command{guix graph} unterstützt nun die mit anderen
Befehlen gemeinsamen Umwandlungsoptionen (siehe @command{info \"(guix.de)
Paketumwandlungsoptionen\"}).  Sie helfen insbesondere dabei, die Wirkung der
Befehlszeilenoption @option{--with-input} zum Umschreiben des
Abhängigkeitsgraphen zu sehen.")))

 (entry (commit "49af34cfac89d384c46269bfd9388b2c73b1220a")
        (title (en "@command{guix pull} now honors
@file{/etc/guix/channels.scm}")
               (es "Ahora @command{guix pull} tiene en cuenta
@file{/etc/guix/channels.scm}"))
        (body
         (en "The @command{guix pull} command will now read the
@file{/etc/guix/channels.scm} file if it exists and if the per-user
@file{~/.config/guix/channels.scm} is not present.  This allows administrators
of multi-user systems to define site-wide defaults.")
         (es "Ahora la orden @command{guix pull} lee el fichero
@file{/etc/guix/channels.scm} si existe y el fichero personalizable
@file{~/.config/guix/channels.scm} no está presente. Esto permite a quienes
administran sistemas con múltiples usuarias definir valores predeterminados
en el sistema."))))
```

Each news entry refers to a commit, the commit that introduced the
change it documents, and it has a title and body.  Those can use Texinfo
markup for rich formatting, and translations can be provided directly
within the news file.

When `guix pull --news` runs, it determines which news entries are
applicable given the user’s previous Guix instance.  The `(guix
channels)` module provides a simple programming interface for that:

```scheme
(use-modules (guix channels) (srfi srfi-1))

(channel-news-for-commit (first %default-channels)
                         "66b707a7d2325daadeed5ca913637eea3a2628e7"
                         "ac19950507e941b6263f62f4ee4e8934c1b1598e")
⇒ (#<<channel-news-entry> commit: "3e962e59d849e4300e447d94487684102d9d412e" tag: #f title: (("en" . "@command{guix graph} now supports package\ntransformations") …) body: …> #<<channel-news-entry> commit: "49af34cfac89d384c46269bfd9388b2c73b1220a" tag: #f title: (("en" . "@command{guix pull} now honors\n@file{/etc/guix/channels.scm}") …) body: …>)
```

One thing is quite unusual (one might say “weird” :-)) about this news
format: it refers to commit IDs “in-band”.  In other words, unlike Git
commit logs, which are “out-of-band”, the news file is contained
_inside_ the repository that it refers to.  Technically, it means that
before pushing a news entry, one must make sure it refers to the right
commit ID (the news format allows you to refer to tags as well, but one
may not want to create tags for every “news-worthy” change).  Likewise,
rebasing might invalidate commit IDs that appear in the news file.  So
this whole “in-band” log has drawbacks, but the big win is that it
allows us to amend news entries to fix typos, add translations, and so
on.

# In other news…

Since it was applied [a bit more than a month
ago](https://issues.guix.gnu.org/issue/37413), we’ve already put the
news mechanism to good use on quite a few occasions: giving users
instructions on how to deal with locales after the last glibc upgrade,
giving them upgrade info for
[CVE-2019-18192](https://guix.gnu.org/blog/2019/insecure-permissions-on-profile-directory-cve-2019-18192/),
telling them about new command-line options, and more.

In parallel, given that reading the mailing lists is akin to “drinking
from a fire hose” as they say, Christopher Baines has been thinking
about how to provide regular development updates to interested users and
developers.  Chris [announced last
week](https://lists.gnu.org/archive/html/guix-devel/2019-11/msg00016.html)
a prototype of a “Guix Weekly News” web site that would aggregate
information about package updates automatically extracted from the [Guix
Data Service](http://data.guix.gnu.org/), along with manually written
updates.  It would seem that this service could readily grab info from
channel news as well.

What about you, what do you expect in terms of news distribution?  Join
us [on the mailing list and on IRC](https://guix.gnu.org/contact) and
let us know!

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
