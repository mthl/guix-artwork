title: Outreachy 'guix git log' internship wrap-up
date: 2021-04-08 17:00:00
author: Magali Lemes, Simon Tournier, Ludovic Courtès
tags: Outreachy
---

Magali Lemes [joined Guix in
December](https://guix.gnu.org/en/blog/2020/add-a-subcommand-showing-gnu-guix-history-of-all-packages/)
for a three-month internship with
[Outreachy](https://www.outreachy.org/).  Magali implemented a `guix git
log` command to browse the history of packaging changes, with mentoring
from Simon Tournier and Gábor Boskovits.  In this blog post, Magali and
Simon wrap up on what's been accomplished.

#### Magali

The first tasks I had to do were pretty simple and were mainly meant to both get
me acquainted with the source code and set the building blocks of the
project.  They were very important so that I could gradually build the
subcommand.  For starters, I created a Guix repository on
[Gitlab](https://gitlab.com/magalilemes/guix), so that I could push all the work
I had done there, tweaked a little bit of the source code, and then proceeded to
have the `guix git log` subcommand show the default channel checkout path.

From there on, I started adding options to the subcommand.  The `oneline`
option was the first and simplest option, and it pretty much emulates what
`git log --oneline` does: it displays the commit short hash id and
title. Afterwards, other options such as `format`, `pretty`, and `grep` came
along.  The possibility of retrieving commits from other channels---and not
only from the default one---was also implemented.  An example of invoking the
subcommand would be:

```
guix git log --oneline --grep=guile-git
```

The road to doing all of this wasn't always smooth.  Right at the beginning of
the internship, for instance, I struggled with getting a segmentation fault
error.  It was a known
[bug](https://gitlab.com/guile-git/guile-git/-/issues/21), and I was able to
[overcome
it](https://lists.gnu.org/archive/html/guix-devel/2020-12/msg00226.html).

I also got to participate in the one-day [Guix
workshop](https://guix.gnu.org/en/blog/2021/meet-guix-at-fosdem-2021/)---a
[FOSDEM 2021](https://fosdem.org/2021/) fringe event---and presented the work
I had done so far. It was quite nice demonstrating the subcommand, receiving
feedback and questions, and I could also get to know other things that were
being worked on in Guix.

In the
[post](https://guix.gnu.org/en/blog/2020/add-a-subcommand-showing-gnu-guix-history-of-all-packages/)
I wrote three months ago, I mention that I wish I could gain meaningful
experience and improve my communication skills.  I'm glad to say that I feel
like I was able to achieve that.  Sending emails, explaining what I had done,
and asking questions about what I had to do during the weekly meetings were a
few of the situations I had to face, and that made me improve these skills.  I
also had a taste of what it's like to take part in a free software project,
got to know a few people, and learned quite a lot about [Guile
Scheme](https://www.gnu.org/software/guile/).

One thing, though, that I wasn't able to implement in due time was the commit
limiting options, such as `guix git log --after=YYYY-MM-DD` and `guix git log
--before=YYYY-MM-DD`.

Hopefully, soon users will be able to invoke `guix git log`, and  have the
commit history from all Guix channels they have.

#### Simon

It was my first experience mentoring for the Outreachy program and now I am
glad I did it.  I have learnt a lot on various topics.  I had already
mentored interns occasionally, though it was the first time fully remote, not
on the same timezone, and with code on which I am not expert.  Thanks Gábor,
Ludovic and Ricardo for pushing me to jump in this journey.

Reading back the [initial
proposal](https://lists.gnu.org/archive/html/guix-devel/2020-09/msg00108.html)
coming from a 2019
[question](https://lists.gnu.org/archive/html/help-guix/2019-06/msg00098.html),
I am happy by the insofar Magali's internship.  It paves the way for
[future](https://lists.gnu.org/archive/html/guix-devel/2020-12/msg00141.html)
[proposals](https://lists.gnu.org/archive/html/guix-devel/2020-12/msg00170.html)
or the implementation of other `guix git` subcommands.

# Next Outreachy round & acknowledgment

Guix is participating in the upcoming Outreachy round.  If you are
[eligible](https://www.outreachy.org/docs/applicant/#eligibility),
please [get in touch with us](https://guix.gnu.org/en/contact) and
consider [applying by April
30th](https://www.outreachy.org/docs/applicant/#outreachy-schedule)!

In light of recent changes at the Free Software Foundation (FSF) and
Outreachy’s subsequent [decision to refuse funds coming from the
FSF](https://www.outreachy.org/blog/2021-03-23/fsf-participation-barred/),
we are grateful to Software Freedom Conservancy (SFC) for their
[decision to sponsor our upcoming
internship](https://sfconservancy.org/blog/2021/mar/23/outreachy-fsf/).
We are working on a longer-term solution so we can keep participating
in Outreachy.  In the meantime, thanks a lot, Conservancy!

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
