title: Meet Guix at FOSDEM
slug: meet-guix-at-fosdem-2020
date: 2020-01-10 14:30:00
author: Manolis Ragkousis
tags: FOSDEM, Guix Days, Talks, Community
---

As usual, GNU Guix will be present at [FOSDEM](https://fosdem.org/2020/)
on February 1st and 2nd.  This year, we’re happy to say that there will
be quite a few talks about Guix and related projects!

 - On Saturday afternoon, [*Guix: Unifying provisioning, deployment, and
   package management in the age of
   containers*](https://fosdem.org/2020/schedule/event/guix/) by Ludovic
   in the [main track on containers and
   security](https://fosdem.org/2020/schedule/track/containers_and_security/),
   will reflect on what GNU Guix
   has to offer to users and how it compares to other approaches—from
   CONDA and pip to Flatpak and Docker.
 - Sunday morning starts with Efraim talking in the Rust devroom about
   [*Packaging Rust programs in
   GNU Guix*](https://fosdem.org/2020/schedule/event/rust_packaging_gnu_guix/)—telling
   Rust programmers about the needs of a distro like Guix, and about the
   journey building infrastructure for Rust packages in Guix.
 - Next up is [*GNU Guix as an alternative to the
   Yocto Project*](https://fosdem.org/2020/schedule/event/ggaaattyp/) by
   Mathieu in the Distributions devroom will demonstrate how to use
   GNU Guix to build a root filesystem for an embedded device.
 - Later on in the HPC, Big Data, and Data Science devroom, Ludovic will
   present [*Towards reproducible Jupyter
   notebooks*](https://fosdem.org/2020/schedule/event/reprod_jupyter_guix/)
   which will talk about
   [Guix-Jupyter](https://hpc.guix.info/blog/2019/10/towards-reproducible-jupyter-notebooks/),
   which aims to make Jupyter
   notebook self-contained and to support reproducible deployment.
 - In the same devroom, Efraim will present [*Sharing Reproducible Results
   in a
   Container*](https://fosdem.org/2020/schedule/event/reprod_container/)
   about how Guix solves the issue of reproducibility and deployment of
   containers.
 - Finally in the Minimalistic, Experimental and Emerging Languages
   devroom, janneke with [*GNU Mes, Scheme-only bootstrap and
   beyond*](https://fosdem.org/2020/schedule/event/gnumes/) will report
   on three years of hard work tackling one of the most pressing
   security issues of operating systems—the “trusting trust” attack.
   Janneke will present exciting bootstrapping achievements and their
   integration in Guix.
 - Pierre will present [*Universal package & service discovery with
   Guix*](https://fosdem.org/2020/schedule/event/gnuguixpackagemanager/)
   on how he intends to leverage the Guile programming language to boost
   searchability of packages and services _via_ intuitive user interfaces
   and semantics.
 - Pjotr will talk about why minimalism matters in computing with [*Lisp
   everywhere!*](https://fosdem.org/2020/schedule/event/lispeverywhere/)
 - Andy will discuss all the work that has gone into the upcoming
   [Guile 3](https://www.gnu.org/software/guile/news)—which will soon
   power Guix—making it a faster implementation, with [*Celebrating
   Guile 2020*](https://fosdem.org/2020/schedule/event/guile2020/).
 - Last Chris Marusich will give an [*Introduction to
   G-Expressions*](https://fosdem.org/2020/schedule/event/gexpressionsguile),
   the magic behind
   [them](https://guix.gnu.org/manual/devel/en/html_node/G_002dExpressions.html),
   and how to use them in Guix.

The [Minimalistic, Experimental and Emerging Languages
devroom](https://fosdem.org/2020/schedule/track/minimalistic_experimental_and_emerging_languages/)
will also feature talks about about [Racket](https://racket-lang.org),
[Lua](https://www.lua.org/), [Crystal](https://crystal-lang.org/) and
[Nim](https://nim-lang.org/) that you should not miss under any
circumstances!

![Guix Days logo.](https://guix.gnu.org/static/blog/img/Guix-Days-2020.png)

For the third time, we are also organizing the Guix Days as a [FOSDEM
fringe event](https://fosdem.org/2020/fringe/), a two-day Guix workshop
where contributors and enthusiasts will meet.  The workshop takes place
on Thursday Jan. 30st and Friday Jan. 31st at the Institute of Cultural
Affairs (ICAB) in Brussels.

Again this year there will be few talks; instead, the event will
consist primarily of
“[unconference-style](https://en.wikipedia.org/wiki/Unconference)”
sessions focused on specific hot topics about Guix, the Shepherd,
continuous integration, and related tools and workflows.

Attendance to the workshop is free and open to everyone, though you are
invited to register (there are only a few seats left!).  Check out [the
workshop’s wiki
page](https://libreplanet.org/wiki/Group:Guix/FOSDEM2020) for
registration and practical info.  Hope to see you in Brussels!

#### About GNU Guix

[GNU Guix](https://guix.gnu.org) is a transactional package
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
