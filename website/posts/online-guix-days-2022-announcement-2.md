title: Online Guix Day Conference: schedule released!
date: 2022-02-14 00:00
author: Guix Hackers
slug: online-guix-days-2022-announcement-2
tags: Conference, Community
---

The Guix hackers are very happy to announce the second online Guix Days
Conference on **Saturday and Sunday, 19 & 20 February 2022**.  This conference
is open to everyone (**no registration fee**) and will be held entirely online.
Want to know the schedule, read on!

*There will be no live talks during the Guix Days!  Please watch the talks beforehand.*

![Guix Days logo](/static/blog/img/Guix-Days-online-2022.png)

[Join us live](https://meet.univ-grenoble-alpes.fr/b/pie-uia-2a2-wzl)
on the 19 and 20 to participate in the various sessions!

Live discussions will take place on Saturday and Sunday, and the agenda is
the following (UTC+1, Paris time). Schedule is subject to change:

### Saturday

 - 10:00–10:35: (BoF) **10 years of Guix - a retrospective (tbd)**
 - 10:40–11:15: **[Making Images for AWS Lambda Functions and Deploying Them with Guix](https://xana.lepiller.eu/guix-days-2022/guix-days-2022-guix-aws-lambda.mkv)**
 - 11:20–11:55: **[Gaming on Guix](https://xana.lepiller.eu/guix-days-2022/guix-days-2022-guix-gaming.mp4)**

 `(break)`

 - 13:00–13:35: **Spontaneous topics**
 - 13:40–14:15: **[A Deep Dive into the Guile Documentation & Makeover Proposal](https://xana.lepiller.eu/guix-days-2022/guix-days-2022-documentation.mp4)**
 - 14:20–14:55: **Spontaneous topics**

 `(break)`

 - 16:00–16:35: (BoF) **WhereisEveryone, Guix 'R Us, Online Meetups**
 - 16:40–17:15: **[My experiences as a newcomer to Guix](https://xana.lepiller.eu/guix-days-2022/guix-days-2022-experience-newcomer.mp4)**
 - 17:20–17:55: **[How GNU Guix saved me when my laptop caught water,  how it didn't, and where it couldn't](https://xana.lepiller.eu/guix-days-2022/guix-days-2022-how-gnu-guix-saved-me.mkv)**


### Sunday

 - 10:00–10:35: (BoF) **bordeaux.guix.gnu.org, Guix Build Coordinator  and related topics Q&A**
 - 10:40–11:15: **[Dreaming of better patch review](https://xana.lepiller.eu/guix-days-2022/guix-days-2022-patch-review.mp4)**
 - 11:20–11:55: **Spontaneous topics**

 `(break)`

 - 13:00–13:35: **[Modernizing python-build-system](https://xana.lepiller.eu/guix-days-2022/guix-days-2022-modernizing-python-build-system.mkv)**
 - 13:40–14:15: (BoF) **Guix Installer**
 - 14:20–14:55: **Spontaneous topics**

 `(break)`

 - 16:00–18:00: **Future of Guix (tbd)**

Each session will be question/answer and discussion related to the topic via
the BigBlueButton instance.

The slots are short so please watch the
[videos](https://xana.lepiller.eu/guix-days-2022/) beforehand to better enjoy
the discussions.  The term BoF means open discussion to address prospects.  The
last discussion may be longer depending on what you have to share.

The main channel for the day will be the video chat and questions will be asked
via the chat hosted there or––because we love it––via `#guix` on
[`libera.chat`](https://guix.gnu.org/en/contact/irc/) then the floor
might be shared, opening more mics.  The discussions will not be recorded
because we would like to keep them informal––where people are less impressed to
share their point of views.

**The [Code of
Conduct](http://git.savannah.gnu.org/cgit/guix.git/tree/CODE-OF-CONDUCT)
applies for all the channels of communication.**

##### 10 years of Guix - a retrospective
Chaired by *GNU Guix Maintainers*

This session will present the various milestones reached by the project
during the 10 years of its existence, from its first commit in 2012 to
today, in 2022.

##### Making Images for AWS Lambda Functions and Deploying Them with Guix
Presented by *George Vafeiadis* and *Phil Beadling*.
(**[video mkv](https://xana.lepiller.eu/guix-days-2022/guix-days-2022-guix-aws-lambda.mkv)**)

AWS Lambda is an event driven, serverless compute service which is
provisioned using custom-made Docker images.  We were interested in seeing if
we could harness Guix's Docker output to produce AWS compatible input images.
Using Guix has enabled us to cut a lot of bulk out of the base image to
produce a lean result.  The talk will cover wrapping of the Lambda Python
Runtime Interface Client in Guix, the various challenges in doing so, and
an overview of the resulting pipeline that we can plug into our CI/CD system.
We will provide example code and packages which people can then build upon
(the ultimate aim is to submit these packages to the official Guix channel).

##### Gaming on Guix
Presented by *Tobias Alexandra Platen*.
(**[video mp4](https://xana.lepiller.eu/guix-days-2022/guix-days-2022-guix-gaming.mp4)**)

This year the I Love Free Software Day 2022, includes Free Software
games such as Veloren and Armagetron Advanced. Since I only
play free software Games, I cant play VRChat, so I decided to go with
V-Sekai instead, VR platform powered by the Godot Engine. 
I plan to package those games for GUIX.

##### A Deep Dive into the Guile Documentation & Makeover Proposal
Presented by *Blake Shaw*.

Recent discussions on the Guix mailing list revealed that many in the Guix
community have found the Guile Reference Manual difficult to navigate as
newcomers.  That should come as no surprise — in PDF form, the docs span
approximately *850 pages*, making it a quite hefty set of documents for an
implementation of a minimal programming language like Scheme, even when
compared to the documentation of relatively large PLs; the Racket Guide,
for instance, is only 450 pages, while the Rust Book is approximately 550
pages.

Serving at the same time as a reference manual & API specification, the
large size may in part be attributed to what simultaneously makes Guile an
appealing project to contribute to, while also rendering the documentation
process somewhat delicate: Guile is a massive collective project featuring
the contributions of many authors over the course of three decades,
contributions which Guilers would hate to trivialize or treat as insignificant
or edit away on a whim.  Additionally, Guile comes from a long set of
traditions within Scheme hacking which itself is deep with sage wisdom
spanning many pedagogical philosophies and one of the greatest literature
traditions of hacker culture.  Is it possible to perform a makeover of the
Guile Documentation while respecting these historical threads, at the same time
rendering it more approachable for new users while not forsaking the deep
nuggets of wisdom that lie therein?

##### WhereisEveryone, Guix 'R Us, Online Meetups
Chaired by *jgart*.

In this session I'll give an introduction and short tour of the WhereisEveryone
community and associated Guix 'R Us channel.  the session will cover how people
can get involved with the project as well as how we see ourselves fitting
into the scheme of contributing to GNU Guix upstream through regular
online collaboration and organized meetups.  The session is open to prospective
as well as experienced Guix contributors alike.  An open discussion will follow
to discuss the strategies presented and how we might improve and extend them.

##### My experiences as a newcomer to Guix
Presented by *John Kehayias*.
(**[video mp4](https://xana.lepiller.eu/guix-days-2022/guix-days-2022-experience-newcomer.mp4)**)

Just over 6 months ago I had never heard of GNU Guix or knew what it was,
and now I'm submitting patches and enjoy hacking on my Guix system constantly
(and spending too much time on IRC).  In this talk I will answer how I got
here, what my experiences have been, and what I've learned as a newbie to
not-quite-a-newbie.

I have been a Linux user for many years, from Debian in the early 32bit to
64bit era, to compiling kernels for a Gentoo media box, and more recently
Arch as my full-time distro on 3 computers (yes, insert Arch meme).  But
now all I want is Guix on everything.

Along the way I had to learn what Guix is, how to use it, and how to hack on
it.  There are clear highlights like being able to use Scheme everywhere and
the cleanliness of a declarative, atomic system, but also rough spots in a
still growing distro and community, like adapting to mailing lists, patch
review, and the "Guix way."  I hope my perspectives will be interesting
for other newcomers to learn from, as well as the more experienced Guix-ers
to help continue the development of GNU Guix.

##### How GNU Guix saved me when my laptop caught water,  how it didn't, and where it couldn't
Presented by *Liliana Marie Prikler*.
(**[video mkv](https://xana.lepiller.eu/guix-days-2022/guix-days-2022-how-gnu-guix-saved-me.mkv)**)

Based on events that actually occurred, this talk shows how to
1. use Guix on a foreign distribution to get a configuration made on
   Guix System running
2. use Guix' containerization to access files (and services) on a third
   machine without modifying data on that machine (e.g. gratuitously
   copying files to $HOME).

Guix is discussed as an alternative package manager similar to Flatpak
or Snap, along with reasons to choose it over other solutions
(particularly some "rarely" discussed UI reasons).
In addition, practical applications of Guix' sandboxed environments are
shown by the examples of
1. editing files with GNU Emacs while interacting with git through
   libsecret
2. reading mail and accessing remote files with GNOME Online Accounts.

For wider context, this talk shows how Guix can help creating
manageable backups in the form of manifests.

##### bordeaux.guix.gnu.org, Guix Build Coordinator  and related topics Q&A
Chaired by *Christopher Baines*.

bordeaux.guix.gnu.org is a relatively new addition to the default
substitute servers, using the Guix Build Coordinator as a key component
to build packages and provide substitutes.

This session will be a chance for live questions and discussion about
bordeaux.guix.gnu.org, the Guix Build Coordinator and any related
topics.

##### Dreaming of better patch review
Presented by *Arun Isaac*.
(**[video mp4](https://xana.lepiller.eu/guix-days-2022/guix-days-2022-patch-review.mp4)**)

The Guix project is growing, and growing rapidly.  Users and
contributors are pouring in with their patches and bug
reports.  Maintainers, committers and reviewers are unable to keep
up.  A few (not me!) are taking on a disproportionate amount of the
work.  Contributors are frustrated that their patches don't get
reviewed or accepted in time.  Reviewers are cracking under the work
load, and silently tuning out.  This situation is obviously
unsustainable, and demands urgent attention.  What do we do?!

We try to motivate more of our reviewers to chip in, and gently
encourage them to help out more.  But, we all have busy lives, and it
is impractical to appeal simply to the goodness of our hearts.  We need
to streamline our review process and make it so easy that reviewers
will want to review.  Suggestions to use more sophisticated and modern
issue trackers such as those popularized by GitHub and GitLab come up
time and again.  But, I believe that Guix is a large enough and a
distinctive enough project to deserve its own customized issue tracker
tooling.

In this talk, I will dream up how our patch review could be better.  I
will describe problems and present mockups bereft of
implementation.  The hope is that this talk will outline the
possibilities for future work and inspire people to pitch in with
code.  If there is time towards the end, I will also present mumi's new
GraphQL API.

##### Modernizing python-build-system
Presented by *Lars-Dominik Braun*.
(**[video mkv](https://xana.lepiller.eu/guix-days-2022/guix-days-2022-modernizing-python-build-system.mkv)**)

Python is moving away from having a single go-to solution for packaging.
Setuptool’s well-known `setup.py` is currently in the process of being
replaced by pluggable build systems as specified by PEP 517, allowing
alternative tools like poetry, flit and others to enter the room.

Currently Guix’s python-build-system is not equipped to deal with
packages that require a PEP 517-compatible build environment.  Therefore
they need custom 'build and 'install phases.  Thus python-build-system
needs to move forward as the ecosystem it is building.  In this talk I
would like to present my ideas for a modern Python build system.

##### Guix Installer
Chaired by *Josselin Poiret*.

This session will discuss the recent development in the installer.

##### Future of Guix
Chaired by *GNU Guix maintainers*.

The session covers the medium- and long-term goals that may or may not look
realistic.  Pragmatic dream!


#### Code of Conduct

This online conference is an official Guix event.  Therefore, the [Code of
Conduct](http://git.savannah.gnu.org/cgit/guix.git/tree/CODE-OF-CONDUCT)
applies.  Please be sure to read it beforehand!

If you witness violations of the code of conduct during the event, please
email `guix-days@gnu.org`, a private email alias that reaches the organizers
(Simon `zimoun` Tournier and Julien `roptat` Lepiller) and the GNU Guix
maintainers.


#### About GNU Guix

[GNU Guix](https://www.gnu.org/software/guix) is a transactional package
manager and an advanced distribution of the GNU system that [respects
user
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

