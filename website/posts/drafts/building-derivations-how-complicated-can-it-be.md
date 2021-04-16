title: Building derivations, how complicated can it be?
date: 2021-04-16 15:00
author: Christopher Baines
tags: Guix Build Coordinator, Continuous integration, Quality Assurance
---

Or to put it another way, how complicated can you make it?

# Background

Around a year ago, the established approach to build derivations
across multiple machines was guix-daemon offloading. The Guix Build
Coordinator project set out to provide an alternative approach though,
aimed at addressing two specific use cases.

The first use case was building things for substitutes. At the time,
the guix-daemon offloading approach used on ci.guix.gnu.org, the
default source of substitutes for Guix users was not scaling
particularly well and there was an opportunity to improve on this.

The second use case was more aspirations, support various quality
assurance tasks, like building packages changed by patches, regularly
testing fixed output derivations, or building the same derivations
across different machines to test for hardware specific differences.

While both these tasks have quite a lot in common, there's still quite
a lot of differences, this in part led to a lot of flexibility in the
design of the Guix Build Coordinator.

# Architecture

Like offloading, the Guix Build Coordinator works in a centralised
manor. There's one coordinator process which manages state, and agent
processes run on the machines that perform builds. The coordinator
plans which builds to allocate to which agents, and agents ask the
coordinator for builds, which they then attempt.

Agents fetch derivations and build inputs using substitutes, and will
report back to the coordinator if they're unable to setup for a
build. If the setup failed because there are missing store items, the
Guix Build Coordinator by default will check for builds for these
store items, and if there are none, it'll submit a build.

The builds to perform are worked through methodically, every build has
a priority, and the build allocation strategies take this in to
account when planning what builds each agent should work on next. The
allocation strategy is very configurable, and the Guix Build
Coordinator comes with two currently.

The basic allocation strategy is the default. It doesn't assume that a
successful build has to exist for outputs to be available, which is
appropriate if you're trying to build some derivations in conjunction
with substitutes available from elsewhere. This allocation strategy is
very suited to general use cases, including building things for
quality assurance purposes.

The other allocation strategy is the derivation ordered one. This
allocation strategy assumes that the Guix Build Coordinator instance
is building the entire graph, so it expects that substitutes will only
be available if a successful build exists. This allocation strategy is
particularly suited for building things for serving substitutes.

# Comparison to offloading

There's lots more to the internal workings of the Guix Build
Coordinator, but how does this compare to the guix-daemon offloading
builds?

Starting from the outside and working in, the API for the Guix Build
Coordinator is all asynchronous. When you submit a build, you get an
ID back, which you can use to track the progress of that build. This
is in contrast to the way the guix-daemon is used, where you keep a
connection established while builds are progressing.

With offloading, the guix-daemon reaches out to another machine,
copies over all the inputs and the derivation, and then starts the
build. Rather than doing this, the Guix Build Coordinator agent pulls
in the inputs and derivation using substitutes. This removes the need
to keep a large store on the machine running the coordinator, and this
large store requirement of using offloading became a problem in terms
of scalability for the offloading approach.

Additionally, when offloading builds, the outputs would be copied back
to the store on build success. Instead, the Guix Build Coordinator
agent sends the outputs back as nar files. The coordinator would then
process these nar files to make substitutes available. This helps
distribute the work in generating the nar files, which can be quite
expensive.

These differences may be subtle, but the architecture makes a big
difference, it's much easier to store and serve nars at scale than it
is to maintain a large store.

One of the other innovations the Guix Build Coordinator brings is a
high degree of customizability through hooks. There are bits of code
(hooks) that run when certain events happen, like a build gets
submitted, or a build finished successfully. It's these hooks that are
responsible for doing things like processing nars to be served as
substitutes, or submitting retries for a failed build.

This hook to automatically retry building particular derivations is
particularly useful when trying to provide substitutes where you want
to lessen the impact of random failures, or for quality assurance
purposes, where you want more data to better identify problems.

In general, this approach with the hooks means I've started thinking
of the Guix Build Coordinator as an extensible toolkit for performing
Guix builds.

# Looking forward

My hope is that the Guix Build Coordinator will enable a better
substitute experience for Guix users, as well as enabling a whole new
range of quality assurance tasks. It's already possible to see some
impact from the Guix Build Coordinator, but there's still much work to
do!

## Additional reading

 - [Git repository](https://git.cbaines.net/guix/build-coordinator/)
 - [2020/04/17 - Initial announcement - Prototype tool for building derivations](https://lists.gnu.org/archive/html/guix-devel/2020-04/msg00323.html)
 - [2020/09/19 - \[PATCH 0/4\] Add package and services for the Guix Build Coordinator](https://issues.guix.gnu.org/43494)
 - [2020/11/17 - Thoughts on building things for substitutes and the Guix Build Coordinator](https://lists.gnu.org/archive/html/guix-devel/2020-11/msg00417.html)
 - [2020/11/22 - Guix Days 2020 - Progress so far on the Guix Build Coordinator](https://xana.lepiller.eu/guix-days-2020/guix-days-2020-christopher-baines-guix-build-coordinator.mp4)
 - [2021/02/09 - The Guix Build Coordinator in 2021](https://lists.gnu.org/archive/html/guix-devel/2021-02/msg00148.html)
 - [2021/02/14 - Getting the Guix Build Coordinator agent working on the Hurd](https://lists.gnu.org/archive/html/guix-devel/2021-02/msg00223.html)

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
