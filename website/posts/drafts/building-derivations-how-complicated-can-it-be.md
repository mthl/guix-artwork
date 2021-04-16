title: Building derivations, how complicated can it be?
date: 2021-04-23 20:00
author: Christopher Baines
tags: Guix Build Coordinator, Continuous integration, Quality Assurance
---

[Derivations][derivations] are key to Guix, they're the low-level build instructions
used for things like packages, disk images, and most things than end
up in the store.

Around a year ago, the established approach to build derivations
across multiple machines was [daemon offloading][offloading]. This
offloading approach is mostly static in terms of the machines involved
and uses SSH to communicate and move things between machines.

[derivations]: https://guix.gnu.org/manual/en/html_node/Derivations.html
[offloading]: https://guix.gnu.org/manual/en/html_node/Daemon-Offload-Setup.html

The Guix Build Coordinator project set out to provide an alternative
approach, both to explore what's possible, but also to provide a
usable tool to address two specific use cases.

The first use case was building things (mostly packages) for the
purpose of providing substitutes. At the time, the daemon offloading
approach used on ci.guix.gnu.org which is the default source of
substitutes. This approach was not scaling particularly well, so there
was room for improvement.

The second use case was more aspirational, support various quality
assurance tasks, like building packages changed by patches, regularly
testing fixed output derivations, or building the same derivations
across different machines to test for hardware specific differences.

While both these tasks have quite a lot in common, there's still quite
a lot of differences, this in part led to a lot of flexibility in the
design of the Guix Build Coordinator.

# Architecture

Like offloading, the Guix Build Coordinator works in a centralised
manner. There's one coordinator process which manages state, and agent
processes run on the machines that perform builds. The coordinator
plans which builds to allocate to which agents, and agents ask the
coordinator for builds, which they then attempt.

Once agents complete a build, they send the log file and any outputs
back to the coordinator. This is shown in the diagram below. Note that
the Guix Build Coordinator doesn't actually take care of building the
individual derivations, that's still left to `guix-daemon`'s on the
machines involved.

![Guix Build Coordinator sequence diagram](/static/blog/img/build-coordinator-architecture.svg)

The builds to perform are worked through methodically, a build won't
start unless all the inputs are available. This behaviour replicates
what `guix-daemon` does, but across all the machines involved.

If agents can't setup to perform a build, they report this back to the
coordinator, which may then perform other builds to produce those
required inputs.

Currently HTTP is used when agents want to communicate to the
coordinator, although additional approaches could be implemented in
the future. Similarly, SQLite is used as the database, but from the
start there has been a plan to support PostgreSQL, but that's yet to
be implemented.

# Comparison to offloading

There's lots more to the internal workings of the Guix Build
Coordinator, but how does this compare to the daemon offloading
builds?

Starting from the outside and working in, the API for the Guix Build
Coordinator is all asynchronous. When you submit a build, you get an
ID back, which you can use to track the progress of that build. This
is in contrast to the way the daemon is used, where you keep a
connection established while builds are progressing.

Offloading is tightly integrated with the daemon, which can be both
useful as offloading can transparently apply to anything that would
otherwise be built locally. However, this can also be a limitation
since the daemon is one of the harder components to modify.

With offloading, `guix-daemon` reaches out to another machine, copies
over all the inputs and the derivation, and then starts the
build. Rather than doing this, the Guix Build Coordinator agent pulls
in the inputs and derivation using substitutes.

This pull approach has a few advantages, firstly it removes the need
to keep a large store on the machine running the coordinator, and this
large store requirement of using offloading became a problem in terms
of scalability for the offloading approach. Another advantage is that
it makes deploying agents easier, as they don't need to be reachable
from the coordinator over the network, which can be an issue with NATs
or virtual machines.

When offloading builds, the outputs would be copied back to the store
on build success. Instead, the Guix Build Coordinator agent sends the
outputs back as nar files. The coordinator would then process these
nar files to make substitutes available. This helps distribute the
work in generating the nar files, which can be quite expensive.

These differences may be subtle, but the architecture makes a big
difference, it's much easier to store and serve nars at scale if this
doesn't require a large store managed by a single guix-daemon.

There's also quite a few things in common with the daemon offloading
approach. Builds are still methodically performed across multiple
machines, and load is taken in to account when starting new builds.

# A basic example

Getting the Guix Build Coordinator up and running does require some
work, the following commands should get the coordinator and an agent
running on a single machine. First, you start the coordinator.

```sh
  guix-build-coordinator
```

Then in another terminal, you interact with the running coordinator to
create an agent in it's database.

```sh
  guix-build-coordinator agent new
```

Note the UUID of the generated agent.

```sh
  guix-build-coordinator agent <AGENT ID> password new
```

Note the generated password for the agent. With the UUID and password,
the agent can then be started.

```sh
  guix-build-coordinator-agent --uuid=<AGENT ID> --password=<AGENT PASSWORD>
```

At this point, both processes should be running and the
guix-build-coordinator should be logging requests from the agent.

In a third terminal, also at the root of the repository, generate a
derivation, and then instruct the coordinator to have it built.

```sh
  guix build --no-grafts -d hello
```

Note the derivation that is output.

```sh
  guix-build-coordinator build <DERIVATION FILE>
```

This will return a randomly generated UUID that represents the build.

While running from the command line is useful for development and
experimentation, there are [services in Guix for running the
coordinator and agents][services].

[services]: https://guix.gnu.org/en/manual/en/html_node/Guix-Services.html#Guix-Build-Coordinator

# Applications of the Guix Build Coordinator

While I think the Guix Build Coordinator is a better choice than
daemon offloading in some circumstances, it doesn't currently replace
it.

At a high level, the Guix Build Coordinator is useful where there's a
need to build derivations and do something with the outputs or build
results, more than just having the outputs in the local store. This
could be serving substitutes, or testing derivations for example.

At small scales, the additional complexity of the coordinator is
probably unnecessary, but when it's useful to use multiple machines,
either because of the resources that provides, or because of a more
diverse range of hardware, then it makes much more sense to use the
Guix Build Coordinator to coordinate what's going on.

# Looking forward

The Guix Build Coordinator isn't just an alternative to daemon
offloading, it's more a general toolkit for coordinating the building
of derivations.

Much of the functionality in the Guix Build Coordinator happens in
hooks. There are bits of code (hooks) that run when certain events
happen, like a build gets submitted, or a build finished
successfully. It's these hooks that are responsible for doing things
like processing nars to be served as substitutes, or submitting
retries for a failed build.

This hook to automatically retry building particular derivations is
particularly useful when trying to provide substitutes where you want
to lessen the impact of random failures, or for quality assurance
purposes, where you want more data to better identify problems.

There are also more features such as build and agent tags and build
priorities that can be really useful in some scenarios.

My hope is that the Guix Build Coordinator will enable a better
substitute experience for Guix users, as well as enabling a whole new
range of quality assurance tasks. It's already possible to see some
impact from the Guix Build Coordinator, but there's still much work to
do!

## Additional material

 - [Guix Build Coordinator Git repository](https://git.cbaines.net/guix/build-coordinator/)
 - [2020/04/17 - Initial announcement - Prototype tool for building derivations](https://lists.gnu.org/archive/html/guix-devel/2020-04/msg00323.html)
 - [2020/09/19 - \[PATCH 0/4\] Add package and services for the Guix Build Coordinator](https://issues.guix.gnu.org/43494)
 - [2020/11/17 - Thoughts on building things for substitutes and the Guix Build Coordinator](https://lists.gnu.org/archive/html/guix-devel/2020-11/msg00417.html)
 - [2020/11/22 - Guix Days 2020 - Progress so far on the Guix Build Coordinator](https://xana.lepiller.eu/guix-days-2020/guix-days-2020-christopher-baines-guix-build-coordinator.mp4)
 - [2021/02/09 - The Guix Build Coordinator in 2021](https://lists.gnu.org/archive/html/guix-devel/2021-02/msg00148.html)
 - [2021/02/14 - Getting the Guix Build Coordinator agent working on the Hurd](https://lists.gnu.org/archive/html/guix-devel/2021-02/msg00223.html)
 - [2021/03/08 - Hurd substitute availability (27.5%) and next steps?](https://lists.gnu.org/archive/html/guix-devel/2021-03/msg00074.html)

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
