title: Cuirass 1.0 released
date: 2021-03-31 09:00
author: Mathieu Othacehe
tags: Cuirass, Releases, CI
---

We are pleased to announce the release of
[Cuirass](http://guix.gnu.org/cuirass/) version 1.0, after almost five years
of development and around 700 commits from 14 contributors.

Cuirass is the GNU Guix continuous integration software. It's a general
purpose build automation server written in [GNU
Guile](https://www.gnu.org/software/guile/) that checks out sources from VCS
repositories, execute build jobs and store build results in a
database. Cuirass also provides a web interface to monitor the build results.

Cuirass is running on the [GNU Guix build farm](https://ci.guix.gnu.org).

Since January, the project is funded through the NGI0 PET Fund, a fund
established by [NLnet](https://nlnet.nl/) with financial support from the
European Commission's Next Generation, as explained
[here](https://othacehe.org/gnu-guix-continuous-integration---nlnet-grant.html).

Thanks to this support, we were able to speed up the developments and finally
propose a first release of this software.  Many things have changed in Cuirass
over the years and now is the perfect time to give it a try.

Here are the highlights of this new release.

### Database

Let's start with the database that is the core of Cuirass.  Until recently
Cuirass was using an SQLite3 database.  This technological choice proved to be
quite challenging, and we had some troubles to make it scale as discussed
[here](https://lists.gnu.org/archive/html/guix-devel/2021-01/msg00127.html).

Cuirass now uses a PostgreSQL database, bringing the performance issues to an
end while providing much more stability.  Almost all the SQL queries are
covered by test cases.

### Specifications

In order to build some derivations, Cuirass first needs to be told what to
build.  Originally, an obscure association list describing the requested build
jobs had to be passed to Cuirass.

Cuirass now operates on specification records that are described
[here](http://guix.gnu.org/cuirass/manual/html_node/Specifications.html#Specifications).
This input format is much more easy to understand for the user.  It relies on
Guix [channels](https://guix.gnu.org/manual/en/html_node/Channels.html), which
are well-adopted.

Here are a few different specifications examples.

This will build all the packages of the `my-channel` channel.

```scheme
(list (specification
       (name "my-channel")
       (build '(channels my-channel))
       (channels
        (cons (channel
               (name 'my-channel)
               (url "https://my-channel.git"))
              %default-channels))))
```

This will build the `linux-libre` package on the default Guix channel master
branch.

```scheme
(list (specification
           (name "my-linux")
           (build '(packages "linux-libre"))))
```

Finally, this will build the packages declared in the `my-manifest` file of
the `my-channel` channel, against the `core-updates` branch of the default
Guix channel.

```scheme
(list (specification
       (name "my-core-manifest")
       (build '(manifests "my-manifest"))
       (channels
        (list (channel
               (name 'my-channel)
               (url "https://my-channel.git"))
              (channel
               (name 'guix)
               (url %default-channel-url)
               (branch "core-updates"))))))
```

For people willing to spare some parens, a specification edition form has been
implemented in the Web interface.

![Specification creation](/static/blog/img/cuirass-specification-form.png)

The Cuirass home page has also been updated to reflect this new input format.

### Notifications

This feature was rightfully requested many times as this is a basic of any
respectable CI system.  Cuirass can now report failing and fixed builds in
three different ways:

- By email on the
  [guix-ci@gnu.org](https://lists.gnu.org/mailman/listinfo/guix-ci) mailing
  list.

- Using Mastodon thanks to the
  [Guile-Mastodon](https://framagit.org/prouby/guile-mastodon) bindings.

- Using the RSS feed available [here](http://ci.guix.gnu.org/events/rss/).

### New build mode

The traditional way of building things in Cuirass is to send batches of
derivations that need to be built to the local [Guix
daemon](https://guix.gnu.org/manual/devel/en/guix.html#Invoking-guix_002ddaemon).
The daemon can possibly
[offload](https://guix.gnu.org/manual/devel/en/guix.html#Daemon-Offload-Setup)
those builds to other machines.  While it's probably the most sensible way to
proceed, this solution doesn't scale well and suffers from some limitations.

- There's no way to influence the scheduling decisions of the Guix daemon.
  It's quite delicate to prioritize builds or build machines from Cuirass.

- The Guix daemon doesn't offer much feedback.  Cuirass needs to parse the
  debug output of the daemon to detect build events such as start and stop
  events.

- Using a unique daemon means using unique build parameters such as build
  `timeout` and `max-silent-time` properties.  Some packages have different build
  properties and Cuirass cannot honor them.

- When relying heavily on offloading, the Guix daemon scales badly.  Builds
  that often take a longer time to complete, such as emulated builds can
  saturate the build queue.

For all those reasons, using a new build mode seemed like a necessary evil.
The rationale behind this new build mode is to have Cuirass communicate
directly with the Guix daemons of all the offloading machines.  Instead of
dealing with a single, local, Guix daemon, Cuirass can now interact with
several Guix daemons on remote machines.

The build jobs are not submitted to the local Guix daemon.  Instead, a remote
server dispatches build requests to the connect remote workers, according to
the build priorities.

The remote server and the connected workers communicate using [ZeroMQ](https://zeromq.org/) over
TCP.  The workers are able to discover the remote server using [Avahi](https://www.avahi.org/).

The built items are exchanged as
[substitutes](https://guix.gnu.org/manual/en/html_node/Substitutes.html) by
spawning [Guix
publish](https://guix.gnu.org/manual/devel/en/guix.html#Invoking-guix-publish)
servers both on the remote server and on each connected remote worker.

It seems more complex, and it is indeed more complex.  However, the
performance gains are real.

![Build machines CPU idle percentage](/static/blog/img/cuirass-idle-chart.png)

This chart shows the CPU idle time percentage of the GNU Guix build farm
machines.  The introduction of the remote building mechanism around January
2021 results in a much higher activity of the connected machines.

This remote build mode also unlocked new features such as:

- The live streaming of build logs from remote workers to Cuirass so that they
  can be browsed in real time through the web interface.

- The support for `timeout` and `max-silent-time` package properties.

- The support for specification and package priorities.

- The new "Workers status" and "Machine status" pages allowing to closely
  monitor remote machine activities.

The workers status page is accessible [here](http://ci.guix.gnu.org/workers).

![Workers status page](/static/blog/img/cuirass-workers-status.png)

The machine status page is accessible
[here](http://ci.guix.gnu.org/machine/hydra-guix-101).

![Machine status page](/static/blog/img/cuirass-machine-status.png)

### Web interface

Besides the features related to the specification record introduction, several
improvements were brought to the Web interface.

Some administration actions that previously required manual SQL intervention
can now be performed directly through the Web interface.

Any Cuirass administrator can now:

- Add a specification
- Edit a specification
- Delete a specification
- Cancel an evaluation pending builds
- Retry all builds of an evaluation
- Retry an evaluation
- Restart a build

The build page was also improved to display the build weather and a build
history.

![Build page](/static/blog/img/cuirass-build-page.png)

Several issues were also fixed such as the broken pagination and the negative
build duration.

### Metrics

Cuirass computes periodically various metrics such as:

- Average evaluation duration per specification (seconds).
- Difference between newly added derivations and built derivations per day.
- Average time required for an evaluation to start its builds.
- Evaluation completion speed.
- Sum of currently pending builds.
- Builds count per machine during the last day.
- Percentage of failed evaluations.

![Metrics](/static/blog/img/cuirass-metrics.png)

Those metrics can be browsed [here](http://ci.guix.gnu.org/metrics).

### Documentation

The Cuirass documentation is now updated to reflect those changes and can be
browsed [online](http://guix.gnu.org/cuirass/manual/).

The release itself is available on Cuirass [home
page](https://guix.gnu.org/en/cuirass/).

The Guix's Cuirass package as well as the Cuirass
[service](https://guix.gnu.org/manual/devel/en/guix.html#Continuous-Integration) were also updated.

### Going further

The NLNet grant will allow me to keep working on Cuirass for a couple more
months. This will hopefully help us to:

- Connect more armhf/aarch64 machines to the build farm.

- Fix the build dependencies [issue](https://issues.guix.gnu.org/46402)

- Add a substitutes availability API and its counterpart in GNU Guix to
  improve the the `channel-with-substitute-available`
  [procedure](https://guix.gnu.org/manual/devel/en/guix.html#Channels-with-Substitutes)
  to take a manifest argument. This way, the `guix pull` command can be
  instructed to only update to Guix revisions where the manifest packages are
  all substitutable.

This release is an important milestone as, combined with the recent substitute
[improvements](https://guix.gnu.org/en/blog/2021/getting-bytes-to-disk-more-quickly/),
the whole substitute availability & download speed situation is now largely
mitigated, at least on Intel architectures.

Don't hesitate to run your own Cuirass server to build stuff ahead of the GNU
Guix build farm, or to build your custom channels.  Also feel free to share
the features you would like to see in the next Cuirass release.
