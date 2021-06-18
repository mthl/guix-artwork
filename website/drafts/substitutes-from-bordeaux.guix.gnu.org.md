title: Substitutes now also available from bordeaux.guix.gnu.org
author: Christopher Baines
tags: Substitutes, Build farm
date: 2021-06-18 12:00:00
---

There have been a number of different project operated sources of
substitutes, for the last couple of years the default source of
substitutes has been [ci.guix.gnu.org][ci.guix.gnu.org] (with a few
different URLs).

[ci.guix.gnu.org]: https://ci.guix.gnu.org

Now, in addition to [ci.guix.gnu.org][ci.guix.gnu.org],
[bordeaux.guix.gnu.org][bordeaux.guix.gnu.org] is a default substitute
server.

[bordeaux.guix.gnu.org]: https://bordeaux.guix.gnu.org

Put that way, this development maybe doesn't sound particularly
interesting.  Why is a second substitute server useful?  There's some
thoughts on that exact question in the next section.  If you're just
interested in how to use (or how not to use) substitutes from
[bordeaux.guix.gnu.org][bordeaux.guix.gnu.org], then you can just skip
ahead to the last section.

# Why a second source of substitutes?

This change is an important milestone, following on from the work that
started on the [Guix Build Coordinator towards the start of
2020][guix-build-coordinator].

[guix-build-coordinator]: https://guix.gnu.org/en/blog/2021/building-derivations-how-complicated-can-it-be/

Back in 2020, the substitute availability from
[ci.guix.gnu.org][ci.guix.gnu.org] was often an issue.  There seemed
to be a number of contributing factors, including some parts of the
architecture.  Without going too much in to the details of the issues,
aspects of the design of the Guix Build Coordinator were specifically
meant to avoid some of these issues.

While there were some very positive results from testing back in 2020,
it's taken so long to bring the substitute availability benefits to
general users of Guix that [ci.guix.gnu.org][ci.guix.gnu.org] has
[changed and improved significantly in the meantime][cuirass-1.0].
This means that any benefits in terms of substitute availability are
less significant now.

[cuirass-1.0]: https://guix.gnu.org/en/blog/2021/cuirass-10-released/

One clearer benefit of just having two independent sources of
substitutes is redundancy.  While the availability of
[ci.guix.gnu.org][ci.guix.gnu.org] has been very high (in my opinion),
having a second independent substitute server should mean that if
there's a future issue with users accessing either source of
substitutes, the disruption should be reduced.

I'm also excited about the new possibilities offered by having a
second substitute server, particularly one using the Guix Build
Coordinator to manage the builds.

Substitutes for the Hurd is already something that's [been
prototyped][hurd-substitute-availability], so I'm hopeful that
[bordeaux.guix.gnu.org][bordeaux.guix.gnu.org] can start using
[childhurd VMs][childhurd] to build things soon.

[hurd-substitute-availability]: https://lists.gnu.org/archive/html/guix-devel/2021-03/msg00074.html
[childhurd]: https://guix.gnu.org/en/blog/2020/childhurds-and-substitutes/

Looking a bit further forward, I think there's some benefits to be had
in doing further work on how the nar and narinfo files used for
substitutes are managed. There are some [rough plans
already][handling-nars-narinfos-at-scale] on how to address the
retention of nars, and how to look at high performance mirrors.

[handling-nars-narinfos-at-scale]: https://lists.gnu.org/archive/html/guix-devel/2021-02/msg00104.html

Having two substitute servers is one step towards stronger trust
policies for substitutes ([as discussed on guix-devel][k-of-n-trust],
where you would only use a substitute if both
[ci.guix.gnu.org][ci.guix.gnu.org] and
[bordeaux.guix.gnu.org][bordeaux.guix.gnu.org] have built it exactly
the same.  This would help protect against the compromise of a single
substitute server.

[k-of-n-trust]: https://lists.gnu.org/archive/html/guix-devel/2020-06/msg00179.html

# Using substitutes from bordeaux.guix.gnu.org

If you're using Guix System, and haven't altered the default
substitute configuration, updating guix (via `guix pull`),
reconfiguring using the updated guix, and then restarting the
guix-daemon should enable substitutes from
[bordeaux.guix.gnu.org][bordeaux.guix.gnu.org].

If the ACL is being managed manually, you might need to add the public
key for [bordeaux.guix.gnu.org][bordeaux.guix.gnu.org] manually as
well.

When using Guix on a foreign distribution with the default substitute
configuration, you'll need to run `guix pull` as root, then restart
the guix-daemon.  You'll then need to add the public key for
[bordeaux.guix.gnu.org][bordeaux.guix.gnu.org] to the ACL.

```sh
guix archive --authorize < /root/.config/guix/current/share/guix/bordeaux.guix.gnu.org.pub
```

If you want to just use [ci.guix.gnu.org][ci.guix.gnu.org], or
[bordeaux.guix.gnu.org][bordeaux.guix.gnu.org] for that matter, you'll
need to adjust the substitute urls configuration for the guix-daemon
to just refer to the substitute servers you want to use.
