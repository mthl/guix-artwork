title: DRAFT Authenticated channels
author: Ludovic Courtès
---

Software deployment tools like Guix are in a key position when it comes
to securing the “software supply chain”—taking source code fresh from
repositories and providing users with ready-to-use binaries.  We have
been paying attention to several aspects of this problem in Guix:
[authentication of pre-built
binaries](https://guix.gnu.org/manual/en/html_node/Substitute-Authentication.html),
[reproducible
builds](https://guix.gnu.org/blog/tags/reproducible-builds/),
[bootstrapping](https://guix.gnu.org/blog/tags/bootstrapping/), and
[fast security
updates](https://guix.gnu.org/blog/2016/timely-delivery-of-security-updates/).

A couple of weeks ago, we addressed the elephant in the room:
authentication of Guix code itself by [`guix
pull`](https://guix.gnu.org/manual/en/html_node/Invoking-guix-pull.html),
the tool that updates Guix and its package collection.  This article
looks at what set out to address, how we achieved it, and how it
compares to existing work in this area.

# Securing updates

The problem of securing distro updates is often viewed through the lens
of binary distributions such as Debian, where the main asset to be
protected are binaries themselves.  The functional deployment model that
Guix and Nix implement is very different: conceptually, Guix is a
_source distribution_, like Gentoo if you will.

Pre-built binaries are of course available and very useful, but they’re
optional; we can them
[_substitutes_](https://guix.gnu.org/manual/en/html_node/Substitutes.html)
because they’re just that: substitutes for local builds.  When you do
choose to accept substitutes, they must be signed by one of the keys you
authorized (this has been the case since [version 0.6 in
2014](https://guix.gnu.org/blog/2014/gnu-guix-06-released/)).

Guix consists of source code for the tools as well as all the [package
definitions](https://guix.gnu.org/manual/en/html_node/Defining-Packages.html)—the
_distro_.  When users run `guix pull`, what happens behind the scene is
equivalent to `git clone` or `git pull`.  There are many ways this can
go wrong.  An attacker can trick the user into pulling code from an
alternate repository that contains malicious code or definitions for
backdoored packages.  This is made more difficult by the fact that code
is fetched over HTTPS from
[Savannah](https://git.savannah.gnu.org/cgit/guix.git/) by default, but
not impossible (FIXME: xref).  If Savannah is compromised ([as happened
in
2010](https://www.fsf.org/blogs/sysadmin/savannah-and-www.gnu.org-downtime)),
an attacker can push code to the Guix repository, which everyone would
pull.  The change might even go unnoticed and remain in the repository
forever.  An attacker with access to Savannah can also reset the main
branch to an earlier revision, leading users to install outdated
software with known vulunerabilities—a _downgrade attack_.  These are
the kind of attacks we want to protect against.

# Authenticating Git checkouts

If we take a step back, the problem we’re trying to solve is not
specific to Guix and to software deployment tools: it’s about
_authenticating Git checkouts_.  By that, we mean that when `guix pull`
obtains code from Git, it should be able to tell that all the commits it
fetched were pushed by authorized developers of the project.  We’re
really looking at individual commits, not tags, because users can choose
to pool at arbitrary point in the commit history of Guix and Guix
[channels](https://guix.gnu.org/manual/en/html_node/Channels.html).

Checkout authentication requires [cryptographically signed
commits](https://git-scm.com/book/en/v2/Git-Tools-Signing-Your-Work).
By signing a commit, a Guix developer takes reponsibility asserts that
they are the one who made the commit; they may be its author, or they
may be the person who applied somebody else’s changes after review.  It
also requires a notion of authorization: we don’t simply want commits to
have a valid signature, we want them to be signed by an authorized key.
The set of authorized keys changes over time as people join and leave
the project.

To implement that, we came up with the following mechanism and rule:

  1. The repository contains a [`.guix-authorizations`
     file](https://git.savannah.gnu.org/cgit/guix.git/tree/.guix-authorizations)
     that lists the OpenPGP key fingerprints of authorized committers.
  2. A commit is considered authentic if and only if it is signed by one
     of the keys listed in the `.guix-authorizations` file of each of
     its parent(s).  This is the _authorization invariant_.

(Remember that Git commits form a directed acyclic graph (DAG) where
each commit can have zero or more parents; merge commits have two parent
commits, for instance.  Do not miss [_Git for Computer
Scientists_](https://eagain.net/articles/git-for-computer-scientists/)
for a pedagogical overview!)

Let’s take an example to illustrate.  In the figure below, each box is a
commit, and each arrow is a parent relationship:

![Example commit graph.](https://guix.gnu.org/static/blog/img/commit-graph.svg)

This figure shows two lines of development: the orange line may be the
main development branch, while the purple line may correspond to a
feature branch that was eventually merged in commit _F_.  _F_ is a merge
commit, so it has two parents: _D_ and _E_.

Labels next to boxes show who’s in `.guix-authorizations`: for commit A,
only Alice is an authorized committer, and for all the other commits,
both Bob and Alice are authorized committers.  For each commit, we see
that the authorization invariant holds; for example:

  - commit _B_ was made by Alice, who was the only authorized committer
    in its parent, commit _A_;
  - commit _C_ was made by Bob, who was among the authorized committers
    as of commit _B_;
  - commit _F_ was made by Alice, who was among the authorized
    committers of both parents, commits _D_ and _E_.

The authorization invariant has the nice property that it’s simple to
state, and it’s simple to check and enforce.  This is what `guix pull`
implements.  If your current Guix, [as returned by `guix
describe`](https://guix.gnu.org/manual/en/html_node/Invoking-guix-describe.html),
is at commit _A_ and you want to pull to commit _F_, `guix pull`
traverses all these commits and checks the authorization invariant.

Once a commit has been authenticated, all the commits in its [transitive
closure](https://en.wikipedia.org/wiki/Transitive_closure#In_graph_theory)
are known to be already authenticated.  `guix pull` keeps a local cache
of the commits it has previously authenticated, which allows it to
traverse only new commits.  For instance, if you’re at commit _F_ and
later update to a descendant of _F_, authentication starts at _F_.

The authorization invariant satisfies our needs to Guix.  It has one
downside: it prevents pull-request-style workflows.  Indeed, merging the
branch of a contributor not listed in `.guix-authorizations` would break
the authorization invariant.  It’s a good tradeoff for Guix because our
workflow relies on [patches carved into stone
tablets](https://lwn.net/Articles/702177/) ([patch
tracker](https://issues.guix.gnu.org/)), but it’s not suitable for every
project out there.

# Bootstrapping

The attentive reader may have noticed that something’s missing from the
explanation above: what do we do about commit _A_ in the example above?
Or, in other words, which commit do we pick as the first one where we
can start verifying the authentication invariant?

We solve this bootstrapping issue by defining _channel introductions_.
Previously, one would identify a channel simply by its URL.  Now, when
introducing a channel to users, one needs to provide an additional piece
of information: the first commit where the authorization invariant
holds, and the fingerprint of the OpenPGP key used to sign that commit
(it’s not strictly necessary but provides an additional check).

As always when it comes to establishing trust, distributing channel
introductions is very sensitive.  The introduction of the official
`guix` channel is [built into
Guix](https://git.savannah.gnu.org/cgit/guix.git/tree/guix/channels.scm#n155).
Users obtain it when they install Guix the first time; hopefully they
verify the signature on the Guix tarball or ISO image, [as noted in the
installation
instructions](https://guix.gnu.org/manual/en/html_node/Binary-Installation.html),
which reduces chances of getting the “wrong” Guix but is still very much
[trust-on-first-use](https://en.wikipedia.org/wiki/Trust_on_first_use)
(TOFU).

For signed third-party channels, users have to provide the channel’s
introduction in their `channels.scm` file, like so:

```scheme
(channel
  (name 'my-channel)
  (url "https://example.org/my-channel.git")
  (introduction
   (make-channel-introduction
    "6f0d8cc0d88abb59c324b2990bfee2876016bb86"
    (openpgp-fingerprint
     "CABB A931 C0FF EEC6 900D  0CFB 090B 1199 3D9A EBB5"))))
```

The `guix describe` command now prints the introduction if there’s one.
That way, it’s easy for a user to share their channel configuration,
including introductions, without having to be an expert

Channel introductions also solve another problem: forks.  Respecting the
authorization invariant “forever” would effectively prevent
“unauthorized” forks (forks made by someone who’s not a committer).
Someone willing to make a fork simply needs to emit a new introduction
for their fork, pointing to a different starting commit.

Last, channel introductions give a _point of reference_: if an attacker
manipulates branch heads on Savannah to have them point to unrelated
commits (such as commits on an orphan branch that do not share any
history with the “official” branches), authentication will necessarily
fail as it stumbles upon the first unauthorized commit made by the
attacker.

That’s all for authentication!  I’m glad you read this far.  At this
point you can take a break or continue with the next section on how
`guix pull` prevents downgrade attacks.

# Downgrade attacks

An important threat for software deployment tools is _downgrade_ or
_roll-back_ attacks.  The attack consists in tricking users into
installing older, known-vulnerable software packages, which in turn may
offer new ways to break into their system.  This is not strictly related
to the authentication issue we’ve been discussing, except that it’s
another important issue in this area that we took the opportunity to
address.

Guix saves provenance info for itself: `guix describe` prints that
information, essentially the Git commits of the channels used during
`git pull`:

```
$ guix describe
Generation 149  Jun 17 2020 20:00:14    (current)
  guix 8b1f7c0
    repository URL: https://git.savannah.gnu.org/git/guix.git
    branch: master
    commit: 8b1f7c03d239ca703b56f2a6e5f228c79bc1857e
```

Thus, `guix pull`, once it has retrieved the latest commit of the
selected branch, can verify that it is doing a _fast-forward update_ in
Git parlance—just like `git pull` does, but compared to the
previously-deployed Guix.  A fast-forward update is when the new commit
is a descendant of the current commit.  Going back to the figure above,
going from commit _A_ to commit _F_ is a fast-forward update, but going
from _F_ to _A_ or from _C_ to _E_ is not.

Not doing a fast-forward update would mean that the user is deploying an
older version of the Guix currently used, or deploying an unrelated
version from another branch.  In both cases, the user is at risk of
ending up installing older, vulnerable software.

By default `guix pull` now errors out on non-fast-forward updates,
thereby protecting from roll-backs.  Users [who know what they’re
doing](https://issues.guix.gnu.org/41882) can override that by passing
`--allow-downgrades`.

Authentication and roll-back prevention allow users to safely refer to
mirrors of the Git repository.  If `git.savannah.gnu.org` is down, one
can still update by fetching from a mirror, for instance with:

```
guix pull --url=https://github.com/guix-mirror/guix
```

If the repository at this URL is behind what the user already deployed,
or if it’s not a genuine mirror, `guix pull` will abort.  In other
cases, it will proceed.

Unfortunately, there is no way to answer the general question “_is_ X
_the latest commit of branch_ B_?_”.  Rollback detection prevents just
that, rollbacks, but there’s no mechanism in place to tell whether a
given mirror is stale.  To mitigate that, channel authors can specify,
in the repository, the channel’s _primary URL_.  This piece of
information lives in the `.guix-channel` file, in the repository, so
it’s authenticated.  `guix pull` uses it to print a warning when the
user pulls from a mirror:

```
$ guix pull --url=https://github.com/guix-mirror/guix
Updating channel 'guix' from Git repository at 'https://github.com/guix-mirror/guix'...
Authenticating channel 'guix', commits 9edb3f6 to 3e51f9e (44 new commits)...
guix pull: warning: pulled channel 'guix' from a mirror of https://git.savannah.gnu.org/git/guix.git, which might be stale
Building from this channel:
  guix      https://github.com/guix-mirror/guix 3e51f9e
…
```

So far we talked about mechanics in a rather abstract way.  That might
satisfy the graph theorist or the Git geek in you, but if you’re up for
a quick tour of the implementation, the next section is for you!

# A long process

  - start signing commits
  - add "make authenticate"
  - implement .guix-authorizations
  - generalize to channels
  - prevent downgrades
  - add primary URL
  - third-party channels

# SHA1
# Related work
# Future work

  - 'guix channel add'
