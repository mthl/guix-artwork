title: Securing updates
author: Ludovic Courtès
date: 2020-07-01 17:40
tags: Security, Software development, Scheme API
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
[security
updates](https://guix.gnu.org/blog/tags/security/).

A couple of weeks ago, we addressed the elephant in the room:
authentication of Guix code itself by [`guix
pull`](https://guix.gnu.org/manual/en/html_node/Invoking-guix-pull.html),
the tool that updates Guix and its package collection.  This article
looks at what we set out to address, how we achieved it, and how it
compares to existing work in this area.

# What updates should be protected against

The problem of securing distro updates is often viewed through the lens
of binary distributions such as Debian, where the main asset to be
protected are binaries themselves.  The functional deployment model that
Guix and Nix implement is very different: conceptually, Guix is a
_source distribution_, like Gentoo if you will.

Pre-built binaries are of course available and very useful, but they’re
optional; we call them
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
[Savannah](https://git.savannah.gnu.org/cgit/guix.git/) by default.  If
Savannah is compromised ([as happened in
2010](https://www.fsf.org/blogs/sysadmin/savannah-and-www.gnu.org-downtime)),
an attacker can push code to the Guix repository, which everyone would
pull.  The change might even go unnoticed and remain in the repository
forever.  An attacker with access to Savannah can also reset the main
branch to an earlier revision, leading users to install outdated
software with known vulnerabilities—a _downgrade attack_.  These are
the kind of attacks we want to protect against.

# Authenticating Git checkouts

If we take a step back, the problem we’re trying to solve is not
specific to Guix and to software deployment tools: it’s about
_authenticating Git checkouts_.  By that, we mean that when `guix pull`
obtains code from Git, it should be able to tell that all the commits it
fetched were pushed by authorized developers of the project.  We’re
really looking at individual commits, not tags, because users can choose
to pull arbitrary points in the commit history of Guix and third-party
[channels](https://guix.gnu.org/manual/en/html_node/Channels.html).

Checkout authentication requires [cryptographically signed
commits](https://git-scm.com/book/en/v2/Git-Tools-Signing-Your-Work).
By signing a commit, a Guix developer asserts that they are the one who
made the commit; they may be its author, or they may be the person who
applied somebody else’s changes after review.  It also requires a notion
of authorization: we don’t simply want commits to have a valid
signature, we want them to be signed by an authorized key.  The set of
authorized keys changes over time as people join and leave the project.

To implement that, we came up with the following mechanism and rule:

  1. The repository contains a [`.guix-authorizations`
     file](https://git.savannah.gnu.org/cgit/guix.git/tree/.guix-authorizations)
     that lists the OpenPGP key fingerprints of authorized committers.
  2. A commit is considered authentic if and only if it is signed by one
     of the keys listed in the `.guix-authorizations` file of each of
     its parents.  This is the _authorization invariant_.

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

Since `.guix-authorizations` is a regular file under version control,
granting or revoking commit authorization does not require special
support.  In the example above, commit _B_ is an authorized commit by
Alice that adds Bob’s key to `.guix-authorizations`.  Revocation is
similar: any authorized committer can remove entries from
`.guix-authorizations`.  Key rotation can be handled similarly: a
committer can remove their former key and add their new key in a single
commit, signed by the former key.

The authorization invariant satisfies our needs for Guix.  It has one
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
In other words, which commit do we pick as the first one where we
can start verifying the authorization invariant?

We solve this bootstrapping issue by defining _channel introductions_.
Previously, one would identify a channel simply by its URL.  Now, when
introducing a channel to users, one needs to provide an additional piece
of information: the first commit where the authorization invariant
holds, and the fingerprint of the OpenPGP key used to sign that commit
(it’s not strictly necessary but provides an additional check).
Consider this commit graph:

![Example commit graph with introduction.](https://guix.gnu.org/static/blog/img/commit-graph-intro.svg)

On this figure, _B_ is the introduction commit.  Its ancestors, such as
_A_ are considered authentic.  To authenticate, _C_, _D_, _E_, and _F_,
we check the authorization invariant.

As always when it comes to establishing trust, distributing channel
introductions is very sensitive.  The introduction of the official
`guix` channel is [built into
Guix](https://git.savannah.gnu.org/cgit/guix.git/tree/guix/channels.scm?id=d2fde340adf197cc42bc4e0187deaf3a7bd3968d#n155).
Users obtain it when they install Guix the first time; hopefully they
verify the signature on the Guix tarball or ISO image, [as noted in the
installation
instructions](https://guix.gnu.org/manual/en/html_node/Binary-Installation.html),
which reduces chances of getting the “wrong” Guix, but it is still very much
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
That way, one can share their channel configuration, including
introductions, without having to be an expert.

Channel introductions also solve another problem: forks.  Respecting the
authorization invariant “forever” would effectively prevent
“unauthorized” forks—forks made by someone who’s not in `.guix-authorizations`.
Someone publishing a fork simply needs to emit a new introduction
for their fork, pointing to a different starting commit.

Last, channel introductions give a _point of reference_: if an attacker
manipulates branch heads on Savannah to have them point to unrelated
commits (such as commits on an orphan branch that do not share any
history with the “official” branches), authentication will necessarily
fail as it stumbles upon the first unauthorized commit made by the
attacker.  In the figure above, the red branch with commits _G_ and _H_
cannot be authenticated because it starts from _A_, which lacks
`.guix-authorizations` and thus fails the authorization invariant.

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
from _F_ to _A_ or from _D_ to _E_ is not.

Not doing a fast-forward update would mean that the user is deploying an
older version of the Guix currently used, or deploying an unrelated
version from another branch.  In both cases, the user is at risk of
ending up installing older, vulnerable software.

By default `guix pull` now errors out on non-fast-forward updates,
thereby protecting from roll-backs.  Users [who understand the
risks](https://issues.guix.gnu.org/41882) can override that by passing
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
_the latest commit of branch_ B _?_”.  Rollback detection prevents just
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

We’re kinda celebrating these days, but the [initial bug
report](https://issues.guix.gnu.org/22883) was opened… in 2016.  One of
the reasons was that we were hoping the general problem was solved
already and we’d “just” have to adapt what others had done.  As for the
actual design: you would think it can be implemented in ten lines of
shell script invoking `gpgv` and `git`.  Perhaps that’s a possibility,
but the resulting performance would be problematic—keep in mind that
users may routinely have to authenticate hundreds of commits.  So we
took a long road, but the end result is worth it.  Let’s recap.

Back in April 2016, committers [started signing
commits](https://issues.guix.gnu.org/22883#4), with a [server-side hook
prohibiting unsigned commits](https://issues.guix.gnu.org/22883#36).  In
July 2016, we had [proof-of-concept libgit2
bindings](https://issues.guix.gnu.org/22883#33) with the primitives
needed to verify signatures on commits, passing them to `gpgv`; later
[Guile-Git](https://gitlab.com/guile-git/guile-git/) was born, providing
good coverage of the libgit2 interface.  Then there was a two-year
hiatus during which no code was produced in that area.

Everything went faster starting from December 2019.  Progress was
incremental and may have been hard to follow, even for die-hard Guix
hackers, so here are the major milestones:

  - In December 2019, a first [authentication program for use by Guix
    developers](https://issues.guix.gnu.org/22883#48) landed; it could
    be run from a checkout with the `make authenticate` command.  It
    would use Guile-Git but call out to `gpgv` for signature
    verification, which made it rather slow.  The list of authorized
    keys was hard-coded.
  - In April 2020, we had [an implementation of OpenPGP for signature
    verification purposes only](https://issues.guix.gnu.org/22883#61)
    available as [`(guix
    openpgp)`](https://git.savannah.gnu.org/cgit/guix.git/tree/guix/openpgp.scm).
    The code is based on Göran Weinholt’s pure-Scheme
    [Industria](https://github.com/weinholt/industria/) library, with
    the addition of a few features and using
    [Guile-Gcrypt](https://notabug.org/cwebber/guile-gcrypt) for faster
    crypto.  That led to a tenfold speedup compared to invoking `gpgv`,
    which is primarily due to the fact that our code [foregoes OpenPGP
    bells and whistles](https://issues.guix.gnu.org/22883#62) and
    focuses on “just” signature verification.  Notably, it ignores key
    expiration and revocation.
  - In May, [`.guix-authorizations` support was
    added](https://issues.guix.gnu.org/22883#64), superseding the
    hard-coded list of authorized keys.  The OpenPGP keyring could now
    be loaded straight from a [Git branch containing all the OpenPGP
    keys ever
    used](https://git.savannah.gnu.org/cgit/guix.git/tree/?h=keyring).
  - Early June, the authentication code was [moved to its own
    module](https://issues.guix.gnu.org/41653), `(guix
    git-authenticate)`, with a test suite.
  - Soon after, Git authentication was [integrated in
    channels](https://issues.guix.gnu.org/41767): `guix pull` would now
    authenticate the `guix` channel, [closing the 4-year old
    mega-issue](https://issues.guix.gnu.org/22883).
  - Just today, we added the final bits, [allowing channel authors to
    benefit from the feature](https://issues.guix.gnu.org/42048).

Whether you’re a channel author or a user, the feature is now [fully
documented in the
manual](https://guix.gnu.org/manual/devel/en/html_node/Channels.html),
and we’d love to get your feedback!

# SHA-1

We can’t really discuss Git commit signing without mentioning
[SHA-1](https://en.wikipedia.org/wiki/SHA-1).  The venerable
crytographic hash function is approaching end of life, as evidenced by
[recent](https://shattered.io/)
[breakthroughs](https://sha-mbles.github.io/).  Signing a Git commit
boils down to signing a SHA-1 hash, because all objects in
the Git store are identified by their SHA-1 hash.

Git now relies on a [collision attack detection
library](https://www.usenix.org/system/files/conference/usenixsecurity17/sec17-stevens.pdf)
to mitigate practical attacks.  Furthermore, the Git
project is planning a [hash function
transition](https://git-scm.com/docs/hash-function-transition/) to
address the problem.

Some projects such as Bitcoin Core choose to not rely on SHA-1 at all.
Instead, for the commits they sign, they include in the commit log the
SHA512 hash of the tree, which the [verification scripts
check](https://github.com/bitcoin/bitcoin/tree/master/contrib/verify-commits).

Computing a tree hash _for each commit_ in Guix would probably be
prohibitively costly.  For now, for lack of a better solution, we rely
on Git’s collision attack detection and look forward to a hash function
transition.

As for SHA-1 in an OpenPGP context: our authentication code [rejects
SHA-1 OpenPGP signatures](https://issues.guix.gnu.org/41787), as
recommended.

# Related work

A lot of work has gone into securing the software supply chain, often in
the context of binary distros, sometimes in a more general context; more
recent work also looks into Git authentication and related issues.
This section attempts to summarize how Guix relates to similar work that
we’re aware of in these two areas.  More detailed discussions can be
found in the [issue tracker](https://issues.guix.gnu.org/22883).

[The Update Framework](https://theupdateframework.io/) (TUF) is a
reference for secure update systems, with [a well-structured
spec](https://github.com/theupdateframework/specification/blob/master/tuf-spec.md#the-update-framework-specification)
and a number of
[implementations](https://github.com/theupdateframework/specification/blob/master/tuf-spec.md#the-update-framework-specification).
TUF is a great source of inspiration to think about this problem space.
Many of its goals are shared by Guix.  Not all the attacks it aims to
protect against (Section 1.5.2 of the spec) are addressed by what’s
presented in this post: _indefinite freeze attacks_,
where updates never become available, are not addressed _per se_ (though
easily observable), and _slow retrieval attacks_ aren’t addressed
either.  The notion of _role_ is also something currently missing from
the Guix authentication model, where any authorized committer can touch
any files, though the model and `.guix-authorizations` format leave room
for such an extension.

However, both in its goals and system descriptions, TUF is biased
towards systems that distribute binaries as plain files with associated
meta-data.  That creates a fundamental impedance mismatch.  As an
example, attacks such as _fast-forward attacks_ or _mix-and-match
attacks_ don’t apply in the context of Guix; likewise, the _repository_
depicted in Section 3 of the spec has little in common with a Git
repository.

Developers of OPAM, the OCaml package manager, [adapted TUF for use with
their Git-based package
repository](http://opam.ocaml.org/blog/Signing-the-opam-repository/),
later updated to write [Conex](https://github.com/hannesm/conex), a
separate tool to authenticate OPAM repositories.  OPAM is interesting
because like Guix it’s a source distro and its [package
repository](https://github.com/ocaml/opam-repository) is a Git
repository containing “build recipe”.  To date, it appears that `opam
update` itself does not authenticate repositories though; it’s up to
users or developer to run Conex.

Another very insightful piece of work is the 2016 paper [_On omitting
commits and committing
omissions_](https://www.usenix.org/system/files/conference/usenixsecurity16/sec16_paper_torres-arias.pdf).
The paper focuses on the impact of malicious modifications to Git
repository meta-data.  An attacker with access to the repository can
modify, for instance, branch references, to cause a rollback attack or a
“teleport” attack, causing users to pull an older commit or an unrelated
commit.  As written above, `guix pull` would detect such attacks.
However, `guix pull` would fail to detect cases where metadata
modification does not yield a rollback or teleport, yet gives users a
different view than the intended one—for instance, a user is directed to
an authentic but different branch rather than the intended one.  The
“secure push” operation and the associated _reference state log_ (RSL)
the authors propose would be an improvement.

# Wrap-up and outlook

Guix now has a mechanism that allows it to authenticate updates.  If
you’ve run `guix pull` recently, perhaps you’ve noticed additional
output and a progress bar as new commits are being authenticated.  Apart
from that, the switch has been completely transparent.  The
authentication mechanism is built around the commit graph of Git; in
fact, it’s a mechanism to _authenticate Git checkouts_ and in that sense
it is not tied to Guix and its application domain.  It is available not
only for the main `guix` channel, but also for third-party channels.

To bootstrap trust, we added the notion of _channel introductions_.
These are now visible in the user interface, in particular in the output
of `guix describe` and in the configuration file of `guix pull` and
`guix time-machine`.  While channel configuration remains a few lines of
code that users typically paste, this extra bit of configuration might
be intimidating.  It certainly gives an incentive to provide a
command-line interface to manage the user’s list of channels: `guix
channel add`, etc.

The solution here is built around the assumption that Guix is
fundamentally a source-based distribution, and is thus completely
orthogonal to the [public key infrastructure (PKI) Guix uses for the
signature of
substitutes](https://guix.gnu.org/manual/en/html_node/Substitute-Server-Authorization.html).
Yet, the substitute PKI could probably benefit from the fact that we now
have a secure update mechanism for the Guix source code: since `guix
pull` can securely retrieve a new substitute signing key, perhaps it
could somehow handle substitute signing key revocation and delegation
automatically?  Related to that, channels could perhaps advertise a
substitute URL and its signing key, possibly allowing users to register
those when they first pull from the channel.  All this requires more
thought, but it looks like there are new opportunities here.

Until then, if you’re a user or a channel author, we’d love to hear from
you!  We’ve already gotten feedback that these new mechanisms [broke
someone’s workflow](https://issues.guix.gnu.org/41882); hopefully it
didn’t break yours, but either way your input is important in improving
the system.  If you’re into security and think this design is terrible
or awesome, please do provide feedback.

It’s a long and article describing a long ride on a path we discovered
as we went, and it felt like an important milestone to share!

# Acknowledgments

Thanks to everyone who provided feedback, ideas, or carried out code
review during this long process, notably (in no particular order):
Christopher Lemmer Webber, Leo Famulari, David Thompson, Mike Gerwitz,
Ricardo Wurmus, Werner Koch, Justus Winter, Vagrant Cascadian, Maxim
Cournoyer, Simon Tournier, John Soo, and Jakub Kądziołka.  Thanks also
to janneke, Ricardo, Marius, and Simon for reviewing an earlier draft of
this post.

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
