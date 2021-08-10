title: Grafts, continued
date: 2020-05-06 15:00
author: Ludovic Courtès
tags: Functional programming, Scheme API, Security, Performance
---

Guix includes a mechanism called _grafts_ that allows us to provide
users with [security
updates](https://guix.gnu.org/manual/en/html_node/Security-Updates.html)
in a timely fashion, even for core packages deep down in the dependency
graph.  Most users value the benefits of grafts, but newcomers were also
unavoidably struck by what turned out to be the undesirable side effect
of our graft implementation on user experience.  This had been a
well-known problem for a while, but [1.1.0 finally addressed these
issues](https://guix.gnu.org/blog/2020/gnu-guix-1.1.0-released/).

This article recaps how grafts are implemented, what problems that
caused, and how we solved it.  It’s a deep dive into core Guix, and I
hope it’ll be insightful to all and intriguing to the functional
programming geeks among us!

# What’s this “graft” thing anyway?

Grafts were introduced in the early days of Guix to [address probably
the main practical shortcomings of functional software
deployment](https://guix.gnu.org/blog/2016/timely-delivery-of-security-updates/).
In a nutshell, functional deployment as implemented by Nix and Guix
means that, when a package changes, everything that depends on it must
be rebuilt (or re-downloaded).  To deploy a security fix in the C
library or in Bash, you would thus need to rebuild everything.  Even
with a huge build farm, that can significantly delay the deployment of
fixes; users would find themselves either rebuilding things locally or,
at best, re-downloading binaries of everything.

To address this, Guix developers can instead specify a _replacement_ in
a [package
definition](https://guix.gnu.org/manual/en/html_node/Defining-Packages.html).
If we have a bug-fix for, say, libc, developers would (1) define a
package for the fixed libc, and (2) add a `replacement` field in the
original libc package pointing to that fixed package.  The effect is
that _only the bug-fix libc needs to be built_.  When building a
package, the bug-fix libc is automatically _grafted onto that package_,
such that the resulting package refers to the bug-fix libc.  [See the
manual](https://guix.gnu.org/manual/en/html_node/Security-Updates.html)
for more.

When “lowering” a high-level [package
definition](https://guix.gnu.org/manual/en/html_node/Defining-Packages.html)
to a low-level
[_derivation_](https://guix.gnu.org/manual/en/html_node/Derivations.html),
Guix traverses the package dependency graph and identifies a set of
potentially applicable grafts.  Why “potentially applicable”?  Consider
this scenario: assume `perl` has a `replacement`; `coreutils` has a
dependency on `perl`, but it’s a build-time dependency: `coreutils` does
not depend on `perl` at run time.  Thus, `coreutils` can be used as is,
there is no need to graft it.

But how do we know whether a dependency is a built-time-only dependency?
The [`native-inputs`
field](https://guix.gnu.org/manual/en/html_node/package-Reference.html#index-package)
of a package usually lists build-time dependencies, but it’s more of a
hint.  Ultimately, the set of run-time dependencies, which we call the
_references_, is the subset of the build-time dependencies that the
garbage collector (GC) in the build daemon finds _in the build
result_—Section 5.5.1 of [Eelco Dolstra’s PhD
thesis](http://nixos.org/~eelco/pubs/phd-thesis.pdf) describes how the
GC
scans for references.  In our example, we first have to actually build
`coreutils` before we can tell whether it depends on `perl` at
run time.

Guix arranges to graft only when necessary.  In this example, `guix
build coreutils` would return the same as `guix build coreutils
--no-grafts`.  Conversely, since `inkscape` has a run-time dependency on
`perl`, `guix build inkscape` returns a derivation that grafts the
`perl` replacement onto the original `inkscape` build result, the one
returned by `guix build inkscape --no-grafts`.  The (simplified)
dependency graph of the derivation for the grafted `inkscape` looks like
this:

![Dependency graph of the graft derivation of Inkscape.](https://guix.gnu.org/static/blog/img/inkscape-graft.svg)

Grafts are a form of what [_Build Systems à la
Carte_](https://www.microsoft.com/en-us/research/uploads/prod/2018/03/build-systems.pdf)
by Mokhov _et al._ (a good read!) refers to as _dynamic dependencies_:
grafting depends on intermediate _build results_.

Still here?  With the background in place, let’s look at the problems
that arose.

# Grafts, the user interface, and performance

Conceptually, to decide whether to graft a package, we examine the
references of the build result of the ungrafted package.  However, we
usually want `guix install` & co. to first display an overall build
plan, especially when invoked with `--dry-run`:

```
$ guix install inkscape
The following package will be installed:
   inkscape 1.0

71.3 MB will be downloaded:
   /gnu/store/xq64iaxx2gmlcgnipj31wjxlf1yd2g2p-gsl-2.6
   /gnu/store/zrmhnj3pwchn2msphgnwzwd3q89m46rn-aspell-0.60.8
   /gnu/store/5g31zf21lk8nrfd2b8qrm19nwdm9gir9-potrace-1.16
   /gnu/store/qpr7387bsjs7rpl6flrwdvn2zbzh5bch-ghostscript-with-cups-9.52
   /gnu/store/7y3lvk3xf4im8n44337mc6y0ccysvfia-font-dejavu-2.37
   /gnu/store/95n3zvzxzq2bxvdvz292cg04757ij30x-cups-minimal-2.3.1
…
```

To accommodate that, the pre-1.1.0 implementation of grafts did the
following: when
[substitutes](https://guix.gnu.org/manual/en/html_node/Substitutes.html)
were enabled, it would get the list of references of ungrafted packages
from substitutes; only when substitutes for an ungrafted package are
missing would it first try to build that package.  Thus, when
substitutes are available, `guix install` and similar commands would be
able to display the build plan upfront.  However, when a packages had no
substitutes, you’d see Guix start building it without saying a word
about the build plan, which was [arguably
confusing](https://issues.guix.gnu.org/issue/28310).

But it’s worse than that.  Grafting is per-package, so every time you
would lower a package to a derivation, you would need to answer the
question “does _this_ specific package have substitutes, and if so,
should it be grafted?”  The end result was [poor resource usage and
terrible user interface
feedback](https://issues.guix.gnu.org/issue/22990).  For every package
that is a graft candidate, the user would see that infamous line:

```
updating substitutes from 'https://ci.guix.gnu.org'...
```

The problem was particularly acute when building whole systems with
`guix system` because there would typically be a large number of such
packages.  Furthermore, each of these lines would correspond to
(roughly) a single HTTP GET request on a fresh TLS connection.  That can
be slow… and annoying.  Perhaps to some users this `updating
substitutes` stuttering was the proof of the developers’ incompetence
and perhaps, truth be told, to some of us developers it was a small
price to pay for the sophistication of grafts.

For users who disable substitutes and build everything locally, the
situation wasn’t much better: all the packages candidate for grafting
would be built one by one, thereby missing parallelization opportunities
as specified by
[`--max-jobs`](https://guix.gnu.org/manual/en/html_node/Invoking-guix_002ddaemon.html).

# Gathering dynamic dependencies

To address this, all these individual dynamic dependencies need to be
gathered somehow instead of being treated one by one.  Conceptually, we
would like to, roughly, do a first pass lowering packages to derivations
as if grafting was disabled, build all these derivations, and then do a
second pass to determine which packages in the graph need to be grafted and
to compute the relevant grafting derivation.  That would address the
performance issue: we’d now have as much parallelism as possible, so we
wouldn’t query substitutes or build packages one by one.  If we reify
that second pass to the user interface code, it also addresses the user
interface issue by allowing it to display, possibly, two build plans:
the “ungrafted” one followed by the grafted one.

The problem is that our API is inherently serial: the
`package-derivation` function takes _one_ package, lowers it, and
returns its derivation:

```scheme
(use-modules (guix)
             (gnu packages base)
             (gnu packages inkscape))

(define s (open-connection))

(package-derivation s coreutils)
⇒ #<derivation /gnu/store/rpfdbax1py483m9ydhvp73s7dgmn6xh4-coreutils-8.31.drv => /gnu/store/jkj7wxybgcpdamkl6fz7wwbb1ak5wxvk-coreutils-8.31-debug /gnu/store/zibwkb5xavnv6z3gzknfqjsxb9b0izh0-coreutils-8.31 7f6c92e3a000>

(package-derivation s coreutils #:graft? #f)
⇒ #<derivation /gnu/store/rpfdbax1py483m9ydhvp73s7dgmn6xh4-coreutils-8.31.drv => /gnu/store/jkj7wxybgcpdamkl6fz7wwbb1ak5wxvk-coreutils-8.31-debug /gnu/store/zibwkb5xavnv6z3gzknfqjsxb9b0izh0-coreutils-8.31 7f6c92e3a000>

(package-derivation s inkscape)
⇒ #<derivation /gnu/store/jzm2zsq8m0rj8wdsmikc0p2ik0cprrcf-inkscape-0.92.4.drv => /gnu/store/clj8rjnsip8a35hyd9nf4l65w7ahn0gs-inkscape-0.92.4 7f6c9c15b730>

(package-derivation s inkscape #:graft? #f)
⇒ #<derivation /gnu/store/psd31x1fq0v2g594z217mh56xzg21dym-inkscape-0.92.4.drv => /gnu/store/zz28ckjwfxwkx3gsm8sc452kmvfiky6y-inkscape-0.92.4 7f6c90ad4f50>
```

Lowering includes dealing with grafts, and
that’s why we ended up with one-by-one inefficiencies.  An option would
be to make all the API “plural”: have `package-derivation` and its
friends accept a _list_ of packages instead of a single one.  That would
be a huge amount of work and the end result would be unpleasant to use:
it’s easier to reason one-by-one.

The solution implemented in 1.1.0 instead starts from this observation:
the call graph of `package-derivation` mirrors the package graph.  Thus,
we could gather dynamic dependencies using [monad
trickery](https://guix.gnu.org/manual/en/html_node/The-Store-Monad.html)
or using “control effects”.  We went for the latter, which didn’t have
the “contamination” problem of monads and led to simpler code.

The starting point is that, by definition, code with dynamic
dependencies necessarily calls
[`build-derivations`](https://guix.gnu.org/manual/en/html_node/The-Store.html#index-build_002dderivations).
Taking advantage of [delimited continuations in
Guile](https://www.gnu.org/software/guile/manual/html_node/Prompts.html),
`build-derivations` is instrumented to [abort to a “build handler”
prompt](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=041b340da409078951267b6a8c43b27716e6b7ec)
when it’s called.  The build handler receives the list of derivations to
build along with a continuation to invoke to resume the aborted
computation and start building things.  User interface code can install
a build handler that displays what’s going to be built:

```scheme
(with-build-handler (lambda (continue store things mode)
                      (show-what-to-build store things)
                      (continue #t))
  …)
```

To implement dry runs, simply omit the call to `continue` and nothing
will be built.  (This is a slightly simplified artist view, see
[`build-notifier`](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=07ce23e011d18460e7ff5553d4ff640f7073075b)
for the real thing.)

Now, we need to take advantage of this mechanism to gather the
individual `build-derivations` calls so we can later emit a single
`build-derivations` call for all the gathered derivations.  The goal is
to effectively gather all the calls for ungrafted packages, build them
all at once, and then resume graft computation.

To achieve that, we write a build handler that, when invoked, returns an
`<unresolved>` object that captures what to build and the continuation.
In addition, we provide a primitive to _introduce parallelism_ such
that, if a dynamic dependency is encountered, we keep going and attempt
to compute as much as possible without resolving that dynamic
dependency.  These are [`build-accumulator` and
`map/accumulate-builds`](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=c40bf5816cb3ffb59920a61f71bd34b53cac3637).
`map/accumulate-builds` is like `map`, except that it accumulates and
gathers `build-derivations` request.

By using `map/accumulate-builds` instead of `map` in a few
[key](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=584dfdac3795541ff020aca3f488ceaf2ddd7fc3)
[places](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=25af35fa32bf6c991510406a330d4a42bd5beba8),
we obtain a good approximation of what we wanted, as illustrated in this
run:

```
$ guix install zile-on-guile vim-full
The following packages will be installed:
   zile-on-guile 2.4.14-0.fd09781
   vim-full      8.2.0411

9.4 MB will be downloaded:
   /gnu/store/vf7w4yiax38ra7x8aqqvbnc38c0ldgpm-zile-on-guile-2.4.14-0.fd09781
   /gnu/store/dnj9wljcck9vdwgp7dwxk00qnnk1g3c5-vim-full-8.2.0411
downloading from https://ci.guix.gnu.org/nar/lzip/dnj9wljcck9vdwgp7dwxk00qnnk1g3c5-vim-full-8.2.0411...
 vim-full-8.2.0411  8.9MiB                 7.6MiB/s 00:01 [##################] 100.0%

downloading from https://ci.guix.gnu.org/nar/lzip/vf7w4yiax38ra7x8aqqvbnc38c0ldgpm-zile-on-guile-2.4.14-0.fd09781...
 zile-on-guile-2.4.14-0.fd09781  140KiB    1.8MiB/s 00:00 [##################] 100.0%

The following derivation will be built:
   /gnu/store/d9xms78917w67xq71pqsx5x9s6dmq6d7-profile.drv
The following graft will be made:
   /gnu/store/4n6dmg6iwjg0adpcvqygr9wgsnclswss-vim-full-8.2.0411.drv
applying 8 grafts for /gnu/store/4n6dmg6iwjg0adpcvqygr9wgsnclswss-vim-full-8.2.0411.drv...
building /gnu/store/d9xms78917w67xq71pqsx5x9s6dmq6d7-profile.drv...
```

What we see above is first a build plan that downloads binaries for the
two ungrafted packages, followed by a build plan for one grafting
derivations: we have successfully preserved parallelism.

The solution resembles the `suspending` scheduler discussed in the _à
la Carte_ paper, though decomposition is not as principled as what the
paper describes.  It remains an approximation and not the
optimal way to deal with dynamic dependencies.  There are still
situations [where that shows](https://issues.guix.gnu.org/issue/40612),
but overall, it’s a significant improvement.  Unlike [other solutions
prototyped before](https://issues.guix.gnu.org/issue/22990#7), this one
has the advantage of being orthogonal and simple: less than [100 new
lines of
code](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=c40bf5816cb3ffb59920a61f71bd34b53cac3637),
and even [about 30 lines
removed](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=4b75a7060058bc2e959dcb4145067f6bba3e34e5)
from the graft implementation.  That alone contributes a lot to the
author’s satisfaction.  :-)

# Interlude: a scatter/gather pattern?

In the end, we’re just gathering all the `build-derivations` calls,
turning them into a single call, and finally calling all the original
site continuations with the result.  The same kind of issue shows up
when dealing with sequences of remote procedure calls (RPCs) and HTTP
requests, and it seems there’s a more general pattern lurking here.
Consider code like this:

```scheme
(map (lambda (thing)
       (http-get (thing->url thing)))
     lst)
```

Wouldn’t it be nice if we could somehow capture all the `http-get`
calls, turn them into a series of [pipelined GET
requests](https://en.wikipedia.org/wiki/HTTP_pipelining), and resume the
continuations with their result?

I haven’t found a standard functional pattern to address this and would
welcome ideas!

# Dynamic dependencies of all shapes

We have seen how Guix deals with dynamic dependencies.  Nix supports a
similar but limited form of dynamic dependencies through
the `import` primitive of the
Nix language, [which can take the result of a derivation
build](https://github.com/NixOS/nix/blob/master/src/libexpr/primops.cc#L74);
it does not attempt to gather the resulting `buildPaths` calls.

If we take a step back, we can see that Nix and Guix actually support
other forms of dynamic dependencies.  For example, it’s possible to
write derivations whose result is a function of the reference graph of
another derivation’s build result.  This is achieved in Guix by passing
the [`#:references-graphs` argument of
`gexp->derivation`](https://guix.gnu.org/manual/en/html_node/G_002dExpressions.html#index-gexp_002d_003ederivation),
which leads the build daemon to [include the reference graph in the
build
environment](https://git.savannah.gnu.org/cgit/guix.git/tree/nix/libstore/build.cc?id=298fb2907e3f432cea7dee9f58e89ab8d9dbd56f#n1763).

Another form of dynamic dependency is _derivation-building derivations_
or _recursive derivations_, which were [recently implemented in
Nix](https://github.com/NixOS/nix/pull/3205).  It supports another form
of dynamic dependency where the build process of a derivation can itself
create and build derivations (these are [_moldable
tasks_](https://en.wikipedia.org/wiki/Parallel_task_scheduling_problem)
in scheduling parlance).  It’s a great feature because in a nutshell, it
allows Nix to be used not only to compose packages, but also at a finer
grain as part of a package build process.

Guix supports yet another form of dynamic dependencies.  The newfangled
[`guix deploy`
tool](https://guix.gnu.org/manual/en/html_node/Invoking-guix-deploy.html)
works by [evaluating g-expressions (gexps)
remotely](https://guix.gnu.org/blog/2019/managing-servers-with-gnu-guix-a-tutorial/).
For example, before actually deploying an operating system, it first
runs code on the remote node to perform sanity checks: checking whether
the declared file system UUIDs or labels are valid, checking whether
additional kernel modules should be added to the initial RAM disk, and
so forth.  To do that,
[`remote-eval`](https://git.savannah.gnu.org/cgit/guix.git/tree/guix/remote.scm#n109)
first builds a derivation that produces a Scheme program, deploys it
along with all its dependencies on that target machine, runs it, and
retrieves the result.  This form of dynamic dependency also benefits
from the gathering machinery discussed above.

# Conclusion

This is a long article on what may look like a fine point of Guix design
and implementation, but there’s much to say about it!  Grafts are key to
the use of functional deployment in production because they enable quick
security updates, and it’s a lot better if they don’t harm the user
experience.

The pre-1.1.0 implementation of grafts had a negative impact on the user
interface and on performance, which was due to the sequential handling
of grafts, one package at a time.  In 1.1.0 we addressed it by using
delimited continuations to gather dynamic dependencies such as grafts,
perform builds in bulk, and resume each derivation computation.

As it turned out, the implementation of dynamic dependencies raises lots
of interesting design and implementation issues, and it’s probably not
the end of the road!

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
