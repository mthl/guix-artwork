title: The Big Change
date: 2021-12-15 15:00
author: Ludovic Courtès
tags: Scheme API
---

Making cross-cutting changes over a large code base is difficult, but
it's occasionally necessary if we are to keep the code base tidy and
malleable.  With almost 800K source lines of code, Guix can reasonably
be called a large code base.  One might argue that almost 80% of this
code is [package
definitions](https://guix.gnu.org/manual/devel/en/html_node/Defining-Packages.html),
which “doesn't count”.  Reality is that it _does_ count, not only
because those package definitions _are_ regular Scheme code, but also
they are directly affected by the big change Guix has just undergone.

This post looks at what’s probably the biggest change Guix has seen
since it started nine years ago and that anyone writing packages will
immediately notice: simplified [package
inputs](https://guix.gnu.org/manual/en/html_node/package-Reference.html#index-package).
Yes, we just changed how each of the 20K packages plus those in
third-party channels can declare their dependencies.  Before describing
the change, how we implemented it, and how packagers can adapt, let’s
first take a look at the previous situation and earlier improvements
that made this big change possible.

# Packages and inputs

Packages in Guix are
[defined](https://guix.gnu.org/manual/devel/en/html_node/Defining-Packages.html)
using a _domain-specific language_ embedded in the Scheme programming
language—an EDSL, for the programming language geeks among us.  This is
a departure from other designs such as that of Nix, which comes with a
dedicated language, and it gives packagers and users access to a rich
programming interface while
retaining the purely declarative style of package definitions.

This package EDSL is one of the oldest bits of Guix and was described
[in a 2013 paper](https://arxiv.org/abs/1305.4584).  Although embedded
in Scheme, the package “language” was designed to be understandable by
people who’ve never practiced any kind of Lisp before—you could think of
it as a parenthesized syntax for JSON or XML.  It’s reasonable to say
that it’s been successful at that: of the 600+ people who contributed to
Guix over the years, most had never written Scheme or Lisp before.  The
example given in that paper remains a valid package definition:

```scheme
(define hello
  (package
    (name "hello")
    (version "2.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/hello/hello-"
                                  version ".tar.gz"))
              (sha256 (base32 "0wqd8..."))))
    (build-system gnu-build-system)
    (arguments
      '(#:configure-flags
        `("--disable-color"
          ,(string-append "--with-gawk="
                          (assoc-ref %build-inputs "gawk")))))
    (inputs `(("gawk" ,gawk)))
    (synopsis "GNU Hello")
    (description "An illustration of GNU's engineering practices.")
    (home-page "http://www.gnu.org/software/hello/")
    (license gpl3+)))
```

Of particular interest here is the `inputs` field, which lists
build-time dependencies.  Here there’s only one, GNU Awk; it has an
associated _label_, `"gawk"`.  The `,gawk` bit lets us insert the value
of the `gawk` variable, which is another package.  We can list more
dependencies like so:

```scheme
(inputs `(("gawk" ,gawk)
          ("gettext" ,gnu-gettext)
          ("pkg-config" ,pkg-config)))
```

Quite a bit of boilerplate.  Unless you’re into Lisp, this probably
looks weird to you—manageable, but weird.  What’s the deal with this
backquote and those commas?  The backquote is shorthand for `quasiquote`
and commas are shorthand for `unquote`; it’s a facility that Lisp and
Scheme provide [to construct
lists](https://www.gnu.org/software/guile/manual/html_node/Expression-Syntax.html).

Lispers couldn’t live without quasiquote, it’s wonderful.  Still,
exposing newcomers to this syntax has always been uncomfortable; in
tutorials you’d end up saying “yeah don’t worry, just write it this
way”.  Our goal though is to empower users by giving them abstractions
they can comprehend, hopefully providing a smooth path towards
[programming without
noticing](https://www.gnu.org/software/guile/manual/html_node/The-Emacs-Thesis.html).
This seemingly opaque backquote-string-unquote construct works against
that goal.

Then, you ask, why did Guix adopt this unpleasant syntax for inputs in
the first place?  Input syntax had to satisfy one requirement: that it’d
be possible for “build-side code” to refer to a specific input.  And
what’s _build-side code_?  It’s code that appears in the package
definition that is _staged_ for later execution—code that’s only
evaluated when the package is actually built.  The bit that follows
`#:configure-flags` in the example above is build-side code: it’s an
expression evaluated if and when the package gets built.  That
`#:configure-flags` expression refers to the gawk package to construct a
flag like `"--with-gawk=/gnu/store/…-gawk-5.0.1"`; it does so by
referring to the special `%build-inputs` variable, which happens to
contain an [association
list](https://www.gnu.org/software/guile/manual/html_node/Association-Lists.html)
that maps input labels to file names.  The `"gawk"` label in `inputs` is
here to allow build-side code to get at an
input’s file name.

Still here?  The paragraphs above are a demonstration of the shortcoming
of this approach.  That we have to explain so much suggests we’re
lacking an abstraction that would make the whole pattern clearer.

# G-expressions and self-referential records

The missing abstraction came to Guix a couple of years later:
[_G-expressions_](https://guix.gnu.org/manual/devel/en/html_node/G_002dExpressions.html).
Without going into the details, which were covered elsewhere, notably
[in a research article](https://hal.inria.fr/hal-01580582/en),
g-expressions, or “gexps”, are traditional [Lisp
s-expressions](https://en.wikipedia.org/wiki/S-Expression) (“sexps”) on
steroids.  A gexp can contain a package record or any other “file-like
object” and, when that gexp is serialized for eventual execution, the
package is replaced by its `/gnu/store/…` file name.

Gexps have been used since 2014–2015 in all of Guix System and they’ve
been great to work with, but package definitions were stuck with
old-style sexps.  One reason is that a big chunk of the code that deals
with packages and build systems had to be ported to the gexp-related
programming interfaces; a first attempt had been made long ago but
performance was not on par with that of the old code, so postponing
until that was addressed seemed wiser.  The second reason was that using
gexps in package definitions could be so convenient that packagers might
unwillingly find themselves creating inconsistent packages.

We can now rewrite our `hello` package such that configure flags are
expressed using a gexp:

```scheme
(define hello
  (package
    (name "hello")
    ;; …
    (arguments
     (list #:configure-flags
           #~`("--disable-color"
               ,(string-append "--with-gawk=" #$gawk))))
    (inputs `(("gawk" ,gawk)))))
```

The reference inserted here with `#$gawk` (`#$` is synonymous for
`ungexp`, the gexp equivalent of traditional `unquote`) refers to the
global `gawk` variable.  This is more concise and semantically clearer
than the `(assoc-ref %build-inputs "gawk")` snippet we had before.

Now suppose you define a package variant [using this common
idiom](https://guix.gnu.org/manual/devel/en/html_node/Defining-Package-Variants.html):

```scheme
(define hello-variant
  (package
    (inherit hello)
    (name "hello-variant")
    (inputs `(("gawk" ,gawk-4.0)))))
```

Here the intent is to create a package that depends on a different
version of GNU Awk—the hypothetical `gawk-4.0` instead of `gawk`.
However, the `#:configure-flags` gexp of this variant still refers to
the `gawk` variable, contrary to what the `inputs` field prescribes; in
effect, this variant depends on the _two_ Awk versions.

To address this discrepancy, we needed a new _linguistic device_, to put
it in a fancy way.  It [arrived in
2019](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=abd4d6b33dba4de228e90ad15a8efb456fcf7b6e)
in the form of _self-referential records_.  Within a field such as the
`arguments` field above, it’s now possible to refer to `this-package` to
get at the value of the package being defined.  (If you’ve done
object-oriented programming before, you might be thinking that we just
rediscovered the `this` or `self` pointer, and there’s some truth in
it. :-)) With a bit of syntactic sugar, we can rewrite the example above
so that it refers to the Awk package that appears in its own `inputs`
field:

```scheme
(define hello
  (package
    (name "hello")
    ;; …
    (arguments
     (list #:configure-flags
           #~(list (string-append "--with-gawk="
                                  #$(this-package-input "gawk")))))
    (inputs `(("gawk" ,gawk)))))
```

With this in place, we can take advantage of gexps in package
definitions while still supporting the common idiom to define package
variants, wheee!

That was a long digression from our input label theme but, as you can
see, all this interacts fairly tightly.

# Getting rid of input labels

Now that we have gexps and self-referential records, it looks like we
can finally get rid of input labels: they’re no longer strictly
necessary because we can insert packages in gexps instead of looking
them up by label in build-side code.  We “just” need to devise a
backward compatibility plan…

Input labels are pervasive; they’re visible in three contexts:

  1. in the `inputs` fields of package definitions;
  2. on the build side with the `inputs` keyword parameter of [build
     phases](https://guix.gnu.org/manual/devel/en/html_node/Build-Phases.html);
  3. in the Scheme programming interface since `package-inputs` and
     related functions are expected to return a list of labeled inputs.

We’re brave but not completely crazy, so we chose to focus on #1 for
now—it’s also the most visible of all three—, with an plan to
incrementally address #2, leaving #3 for later.

To allow for label-less inputs, we augmented the record interface with
[_field
sanitizers_](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=5291fd7a4205394b863a8705b32fbb447321dc60).
This feature allows us to define a procedure that inspects and
transforms the value specified for the `inputs`, `native-inputs`, and
`propagated-inputs`.  Currently that procedure [reintroduces input
labels](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=8524349f78c37439698f29d43049c2b21df6370f)
when they’re missing.  In a sense, we’re just changing the surface
syntax but under the hood everything is unchanged.  With this change,
our example above can now be written like this:

```scheme
(define hello
  (package
    (name "hello")
    ;; …
    (inputs (list gawk))))
```

Much nicer, no?  The effect is even more pleasant for packages with a
number of inputs:

```scheme
(define-public guile-sqlite3
  (package
    (name "guile-sqlite3")
    ;; …
    (build-system gnu-build-system)
    (native-inputs (list autoconf automake guile-3.0 pkg-config))
    (inputs (list guile-3.0 sqlite))))
```

That’s enough to spark joy to anyone who’s taught the previous syntax.
Currently this is transformed into something like:

```scheme
(define-public guile-sqlite3
  (package
    ;; …
    (native-inputs
      (map add-input-label
           (list autoconf automake guile-3.0 pkg-config)))))
```

… where the `add-input-label` function turns a package into a
label/package pair.  It does add a little bit of run-time overhead, but
nothing really measurable.

There are also cases where package definitions, in particular for
package variants, would directly manipulate input lists as returned by
`package-inputs` and related procedures.  It’s a case where packagers
had to be aware of input labels, and they would typically use
[association list (or “alist”) manipulation
procedures](https://www.gnu.org/software/guile/manual/html_node/Association-Lists.html)
and similar construct—this is context #3 above.  To replace those
idioms, we defined a higher-level construct that does not assume input
labels.  For example, a common idiom when defining a package variant
with additional dependencies goes like this:

```scheme
(define hello-with-additional-dependencies
  (package
    (inherit hello)
    (name "hello-with-bells-and-whistles")
    (inputs `(("guile" ,guile-3.0)
              ("libtextstyle" ,libtextstyle)
              ,@(package-inputs hello)))))
```

The variant defined above adds two inputs to those of `hello`.  We
introduced a macro,
[`modify-inputs`](https://guix.gnu.org/manual/devel/en/html_node/Defining-Package-Variants.html#index-modify_002dinputs),
which allows packagers to express that in a higher-level (and less
cryptic) fashion, in a way that does not refer to input labels.  Using
this other linguistic device (ha ha!), the snippet above becomes:

```scheme
(define hello-with-additional-dependencies
  (package
    (inherit hello)
    (name "hello-with-bells-and-whistles")
    (inputs (modify-inputs (package-inputs hello)
              (prepend guile-3.0 libtextstyle)))))
```

Similarly, `modify-inputs` advantageously subsumes `alist-delete` and
whatnot when willing to replace or remove inputs, like so:

```scheme
(modify-inputs (package-inputs hello)
  (replace "gawk" my-special-gawk)
  (delete "guile"))
```

On the build side—context #2 above—, we also provide new procedures that
allow packagers to avoid relying on input labels: [`search-input-file`
and
`search-input-directory`](https://guix.gnu.org/manual/devel/en/html_node/Build-Utilities.html#index-search_002dinput_002dfile).
Instead of having build phases that run code like:

```scheme
(lambda* (#:key inputs #:allow-other-keys)
  ;; Replace references to “/sbin/ip” by references to
  ;; the actual “ip” command in /gnu/store.
  (substitute* "client/scripts/linux"
    (("/sbin/ip")
     ;; Look up the input labeled “iproute”.
     (string-append (assoc-ref inputs "iproute")
                    "/sbin/ip"))))
```

… you’d now write:

```scheme
(lambda* (#:key inputs #:allow-other-keys)
  ;; Replace references to “/sbin/ip” by references to
  ;; the actual “ip” command in /gnu/store.
  (substitute* "client/scripts/linux"
    (("/sbin/ip")
     ;; Search “/sbin/ip” among all the inputs.
     (search-input-file inputs "/sbin/ip"))))
```

Nothing revolutionary but a couple of advantages: code is no longer tied
to input labels or package names, and `search-input-file` raises an
exception when the file is not found, which is better than building an
incorrect file name.

That was a deep dive into packaging!  If you’re already packaging
software for Guix, you hopefully see how to do things “the new way”.
Either way, it’s interesting to see the wide-ranging implications of
what initially looks like a simple change.  Things get complex when you
have to consider all the idioms that 20,000 packages make use of.

# Adapting to the new style

It’s nice to have a plan to change the style of package definitions, but
how do you make it happen concretely?  The last thing we want is, for
the next five years, to have to explain _two_ package styles to
newcomers instead of one.

First, [`guix
import`](https://guix.gnu.org/manual/devel/en/html_node/Invoking-guix-import.html)
now returns packages in the new style.  But what about existing package
definitions?

Fortunately, most of the changes above can be automated: that’s the job
of the [`guix
style`](https://guix.gnu.org/manual/devel/en/html_node/Invoking-guix-style.html)
command that we added for this purpose, but which may eventually be
extended to make package definitions prettier in all sorts of ways.
From a checkout, one can run:

```
./pre-inst-env guix style
```

Whenever it can be done safely, package inputs in _every_ package
definition are rewritten to the new style: removing input labels, and
using `modify-inputs` where appropriate.  If you maintain your own
channel, you can also run it for your packages:

```
guix style -L /path/to/channel my-package1 my-package2 …
```

We recommend waiting until the next Guix release is out though, which
could be a month from now, so that your channel remains usable by those
who pinned an older revision of the `guix` channel.

We ran `guix style` a couple of days ago on the whole repository,
leading to [the biggest
commit](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=8394619baceb118df92e355377fd543bb1aa501a)
in Guix history:

    460 files changed, 37699 insertions(+), 49782 deletions(-)

Woohoo!  Less than 15% of the packages (not counting Rust packages,
which are a bit special) have yet to be adjusted.  In most cases,
package inputs that were left unchanged are either those where we cannot
automatically determine whether the change would break the package, for
instance because build-side code expects certain labels, and those that
are “complex”—e.g., inputs that include conditionals.

The key challenge here was making sure `guix style` transformations are
correct; by default, we even want to be sure that changes introduced by
`guix style` do not trigger a rebuild—that package
[derivations](https://guix.gnu.org/manual/devel/en/html_node/Derivations.html)
are unchanged.

To achieve that, `guix style` _correlates_ the source code of each
package definition with the corresponding live package record.  That
allows it to answer questions such as “is this label the name of the
input package”.  That way, it can tell that, say:

```scheme
(inputs `(("libx11" ,libx11)
          ("libxcb" ,libxcb)))
```

can be rewritten without introducing a rebuild, because labels match
actual package names, whereas something like this cannot, due to label
mismatches:

```scheme
(inputs `(("libX11" ,libx11)
          ("xcb" ,libxcb)))
```

`guix style` can also determine situations where changes would trigger a
rebuild but would still be “safe”—without any observable effect.  You
can force it to make such changes by running:

```
guix style --input-simplification=safe
```

Because we’re all nitpicky when it comes to code formatting, `guix
style` had to produce nicely formatted code, and to make local changes
as opposed to rewriting complete package definitions.  Lisps are famous
for being [homoiconic](https://en.wikipedia.org/wiki/Homoiconicity),
which comes in handy in such a situation.

But the tools at our disposal are not capable enough for this
application.  First, Scheme’s standard
[`read`](https://www.gnu.org/software/guile/manual/html_node/Scheme-Read.html)
procedure, which reads an sexp (or an abstract syntax tree if you will)
from a byte stream and returns it, does not preserve comments.
Obviously we’d rather not throw away comments, so we came up with our
own `read` variant that preserves comments.  Similarly, we have a custom
pretty printer that can write comments, allowing it to achieve changes
like this:

```diff
@@ -2749,18 +2707,17 @@ (define-public debops
                         "debops-debops-defaults-fall-back-to-less.patch"))))
     (build-system python-build-system)
     (native-inputs
-     `(("git" ,git)))
+     (list git))
     (inputs
-     `(("ansible" ,ansible)
-       ("encfs" ,encfs)
-       ("fuse" ,fuse)
-       ("util-linux" ,util-linux)  ;for umount
-       ("findutils" ,findutils)
-       ("gnupg" ,gnupg)
-       ("which" ,which)))
+     (list ansible
+           encfs
+           fuse
+           util-linux ;for umount
+           findutils
+           gnupg
+           which))
     (propagated-inputs
-     `(("python-future" ,python-future)
-       ("python-distro" ,python-distro)))
+     (list python-future python-distro))
     (arguments
      `(#:tests? #f
```

The pretty printer also has special rules for input lists.  For
instance, lists of five inputs or less go into a single line, if
possible, whereas longer lists end up with one input per line, which is
often more convenient, especially when visualizing diffs.  It also has
rules to format `modify-inputs` in the same way we’d do it in Emacs:

```diff
@@ -171,9 +170,9 @@ (define-public arcan-sdl
     (inherit arcan)
     (name "arcan-sdl")
     (inputs
-     `(("sdl" ,sdl)
-       ,@(fold alist-delete (package-inputs arcan)
-               '("libdrm"))))
+     (modify-inputs (package-inputs arcan)
+       (delete "libdrm")
+       (prepend sdl)))
     (arguments
```

Overall that makes `guix style` a pretty fun meta-project!

# “Worse is better” or “the Right Thing”?

There are several lessons here.  One is that having an _embedded_
domain-specific language is what makes such changes possible: yes
package definitions have a declarative field, but we do not hide the
fact that their meaning is determined by the broader Guix framework,
starting with the `(guix packages)` module, which defines the package
record type and associated procedures.  Having a single repository
containing both package definitions _and_ “the package manager” is also
a key enabler; we can change the framework, add new linguistic tools,
_and_ adjust package definitions at the same time.  This is in contrast,
for instance, with the approach taken by Nix, where the language
implementation lives separately from the package collection.

Another one is that a strong, consistent community leads to consistent
changes—[not surprisingly in
fact](https://en.wikipedia.org/wiki/Conway%27s_law).  It’s a pleasure to
see that we, collectively, can undertake such overarching changes and
all look in the same direction.

The last lesson is in how we approach design issues in a project that is
now a little more than nine years old.  Over these nine years it’s clear
that we have usually favored [“the right
thing”](https://en.wikipedia.org/wiki/Worse_is_better#The_MIT_approach)
in our design—but not at any cost.  This whole change is an illustration
of this.  It was clear from the early days of Guix that input labels and
the associated machinery were suboptimal.  But it took us a few years to
design an implement the missing pieces: G-expressions, self-referential
records, and the various idioms that allow package definitions to
benefit from these.  In the meantime, we built a complete system _and a
community_ and we gained experience.  We cannot claim we attained _the_
right thing, if such a thing exists, but certainly package definitions
today are closer to the declarative ideal and easier to teach.

# It’s here today!

This big change, along with countless other improvements and package
upgrades, is just one `guix pull` away!  After months of development, we
have just merged the “core updates” branch bringing so many new
things—from GNOME 41, to GCC 10 by default, to hardened Python packages
and [improved executable startup
times](https://guix.gnu.org/en/blog/2021/taming-the-stat-storm-with-a-loader-cache/).
This paves the way for the upcoming release, most likely labeled “1.4”,
unless a closer review of the changes that have landed leads us to think
“2.0” would be more appropriate…  Stay tuned!

#### About GNU Guix

[GNU Guix](https://guix.gnu.org) is a transactional package manager and
an advanced distribution of the GNU system that [respects user
freedom](https://www.gnu.org/distros/free-system-distribution-guidelines.html).
Guix can be used on top of any system running the Hurd or the Linux
kernel, or it can be used as a standalone operating system distribution
for i686, x86_64, ARMv7, AArch64 and POWER9 machines.

In addition to standard package management features, Guix supports
transactional upgrades and roll-backs, unprivileged package management,
per-user profiles, and garbage collection.  When used as a standalone
GNU/Linux distribution, Guix offers a declarative, stateless approach to
operating system configuration management.  Guix is highly customizable
and hackable through [Guile](https://www.gnu.org/software/guile)
programming interfaces and extensions to the
[Scheme](http://schemers.org) language.
