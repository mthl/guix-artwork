title: Guix Profiles in Practice
date: 2019-10-25 12:15
author: Pierre Neidhardt
tags: Functional package management, Reproducibility, User interfaces, Customization, Software development, Cookbook
---

*Note: An updated version of this article is available in the brand new [cookbook](https://guix.gnu.org/cookbook/en/).*

Guix provides a very useful feature that may be quite foreign to newcomers:
*profiles*.  They are a way to group package installations together and all users
on a same system are free to use as many profiles as they want.

Whether you're a developer or not, you may find that multiple profiles bring you
great power and flexibility.  While they shift the paradigm somewhat compared to
*traditional package managers*, they are very convenient to use once you've
understood how to set them up.

If you are familiar with Python's `virtualenv`, you can think of a profile as a
kind of universal `virtualenv` that can hold any kind of software whatsoever, not
just Python software.  Furthermore, profiles are self-sufficient: they capture
all the runtime dependencies which guarantees that all programs within a profile
will always work at any point in time.

Multiple profiles have many benefits:

-   Clean semantic separation of the various packages a user needs for different contexts.

-   Multiple profiles can be made available into the environment either on login
    or within a dedicated shell.

-   Profiles can be loaded on demand.  For instance, the user can use multiple
    shells, each of them running different profiles.

-   Isolation: Programs from one profile will not use programs from the other, and
    they user can even install different versions of the same programs to the two
    profiles without conflict.

-   Deduplication: Profiles share dependencies that happens to be the exact same.
    This makes multiple profiles storage-efficient.

-   Reproducible: when used with declarative manifests, a profile can be fully
    specified by the Guix commit that was active when it was set up.  This means
    that the exact same profile can be [set up anywhere, anytime](https://guix.gnu.org/blog/2018/multi-dimensional-transactions-and-rollbacks-oh-my/), with just the
    commit information.  See the section on [5](#org98fddee).

-   Easier upgrades and maintenance: Multiple profiles make it easy to keep
    package listings at hand and make upgrades completely friction-less.

Concretely, here follows some typical profiles:

-   The dependencies of a project you are working on.

-   Your favourite programming language libraries.

-   Laptop-specific programs (like `powertop`) that you don't need on a desktop.

-   TeXlive (this one can be really useful when you need to install just one
    package for this one document you've just received over email).

-   Games.

Let's dive in the set up!


# Basic setup with manifests

A Guix profile can be set up *via* a so-called *manifest specification* that looks like
this:

```scheme
    (specifications->manifest
      '("package-1"
        ;; Version 1.3 of package-2.
        "package-2@1.3"
        ;; The "lib" output of package-3.
        "package-3:lib"
        ; ...
        "package-N"))
```

See [(guix) Invoking guix package](https://guix.gnu.org/manual/en/html_node/Invoking-guix-package.html) for the syntax details.

We can create a manifest specification per profile and install them this way:

```scheme
    GUIX_EXTRA_PROFILES=$HOME/.guix-extra-profiles
    mkdir -p "$GUIX_EXTRA_PROFILES"/my-project # if it does not exist yet
    guix package --manifest=/path/to/guix-my-project-manifest.scm --profile="$GUIX_EXTRA_PROFILES"/my-project/my-project
```

Here we set an arbitrary variable `GUIX_EXTRA_PROFILES` to point to the directory
where we will store our profiles in the rest of this article.

Placing all your profiles in a single directory, with each profile getting its
own sub-directory, is somewhat cleaner.  This way, each sub-directory will
contain all the symlinks for precisely one profile.  Besides, "looping over
profiles" becomes obvious from any programming language (e.g. a shell script) by
simply looping over the sub-directories of `$GUIX_EXTRA_PROFILES`.

Note that it's also possible to loop over the output of

```scheme
    guix package --list-profiles
```

although you'll probably have to filter out `~/.config/guix/current`.

To enable all profiles on login, add this to your `~/.bash_profile` (or similar):

```scheme
    for i in $GUIX_EXTRA_PROFILES/*; do
    	profile=$i/$(basename "$i")
    	if [ -f "$profile"/etc/profile ]; then
    		GUIX_PROFILE="$profile"
    		. "$GUIX_PROFILE"/etc/profile
    	fi
    	unset profile
    done
```

Note to Guix System users: the above reflects how your default profile
`~/.guix-profile` is activated from `/etc/profile`, that latter being loaded by
`~/.bashrc` by default.

You can obviously choose to only enable a subset of them:

```scheme
    for i in "$GUIX_EXTRA_PROFILES"/my-project-1 "$GUIX_EXTRA_PROFILES"/my-project-2; do
    	profile=$i/$(basename "$i")
    	if [ -f "$profile"/etc/profile ]; then
    		GUIX_PROFILE="$profile"
    		. "$GUIX_PROFILE"/etc/profile
    	fi
    	unset profile
    done
```

When a profile is off, it's straightforward to enable it for an individual shell
without "polluting" the rest of the user session:

```scheme
    GUIX_PROFILE="path/to/my-project" ; . "$GUIX_PROFILE"/etc/profile
```

The key to enabling a profile is to *source* its `etc/profile` file.  This file
contains shell code that exports the right environment variables necessary to
activate the software contained in the profile.  It is built automatically by
Guix and meant to be sourced.
It contains the same variables you would get if you ran:

```scheme
    guix package --search-paths=prefix --profile=$my_profile"
```

Once again, see [(guix) Invoking guix package](https://guix.gnu.org/manual/en/html_node/Invoking-guix-package.html) for the command line options.

To upgrade a profile, simply install the manifest again:

```scheme
    guix package -m /path/to/guix-my-project-manifest.scm -p "$GUIX_EXTRA_PROFILES"/my-project/my-project
```

To upgrade all profiles, it's easy enough to loop over them.  For instance,
assuming your manifest specifications are stored in
`~/.guix-manifests/guix-$profile-manifest.scm`, with `$profile` being the name
of the profile (e.g. "project1"), you could do the following in Bourne shell:

```scheme
    for profile in "$GUIX_EXTRA_PROFILES"/*; do
      guix package --profile="$profile" --manifest="$HOME/.guix-manifests/guix-$profile-manifest.scm"
    done
```

Each profile has its own generations:

```scheme
    guix package -p "$GUIX_EXTRA_PROFILES"/my-project/my-project --list-generations
```

You can roll-back to any generation of a given profile:

```scheme
    guix package -p "$GUIX_EXTRA_PROFILES"/my-project/my-project --switch-generations=17
```


# Required packages

Activating a profile essentially boils down to exporting a bunch of
environmental variables.  This is the role of the `etc/profile` within the
profile.

*Note: Only the environmental variables of the packages that consume them will
be set.*

For instance, `MANPATH` won't be set if there is no consumer application for man
pages within the profile.  So if you need to transparently access man pages once
the profile is loaded, you've got two options:

-   Either export the variable manually, e.g.

        export MANPATH=/path/to/profile${MANPATH:+:}$MANPATH"

-   Or include `man-db` to the profile manifest.

The same is true for `INFOPATH` (you can install `info-reader`),
`PKG_CONFIG_PATH` (install `pkg-config`), etc.


# Default profile

What about the default profile that Guix keeps in `~/.guix-profile`?

You can assign it the role you want.  Typically you would install the manifest
of the packages you want to use all the time.

Alternatively, you could keep it "manifest-less" for throw-away packages
that you would just use for a couple of days.
This way makes it convenient to run

```scheme
    guix install package-foo
    guix upgrade package-bar
```

without having to specify the path to a profile.


# The benefits of manifests

Manifests are a convenient way to keep your package lists around and, say,
to synchronize them across multiple machines using a version control system.

A common complaint about manifests is that they can be slow to install when they
contain large number of packages.  This is especially cumbersome when you just
want get an upgrade for one package within a big manifest.

This is one more reason to use multiple profiles, which happen to be just
perfect to break down manifests into multiple sets of semantically connected
packages.  Using multiple, small profiles provides more flexibility and
usability.

Manifests come with multiple benefits.  In particular, they ease maintenance:

-   When a profile is set up from a manifest, the manifest itself is
    self-sufficient to keep a "package listing" around and reinstall the profile
    later or on a different system.  For ad-hoc profiles, we would need to
    generate a manifest specification manually and maintain the package versions
    for the packages that don't use the default version.

-   `guix package --upgrade` always tries to update the packages that have
    propagated inputs, even if there is nothing to do.  Guix manifests remove this
    problem.

-   When partially upgrading a profile, conflicts may arise (due to diverging
    dependencies between the updated and the non-updated packages) and they can be
    annoying to resolve manually.  Manifests remove this problem altogether since
    all packages are always upgraded at once.

-   As mentioned above, manifests allow for reproducible profiles, while the
    imperative `guix install`, `guix upgrade`, etc. do not, since they produce
    different profiles every time even when they hold the same packages.  See
    [the related discussion on the matter](https://issues.guix.gnu.org/issue/33285).

-   Manifest specifications are usable by other `guix` commands.  For example, you
    can run `guix weather -m manifest.scm` to see how many substitutes are
    available, which can help you decide whether you want to try upgrading today
    or wait a while.  Another example: you can run `guix pack -m manifest.scm` to
    create a pack containing all the packages in the manifest (and their
    transitive references).

-   Finally, manifests have a Scheme representation, the `<manifest>` record type.
    They can be manipulated in Scheme and passed to the various Guix [APIs](https://en.wikipedia.org/wiki/Api).

It's important to understand that while manifests can be used to declare
profiles, they are not strictly equivalent: profiles have the side effect that
they "pin" packages in the store, which prevents them from being
[garbage-collected](https://guix.gnu.org/manual/en/html_node/Invoking-guix-gc.html) and ensures that they will still be available at any point in
the future.

Let's take an example:

1.  We have an environment for hacking on a project for which there isn't a Guix
    package yet.  We build the environment using a manifest, and then run `guix
       environment -m manifest.scm`.  So far so good.

2.  Many weeks pass and we have run a couple of `guix pull` in the mean time.
    Maybe a dependency from our manifest has been updated; or we may have run
    `guix gc` and some packages needed by our manifest have been
    garbage-collected.

3.  Eventually, we set to work on that project again, so we run `guix environment
       -m manifest.scm`.  But now we have to wait for Guix to build and install
    stuff!

Ideally, we could spare the rebuild time.  And indeed we can, all we need is to
install the manifest to a profile and use `GUIX_PROFILE=/the/profile;
. "$GUIX_PROFILE"/etc/profile` as explained above: this guarantees that our
hacking environment will be available at all times.

*Security warning:* While keeping old profiles around can be convenient, keep in
mind that outdated packages may not have received the latest security fixes.


<a id="org98fddee"></a>

# Reproducible profiles

To reproduce a profile bit-for-bit, we need two pieces of information:

-   a manifest,
-   a Guix channel specification.

Indeed, manifests alone might not be enough: different Guix versions (or
different channels) can produce different outputs for a given manifest.

You can output the Guix channel specification with `guix describe
--format=channels`.
Save this to a file, say `channel-specs.scm`.

On another computer, you can use the channel specification file and the manifest
to reproduce the exact same profile:

```scheme
    GUIX_EXTRA_PROFILES=$HOME/.guix-extra-profiles
    GUIX_EXTRA=$HOME/.guix-extra

    mkdir "$GUIX_EXTRA"/my-project
    guix pull --channels=channel-specs.scm --profile "$GUIX_EXTRA/my-project/guix"

    mkdir -p "$GUIX_EXTRA_PROFILES/my-project"
    "$GUIX_EXTRA"/my-project/guix/bin/guix package --manifest=/path/to/guix-my-project-manifest.scm --profile="$GUIX_EXTRA_PROFILES"/my-project/my-project
```

It's safe to delete the Guix channel profile you've just installed with the
channel specification, the project profile does not depend on it.


# Special thanks

Chris Marusich and Simon Tournier for their thorough feedback.


# About GNU Guix

[GNU Guix](https://www.gnu.org/software/guix) is a transactional package manager and an advanced distribution of the
GNU system that [respects user freedom](https://www.gnu.org/distros/free-system-distribution-guidelines.html). Guix can be used on top of any system
running the kernel Linux, or it can be used as a standalone operating system
distribution for i686, x86_64, ARMv7, and AArch64 machines.

In addition to standard package management features, Guix supports transactional
upgrades and roll-backs, unprivileged package management, per-user profiles, and
garbage collection. When used as a standalone GNU/Linux distribution, Guix
offers a declarative, stateless approach to operating system configuration
management. Guix is highly customizable and hackable through [Guile](https://www.gnu.org/software/guile) programming
interfaces and extensions to the [Scheme](http://schemers.org/) language.
