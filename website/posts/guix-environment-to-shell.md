title: From ‘guix environment’ to ‘guix shell’
author: Ludovic Courtès
tags: Software development, User interfaces, Performance
date: 2021-10-26 15:00:00
---

There are times when what looked like the right design choice some years
back comes out as an odd choice as time passes.  The beloved [`guix
environment`](https://guix.gnu.org/manual/devel/en/html_node/Invoking-guix-environment.html)
tool is having that fate.  Its command-line interface has become
non-intuitive and annoying for the most common use cases.  Since it
could not be changed without breaking compatibility in fundamental ways,
we devised a new command meant to progressively replace it; [`guix
shell`](https://guix.gnu.org/manual/devel/en/html_node/Invoking-guix-shell.html)—that’s
the name we unimaginatively ended up with—has just landed after [a
three-week review period](https://issues.guix.gnu.org/50960), itself a
followup to discussions and hesitations on the best course of action.

This post introduces `guix shell`, how it differs from `guix
environment`, the choices we made, and why we hope you will like it.

# The story of `guix environment`

The `guix environment` command [started its life in
2014](https://lists.gnu.org/archive/html/guix-devel/2014-10/msg00111.html),
when Guix was a two-year old baby and the whole community could fit in a
small room.  It had one purpose: “to assist hackers in creating
reproducible development environments”.  It was meant to be similar in
spirit to [VirtualEnv](https://docs.python.org/3/tutorial/venv.html) or
[Bundler](https://bundler.io/), but *universal*—not limited to a single
language.  You would run:

```
guix environment inkscape
```

… and obtain an interactive shell with all the packages needed to hack
on Inkscape; in that shell, the relevant environment variables—`PATH`,
`CPATH`, `PKG_CONFIG_PATH`, and so on—would automatically point to a
profile created on the fly and containing the compiler, libraries, and
tools Inkscape depends on, but not Inkscape itself.

Only a year later did it become clear that there are cases where one
would want to create an environment containing specific packages, rather
than an environment containing *the dependencies* of packages.  To
address that, David Thompson [proposed the `--ad-hoc`
option](https://lists.gnu.org/archive/html/guix-devel/2015-05/msg00561.html):

```
guix environment --ad-hoc inkscape -- inkscape
```

… would create an environment containing only Inkscape, and would then
launch the `inkscape` command in that environment.  Many features were
added over the years, such as [the invaluable `--container`
option](https://guix.gnu.org/en/blog/2015/container-provisioning-with-guix/),
but these two modes, development and “ad hoc”, are the guts of it.

Fast forward six years: today, there’s consensus that the name
`--ad-hoc` is confusing for newcomers and above all, that the “ad hoc”
mode should be the default.  This is the main problem that [`guix
shell`](https://guix.gnu.org/manual/devel/en/html_node/Invoking-guix-environment.html)
addresses.

# Doing what you’d expect

Changing the default mode from “development environment” to “ad hoc” is
technically easy, but how to do that without breaking compatibility is
harder.  This led to [lengthy
discussions](https://issues.guix.gnu.org/38529), including proposals of
mechanisms to choose between the new and old semantics.

In the end, keeping the `guix environment` name while allowing it to
have different semantics was deemed dangerous.  For one thing, there’s
lots of material out there that demoes `guix environment`—blog posts,
magazine articles, on-line courses—and it would have been impossible to
determine whether they refer to the “new” or to the “old” semantics.  We
[reached the conclusion](https://issues.guix.gnu.org/38529#17) that it
would be easier to use a new command name and to eventually deprecate
`guix environment`.

With `guix shell`, the default is to create an environment that contains
the packages that appear on the command line; to launch Inkscape, run:

```
guix shell inkscape -- inkscape
```

The `--ad-hoc` option is gone!  Likewise, to spawn an ephemeral
development environment containing Python and a couple of libraries,
run:

```
guix shell python python-numpy python-scipy -- python3
```

Now, if you want, say, the development environment of Inkscape, add the
`--development` or `-D` option right before:

```
guix shell -D inkscape
```

You can add Git and GDB on top of it like so:

```
guix shell -D inkscape git gdb
```

(Note that `-D` only applies to the immediately following package,
`inkscape` in this case.)  It’s more concise and more natural than with
`guix environment`.  As can be seen [in the
manual](https://guix.gnu.org/manual/devel/en/html_node/Invoking-guix-shell.html),
all the other options supported by `guix environment` remain available
in `guix shell`.

# Short-hands for development environments

A convention that’s become quite common is for developers to provide a
`guix.scm` at the top of their project source tree, so that others can
start a development environment right away:

```
guix environment -l guix.scm
```

The `guix.scm` file would contain a [package
definition](https://guix.gnu.org/manual/devel/en/html_node/Defining-Packages.html)
for the project at hand, [as in this
example](https://notabug.org/cwebber/guile-gcrypt/src/master/guix.scm).
This option is known as `-f` in `guix shell`, for consistency with other
commands, and the equivalent command is:

```
guix shell -D -f guix.scm
```

Since all Guix commands accept a
[“manifest”](https://guix.gnu.org/manual/devel/en/html_node/Invoking-guix-package.html#index-manifest)
with `-m`, another option is to provide a `manifest.scm` file and to
run:

```
guix shell -m manifest.scm
```

“Wouldn’t it be nice if `guix shell` would automatically follow these
conventions when not given any argument?”, some
[suggested](https://lists.gnu.org/archive/html/guix-devel/2017-08/msg00300.html).
As in the case of Bundler, [direnv](https://direnv.net/), or typical
build tools from Meson to Make, having a default file name can save
typing and contribute to a good user experience for frequently-used
commands.  In this spirit, `guix shell` automatically loads `guix.scm`
or `manifest.scm`, from the current directory or an ancestor thereof,
such that entering a project to hack on it is as simple as:

```
cd ~/my/project/src
guix shell
```

Worry not: `guix shell` loads `guix.scm` or `manifest.scm` *if and only
if* you have first added its directory to
`~/.config/guix/shell-authorized-directories`.  Otherwise `guix shell`
warns you and prints a hint that you can copy/paste if you want to
authorize the directory.

# Caching environments

With that in place, `guix shell` can pretty much fill the same role as
direnv and similar tools, with one difference though: speed.  When all
the packages are already in store, `guix shell` can take one to a few
seconds to run, depending on the package set, on whether you’re using a
solid state device (SSD) or a “spinning” hard disk, and so on.  It’s
acceptable but prohibitively slow for direnv-like use cases.

To address that, `guix shell` maintains a profile cache for the `-D -f
guix.scm` and `-m manifest.scm` cases.  On a hot cache, it runs in
0.1 second.  All it has to do is fork a shell with the right environment
variable definitions; it does not talk to `guix-daemon`, and it does not
even read `guix.scm` or `manifest.scm` (it’s possible to forcefully
update the cache with `--rebuild-cache`).

That makes `guix shell` usable even for short-lived commands like
`make`:

```
guix shell -- make
```

Hopefully it’ll change the way we use the tool!

# The shell doctor

While revamping this command-line interface, the idea of a “shell
doctor” came up.  In interactive use, `guix shell` sets environment
variables and spawns a shell, but it’s not uncommon for the shell to
mess up with the whole environment.  Why?  Because, [contrary to
documented
practice](https://www.gnu.org/software/bash/manual/html_node/Bash-Startup-Files.html),
it’s quite common for users to define or override environment variables
in the startup files of non-login shells, `~/.bashrc` for Bash,
`~/.zshrc` for Zsh.  Instead, environment variable definitions should go
to the startup file of *login* shells—`~/.bash_profile`, `~/.profile`,
or similar.  But let’s face it: it’s a subtle distinction that few of us
know or care about.

As a result, users of Guix, especially on distros other than Guix
System, would often be disappointed when running `guix environment
--pure` and *yet* find that `PATH` contains non-Guix entries, that
there’s a bogus `LD_LIBRARY_PATH` definition, and whatnot.  Now, they
can call the doctor, so to speak, to obtain a diagnosis of the health of
their shell by adding the `--check` flag:

```
guix shell --check python python-numpy
```

The command creates an environment containing Python and NumPy, spawns
an interactive shell, checks the environment variables as seen by the
shell, and prints a warning if `PATH` or `PYTHONPATH` in this case have
been overridden.  It does not tell users where the problem comes from—it
cannot guess—but it tells them if something’s wrong, which is a first
step.

Of course, the best way to sidestep these problems is to pass
`--container`, which gives a fresh, isolated environment that does not
contain those startup files.  That’s not always an option though, for
instance on systems lacking support for unprivileged user namespaces, so
`--check` comes in handy there.

# Try it!

Just run `guix pull` to get this shiny new `guix shell` thingie!

If you don’t feel ready yet, that’s OK: `guix environment` won’t
disappear overnight.  We have a [written
commitment](https://guix.gnu.org/manual/devel/en/html_node/Invoking-guix-environment.html)
to keep it around until May, 1st 2023.  Though overall, we hope you’ll
find the `guix shell` interface easier to use and compelling enough that
you’ll be willing to switch overnight!

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
