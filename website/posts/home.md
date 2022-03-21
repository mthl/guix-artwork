title: Keeping one’s home tidy
date: 2022-03-21 15:30:00
author: Ludovic Courtès
tags: Home, Scheme API, Programming interfaces
---

How much effort to recreate your work environment when you switch to a
new machine?  What would it take to roll back to your previous
environment once you’ve noticed a program no longer behaves as expected?
What about sharing your environment with friends of yours?  These are
some of the things that [Guix
Home](https://guix.gnu.org/manual/devel/en/html_node/Home-Configuration.html),
which landed in Guix as a “technology preview” in September 2021, aims
to make effortless, reliable, and fun.

In a nutshell, Guix Home brings the [fully declarative configuration of
Guix
System](https://guix.gnu.org/manual/devel/en/html_node/Using-the-Configuration-System.html)
to home directories.  With Guix System, users and administrators provide
a configuration file that defines the operating system configuration;
with Guix Home, users provide a configuration file that defines the
configuration of their work environment in their home directory—their
_home environment_.  That configuration is meant to *stand alone*, to
describe all the relevant aspects of your work environment.  But what
exactly goes in a home environment?

# “Dot files” don’t live in a vacuum

Among seasoned Unix-style users, we often equate “home environment” with
“dot files”—configuration files in our home directory, from `~/.bashrc`
and `~/.ssh/config` to `~/.emacs` and everything under `~/.config`.
These files are precious and many store them under version control, to
keep track of changes made to their configuration.  That’s a good idea,
but is that all it takes to describe the home environment?  To roll
back to a previous version?

Of course not.  Dot files don’t exist in a vacuum; at the very least,
your home environment is not just a set of dot files, but also a set of
installed packages.  They work together: if you upgrade a package, the
corresponding dot file might need to be adjusted; if a package is
missing, its dot file is not of any use.  Sometimes a home environment
contains additional things: daemons (programs that run in the
background), or periodically executed jobs.

Guix Home goes beyond dot files: it lets you declare and
instantiate all these aspects that make up your home environment.

# Genesis

Guix Home was initially developed by Andrew Tropin as part of the [rde
project](https://git.sr.ht/~abcdw/rde); it was integrated in Guix proper
six months ago.  I am writing this as an adopter and contributor, but
there were a number of earlier adopters and earlier contributors.  In
fact, despite being still very much under development, the tool has
already attracted a number of excited users eager to find a way to keep
their home tidy!

The idea of writing down a declaration of your home environment that you
can reproduce anytime is a natural followup to everything Guix does—you
could already declare a package set in a
[manifest](https://guix.gnu.org/manual/devel/en/html_node/Invoking-guix-package.html#index-profile-manifest)
or even a [complete operating
system](https://guix.gnu.org/manual/devel/en/html_node/Using-the-Configuration-System.html).
It had been floating around, in Nix land with [Home
Manager](https://github.com/nix-community/home-manager) and in Guix land
with the now-defunct [Guix Home
Manager](https://framagit.org/tyreunom/guix-home-manager/) by Julien
Lepiller.  The latter was similar to today’s Guix Home, but went one
step further by making your home directory read-only—yes, *read-only*!
The main advantage is that it would ensure statelessness—you’d be sure
that absolutely *all* your home configuration is under Guix Home
Manager’s control; sub-directories containing mutable data would have to
be explicitly declared.  The downside is that it raised the barrier to
entry: you’d have to either switch entirely, or not use it at all.  Guix
Home takes a more pragmatic approach and happily coexists with
configuration managed “the old way”.

# Getting started

To get started, you need a Home configuration file.  There’s
[documentation](https://guix.gnu.org/manual/devel/en/html_node/Declaring-the-Home-Environment.html),
but as always, starting from a blank page is a bit intimidating.  So
instead of starting from a blank page, you can let `guix home import`
generate an initial config for you:

```
guix home import ~/src/guix-config
```

This will create the `~/src/guix-config` directory and populate it with a
bunch of files among which `home-configuration.scm` along these lines:

```scheme
(use-modules (gnu home)
             (gnu packages)
             (gnu services)
             (guix gexp)
             (gnu home services shells))

(home-environment
 (packages
  (map (compose list specification->package+output)
       (list "emacs-geiser-guile"
             "emacs-geiser"
             "pinentry-emacs"
             "emacs-exwm"
             "gnome-maps"
             "pipe-viewer"
             "emacs"
             "pavucontrol"
             "git"
             "xterm"
             "qemu"
             "openssh")))
 (services
  (list (service home-bash-service-type
                 (home-bash-configuration
                  (aliases
                   '(("grep" . "grep --color=auto")
                     ("ll" . "ls -l")
                     ("ls" . "ls -p --color=auto")
                     ("qemu" . "qemu-system-x86_64 -enable-kvm -m 512")
                     ("rm" . "rm --one-file-system")))
                  (bashrc
                   (list (local-file "/home/charlie/src/guix-config/.bashrc" 
				                     "bashrc")))
                  (bash-profile
                   (list (local-file
                          "/home/charlie/src/guix-config/.bash_profile"
                          "bash_profile"))))))))
```

`guix home import` automatically added the packages of `~/.guix-profile`
to the `packages` field.  Because I’m using
[Bash](https://www.gnu.org/software/bash), it also added an instance of
[`home-bash-service-type`](https://guix.gnu.org/manual/devel/en/html_node/Shells-Home-Services.html)
with aliases extracted from my `~/.bashrc`; it also made copies of
`~/.bashrc` and `~/.bash_profile` and refers to them.

Now that I have an initial configuration, I can first test it in an
_isolated container_:

```
guix home container ~/src/guix-config/home-configuration.scm
```

This command gives an interactive shell in a container where my home
environment, as declared in `home-configuration.scm`, is deployed.
There I can see my home directory as it would look like if I deploy my
home environment “for real”: I can see my `~/.bashrc` and co., I can
check that all the packages declared are in `$PATH` and visible in
`~/.guix-home`, and so on.  And all this is safe: my actual home
directory has been left unchanged!

Once satisfied with my configuration, I can instantiate it:

```
guix home reconfigure ~/src/guix-config/home-configuration.scm
```

At that point, my actual home directory corresponds to that
configuration.  Some of my dot files are now provided by Guix Home, and
thus they’re symbolic links (“symlinks”) to their read-only copy in
`/gnu/store`:

```
$ ls -l ~/.bashrc ~/.bash_profile
lrwxrwxrwx 1 charlie users 56 Mar  7 15:46 /home/charlie/.bash_profile -> /gnu/store/lpdydssyyxx9n0xvp2jmv7yqgyr2pcg3-bash_profile
lrwxrwxrwx 1 charlie users 50 Mar  7 15:46 /home/charlie/.bashrc -> /gnu/store/kxc0j4i05sib04vf92nr8xxkb8isdfn7-bashrc
```

But don’t worry: before creating those symlinks, `guix home reconfigure`
created backups of existing files under
`~/TIMESTAMP-guix-home-legacy-configs-backup`, where `TIMESTAMP` is a
Unix-style timestamp.

And voilà, I have my first Guix Home generation!

```
$ guix home describe
Generation 1    Mar 07 2022 15:46:20   (current)
  file name: /var/guix/profiles/per-user/charlie/guix-home-1-link
  canonical file name: /gnu/store/qr1c5jpfrj815ncv6yr2lfdgs8nq8kkn-home
  channels:
    guix:
      repository URL: https://git.savannah.gnu.org/git/guix.git
      branch: master
      commit: 3ac1366648f933f7244c2d0b9926f7ba5d92a113
  configuration file: /gnu/store/xfgasfms9rhhigyj7i8za77zpqx6zbhn-configuration.scm
```

`guix home describe` shows provenance tracking we know and love from
Guix System: all the info we need to redeploy the same home environment
elsewhere, or at a different point in time.  It’s also information `guix
home reconfigure` relies on to make sure you never accidentally
_downgrade_ you home environment to an older Guix revision.

# Going further

Alright, at this point, you might be thinking that it’s a lot of fuss
but the “only” benefit over dot files under version control is that
`guix home` also takes care of installing packages.  Guix Home really
shines once you use higher-level services, and when you start composing
services together.

To the example above, in the `services` field, we can add a service
declaration that runs
[Redshift](https://guix.gnu.org/manual/devel/en/html_node/Desktop-Home-Services.html#index-home_002dredshift_002dservice_002dtype),
a program that adjusts the display color temperature according to the
time of day:

```scheme
(service home-redshift-service-type
		 (home-redshift-configuration
		  (location-provider 'manual)
		  (latitude 35.81)    ;northern hemisphere
		  (longitude -0.80))) ;west of Greenwich
```

The effect is that, as soon as we log in, under Xorg, Redshift will be
started in the background as a [Shepherd
service](https://guix.gnu.org/manual/devel/en/html_node/Shepherd-Home-Service.html).
The Home-generated `~/.profile` takes care of spawning `shepherd`, which
in turn spawns the `redshift` service:

```
$ herd status
Started:
 + root
 + redshift
```

We gained another thing here: a consistent, unified configuration
language.  Instead of learning Redshift’s configuration file format, we
define a `home-redshift-configuration` record, right in Scheme.  Under
the hood, that configuration is converted into Redshift’s file format;
any error is caught at configuration time, when running `guix home
reconfigure`, and we can be sure that Redshift is passed a valid
configuration file.

We can similarly define a [periodic mcron
job](https://guix.gnu.org/manual/devel/en/html_node/Mcron-Home-Service.html#index-home_002dmcron_002dservice_002dtype),
for example one that updates a
[GNU Idutils](https://www.gnu.org/software/idutils) search database
(that’s a pretty convenient and speedy way to look for code or
documents!):

```scheme
(simple-service 'idutils home-mcron-service-type
                ;; Every day at 12:15 and 19:15.
				(list #~(job '(next-minute-from (next-hour '(12 19)) '(15))
							 (string-append #$idutils "/bin/mkid \
-o $HOME/.idutils/src.db $HOME/src"))))
```

Again, `guix home` creates a Shepherd service that start mcron with a
configuration file containing definitions for periodic jobs, which we
can inspect *via* `herd`:

```
$ herd schedule mcron | head -5
Sun Mar 20 19:15:00 2022 +0000
/gnu/store/2d026nan309qkci968k8gpa8fcv9q4mv-idutils-4.6/bin/mkid -o $HOME/.idutils/src $HOME/src

Mon Mar 21 12:15:00 2022 +0000
/gnu/store/2d026nan309qkci968k8gpa8fcv9q4mv-idutils-4.6/bin/mkid -o $HOME/.idutils/src $HOME/src
```

# Services, composed

If you already use Guix System, all the above certainly looks familiar:
Guix Home builds upon the [service
framework](https://guix.gnu.org/manual/devel/en/html_node/Defining-Services.html)
that powers Guix System; Home services are defined in the [`(gnu home
services …)` module
tree](https://git.savannah.gnu.org/cgit/guix.git/tree/gnu/home/services).

That framework lets us define relations among
“services”, in a broad sense, and how services _extend_ each other—in
the example above, `redshift` and `mcron` both extend `shepherd` by
giving it a daemon to take care of.  We can see those relations at play
by running:

```
guix home extension-graph home-configuration.scm
```

… which, for the configuration described above, gives a graph that looks
like this:

![Extension graph for home services.](/static/blog/img/home-extension-graph.svg)

We see `redshift`, `mcron`, and `shepherd`, but we also see lower-level
services that `guix home` instantiates for us, such as the `profile`
service which takes care of deploying packages listed in the `packages`
field under `~/.guix-home/profile`.  Each arrow denotes a _service
extension_.  You can [read
more](https://guix.gnu.org/manual/devel/en/html_node/Service-Composition.html)
(and [view
more!](https://archive.fosdem.org/2017/schedule/event/composingsystemservicesinguixsd/))
about service composition.  To satisfy our math and
functional-programming geek audience, we should mention that service
types and their extension operation form a
[monoid](https://en.wikipedia.org/wiki/Monoid).

# What’s next?

Let’s be clear: Guix Home is pretty new and chances are that `guix home
search`—the command to search for services by keyword—won’t give you the
service you’re looking for.  There’s also a bunch of open questions
left, such as how to reuse services initially defined for Guix System in
cases where they could be equally useful in Guix
Home—[Syncthing](https://guix.gnu.org/manual/devel/en/html_node/Networking-Services.html#index-syncthing_002dservice_002dtype),
for example.

But while it’s still a “technology preview”, it’s already a tool that
tinkerers can play with and benefit from.  Patches adding new services
have already been proposed; maybe your favorite service is next?
Consider
[contributing](https://guix.gnu.org/manual/devel/en/html_node/Contributing.html).

With a new release and ten-year anniversary coming up, we’re happy to
celebrate with a tool that extends the reach of declarative and
reproducible deployment!

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
