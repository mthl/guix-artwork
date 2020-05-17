/* vim: set filetype=text : */
title: GNU Shepherd user services
date: 2020-05-17 20:00:00
author: Efraim Flashner
slug: gnu-shepherd-user-services
tags: Scheme API, shepherd
---

One of the things which sets Guix apart from other GNU/Linux
distributions is that it uses [GNU
Shepherd](https://www.gnu.org/software/shepherd/) instead of the now
ubiquitous systemd.  A side effect of this is that user systemd units do
not work on Guix System.  Love, hate or extremely ambivalent toward
systemd, this means that users cannot rely on already written systemd
unit files for their regular user-level services.

There are a couple of benefits to using GNU Shepherd, and not all of
them are due to it already being installed on Guix. Becoming comfortable
with using Shepherd and understanding how to write and edit Shepherd
service configurations makes the transition from other GNU/Linux
distributions to Guix System easier. More complex services with their
own logic tree, using the full power of [GNU
Guile](https://www.gnu.org/software/guile/), are also possible. This
means you can have one service that behaves differently if it's running
on a different system or architecture without needing to call out to
shell scripts or using minimally different service definitions.

The GNU Shepherd manual
[suggests](https://www.gnu.org/software/shepherd/manual/html_node/Jump-Start.html#index-Configuration-file)
putting all the services inside a
monolithic `init.scm` file, located by default at
`$XDG_CONFIG_DIR/shepherd/init.scm`. While this does make it easy to keep
everything in one place, it does create one glaring issue: any changes
to the file mean that all the services need to be stopped and restarted
in order for any changes to take place.

Luckily there's a nice function called `scandir` hiding in [`ice-9
ftw`](https://www.gnu.org/software/guile/manual/html_node/File-Tree-Walk.html#index-scandir)
which returns a list of all files in a specified directory (with options
for narrowing down the list or sorting it). This means that our `init.scm`
can contain a minimum of code and all actual services can be loaded from
individual files.

First the minimal `init.scm`:

```scheme
(use-modules (shepherd service)
             ((ice-9 ftw) #:select (scandir)))

;; Load all the files in the directory 'init.d' with a suffix '.scm'.
(for-each
  (lambda (file)
    (load (string-append "init.d/" file)))
  (scandir (string-append (dirname (current-filename)) "/init.d")
           (lambda (file)
             (string-suffix? ".scm" file))))

;; Send shepherd into the background
(action 'shepherd 'daemonize)
```

Let's take a sample service for running syncthing, as defined in
`$XDG_CONFIG_DIR/shepherd/init.d/syncthing.scm`:
```scheme
(define syncthing
  (make <service>
    #:provides '(syncthing)
    #:docstring "Run `syncthing' without calling the browser"
    #:start (make-forkexec-constructor
              '("syncthing" "-no-browser")
              #:log-file (string-append (getenv "HOME")
                                        "/log/syncthing.log"))
    #:stop (make-kill-destructor)
    #:respawn? #t))
(register-services syncthing)

(start syncthing)
```

As with any other shepherd service it is defined and registered, and in
this case it will start automatically.  When the file is loaded by
shepherd after being discovered by scandir everything works exactly as
though the service definition were located directly inside the `init.scm`.

Now lets make a change.  Since syncthing already has a `-logfile` flag and
it has built-in log rotation that sounds better than using shepherd's
`#:log-file` option.  First we'll make our changes to the service:

```scheme
(define syncthing
  (make <service>
    #:provides '(syncthing)
    #:docstring "Run `syncthing' without calling the browser"
    #:start (make-forkexec-constructor
              '("syncthing" "-no-browser"
                "-logflags=3" ; prefix with date & time
                "-logfile=/home/user/log/syncthing.log"))
    #:stop (make-kill-destructor)
    #:respawn? #t))
(register-services syncthing)

(start syncthing)
```

Now we stop syncthing:

```sh
$ herd stop syncthing
```

And we load the new service:

```sh
$ herd load root ~/.config/shepherd/init.d/syncthing.scm
```

This allows for quickly iterating on services without needing to stop
all the services!  Let's take a look at another service:

```scheme
(define fccache
  (make <service>
    #:provides '(fccache)
    #:docstring "Run 'fc-cache -frv'"
    #:start (make-forkexec-constructor
              '("guix" "environment" "--ad-hoc" "fontconfig" "--"
                "fc-cache" "-frv")
              #:log-file (string-append (getenv "HOME")
                                        "/log/fccache.log"))
    #:one-shot? #t))

(register-services fccache)
```

In this example I want to refresh my font cache but I don't want to
actually install fontconfig either system-wide or in my profile.

```sh
$ which fc-cache
which: no fc-cache in (/home/user/.config/guix/current/bin:/home/user/.guix-profile/bin:/home/user/.guix-profile/sbin:/run/setuid-programs:/run/current-system/profile/bin:/run/current-system/profile/sbin)
$ herd start fccache
Service fccache has been started.
```

Of course we can import other modules and leverage the code already
written there. In this case, instead of using the string "guix
environment --ad-hoc fontutils -- fc-cache -frv" let's use the `guix
environment` function already available in `guix scripts environment`:

```scheme
(use-modules (guix scripts environment))

(define fccache
  (make <service>
    #:provides '(fccache)
    #:docstring "Run 'fc-cache -frv'"
    #:start (lambda () ; Don't run immediately when registered!
              (guix-environment "--ad-hoc" "fontconfig" "--" "fc-cache" "-frv"))
    #:one-shot? #t))

(register-services fccache)
```

```sh
$ herd load root ~/.config/shepherd/init.d/fccache.scm
Loading /home/user/.config/shepherd/init.d/fccache.scm.
$ herd start fccache
/gnu/store/hbqlzgd8hcf6ndcmx7q7miqrsxb4dmkk-gs-fonts-8.11/share/fonts: caching, new cache contents: 0 fonts, 1 dirs
/gnu/store/hbqlzgd8hcf6ndcmx7q7miqrsxb4dmkk-gs-fonts-8.11/share/fonts/type1: caching, new cache contents: 0 fonts, 1 dirs
/gnu/store/hbqlzgd8hcf6ndcmx7q7miqrsxb4dmkk-gs-fonts-8.11/share/fonts/type1/ghostscript: caching, new cache contents: 35 fonts, 0 dirs
/home/user/.guix-profile/share/fonts: caching, new cache contents: 0 fonts, 7 dirs
/home/user/.guix-profile/share/fonts/opentype: caching, new cache contents: 8 fonts, 0 dirs
/home/user/.guix-profile/share/fonts/otf: caching, new cache contents: 12 fonts, 0 dirs
/home/user/.guix-profile/share/fonts/terminus: caching, new cache contents: 18 fonts, 0 dirs
/home/user/.guix-profile/share/fonts/truetype: caching, new cache contents: 58 fonts, 0 dirs
/home/user/.guix-profile/share/fonts/ttf: caching, new cache contents: 12 fonts, 0 dirs
/home/user/.guix-profile/share/fonts/type1: caching, new cache contents: 0 fonts, 1 dirs
/home/user/.guix-profile/share/fonts/type1/ghostscript: caching, new cache contents: 35 fonts, 0 dirs
/home/user/.guix-profile/share/fonts/woff: caching, new cache contents: 1 fonts, 0 dirs
/run/current-system/profile/share/fonts: skipping, no such directory
/home/user/.local/share/fonts: skipping, no such directory
/home/user/.fonts: skipping, no such directory
/gnu/store/hbqlzgd8hcf6ndcmx7q7miqrsxb4dmkk-gs-fonts-8.11/share/fonts/type1: skipping, looped directory detected
/home/user/.guix-profile/share/fonts/opentype: skipping, looped directory detected
/home/user/.guix-profile/share/fonts/otf: skipping, looped directory detected
/home/user/.guix-profile/share/fonts/terminus: skipping, looped directory detected
/home/user/.guix-profile/share/fonts/truetype: skipping, looped directory detected
/home/user/.guix-profile/share/fonts/ttf: skipping, looped directory detected
/home/user/.guix-profile/share/fonts/type1: skipping, looped directory detected
/home/user/.guix-profile/share/fonts/woff: skipping, looped directory detected
/gnu/store/hbqlzgd8hcf6ndcmx7q7miqrsxb4dmkk-gs-fonts-8.11/share/fonts/type1/ghostscript: skipping, looped directory detected
/home/user/.guix-profile/share/fonts/type1/ghostscript: skipping, looped directory detected
/var/cache/fontconfig: not cleaning unwritable cache directory
/home/user/.cache/fontconfig: cleaning cache directory
/home/user/.fontconfig: not cleaning non-existent cache directory
fc-cache: succeeded
herd: exception caught while executing 'start' on service 'fccache':
Throw to key `quit' with args `(0)'.
```

The problem with this approach is that `guix-environment` returns the
[exit
code](https://git.savannah.gnu.org/cgit/guix.git/tree/guix/scripts/environment.scm?h=v1.1.0#n771)
of the programs it calls and `#:start` expects a
[`constructor`](https://www.gnu.org/software/shepherd/manual/html_node/Slots-of-services.html#index-Starting-a-service-1)
to return `#t` or `#f` so there's some work to be done here.

This was just a quick peek into what's possible with GNU Shepherd when
run as a user. Next time we'll take a look at integrating
[mcron](https://www.gnu.org/software/mcron/) to replicate some of
systemd's timer functionality.

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
