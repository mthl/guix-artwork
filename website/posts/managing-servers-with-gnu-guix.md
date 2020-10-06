title: Managing Servers with GNU Guix: A Tutorial
date: 2019-11-04 15:00
author: Jakob L. Kreuze
tags: GSoC, Programming interfaces, Scheme API, Devops
---

The outcome of this year's
[GSoC](https://summerofcode.withgoogle.com/projects/#5232565294727168)
is a Guile-based programming interface named `guix deploy` for
automatically creating, upgrading, and changing the configurations of
machines running the Guix System.  The tool is comparable to
[Ansible](https://www.ansible.com/) or
[NixOps](https://nixos.org/nixops/), but makes use of the system
configuration facilities provided by Guix.  A [post from earlier this
summer](http://guix.gnu.org/blog/2019/towards-guix-for-devops/)
described an early version of the programming interface, but we're
already a few months into autumn, so it's time for guide on how you
can use `guix deploy` in production.

#### Simple case: managing a home server

If the machine you need to manage is already running the Guix System,
it shouldn't be too hard to incorporate `guix deploy` into your
workflow.  All that's needed is the `<operating-system>` declaration
you've been passing to `guix system reconfigure` and some information
about the machine (specifically its IP address and architecture). The
`guix deploy` command is invoked with the filename of a "deployment
specification" as an argument, whose contents should look something
like this:

```scheme
;; Module imports
(use-modules (gnu) (guix))
(use-service-modules networking ssh)
(use-package-modules bootloaders)

;; Operating system description
(define os
  (operating-system
    (locale "en_US.utf8")
    (timezone "America/New_York")
    (keyboard-layout (keyboard-layout "us" "altgr-intl"))
    (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (target "/dev/sda")
                 (keyboard-layout keyboard-layout)))
    (file-systems (cons* (file-system
                          (mount-point "/")
                          (device "/dev/sda1")
                          (type "ext4"))
                         %base-file-systems))
    (host-name "alyssas-home-server")
    (users (cons* (user-account
                   (name "alyssa")
                   (comment "Alyssa")
                   (group "users")
                   (home-directory "/home/alyssa")
                   (supplementary-groups
                    '("wheel" "netdev" "audio" "video")))
                  %base-user-accounts))
    (sudoers-file (plain-file "sudoers" "\
root ALL=(ALL) ALL
%wheel ALL=NOPASSWD: ALL\n"))
    (services (append
               (list (service openssh-service-type
                              (openssh-configuration
                               (permit-root-login #t)))
                     (service dhcp-client-service-type))
               %base-services))))

;; List of machines to deploy
(list (machine
       (operating-system os)
       (environment managed-host-environment-type)
       (configuration (machine-ssh-configuration
                       (host-name "alyssa-p-hacker.tld")
                       (system "i686-linux")
                       (identity "/path/to/ssh-key")))))
```

Even if Scheme isn't your forté, parts of this should look familiar if
you've used Guix before.  The "operating system description" section
in particular is something you might use with `guix system
reconfigure`.  What's new is the last part: We construct a `list`
containing one `machine` of the `managed-host-environment-type`, for
which we've specified that `os` is the `operating-system`
declaration that we want to install on it, and that we can connect to
it using the parameters specified by the `machine-ssh-configuration`.

Let's take a step back for a moment and explain what a `machine` is.
`guix deploy` aims to support a number of different use-cases, which
we abstract as "environment types".  We'll see other environment types
later in this article, but the general idea is that these environments
specify how resources should be "provisioned" or created.  For
example, an environment type designed for working with a Virtual
Private Server (VPS) provider might make calls the provider's API to
request a virtual machine before installing the `machine`'s
`operating-system` declaration on it.

The environment type used in this example,
`managed-host-environment-type`, is intended for machines that are
already running Guix System and are accessible over SSH.  It expects
that the `configuration` field of the `machine` be an instance of
`machine-ssh-configuration`, whose available fields are described in
the
[manual](https://guix.gnu.org/manual/devel/en/html_node/Invoking-guix-deploy.html).
This gives `guix deploy` the information it needs to connect to the
machine's SSH daemon.

Running `guix deploy` with this file would build the "operating system
closure" of `os` -- a bundle of the packages, configuration
files, and other dependencies necessary to realize that configuration
-- for the architecture specified by `system` (in this case
`i686-linux`), send it over SSH to `alyssa-p-hacker.tld`, and then
remotely "activate" the configuration by creating a new system
generation and upgrading running services.  Sweet!  Upgrading our
single server setup has been reduced to an endeavour involving just
over a dozen keystrokes.

#### More advanced case: managing a virtual private server deployment

One server not cutting it for you?  `guix deploy` can still help.
Suppose we run a web service that we'd like to split up across
multiple machines for performance reasons.

```scheme
(define %forum-server-count 4)

(define (forum-server n)
  (operating-system
    (host-name (format #f "forum-server-~a" n))
    ...
    (services (append (list (service httpd-service-type
                                     (httpd-configuration
                                      ...)))
                      %base-services))))

(map (lambda (n)
       (machine
        (system (forum-server n))
        (environment digital-ocean-environment-type)
        (configuration (digital-ocean-configuration
                        (region "nyc3")
                        (size "s-1vcpu-1gb")
                        (enable-ipv6 #t)))))
     (iota %forum-server-count))
```

This example isn't as concrete as the first one; I'm intentionally
omitting parts of the configuration to make the example clearer.
Here, we automate the creation of `%forum-server-count` Digital Ocean
"droplets" in their `NYC3` region by creating a list of 4 machines.

Assuming that the environment variable `GUIX_DIGITAL_OCEAN_TOKEN` is
properly set, running `guix deploy` with this file will do much of the
same as the previous example.  The difference is that four virtual
machines will be automatically created on Digital Ocean.

One important thing to note about the `digital-ocean-environment-type`
is that, currently, it *does not* automatically clean up unused
virtual machines. If you change something in the deployment
specification and run `guix deploy` again, the virtual machines from
the previous deployment will remain until you destroy them yourself.

#### A quick peek into the internals of `digital-ocean-environment-type`

It would be an overstatement to say that the process of implementing a
new environment type is easy, but a fair amount of the work has
already been done for you.  We'll use the definition of
`digital-ocean-environment-type` as an example.

```scheme
(define digital-ocean-environment-type
  (environment-type
   (machine-remote-eval digital-ocean-remote-eval)
   (deploy-machine      deploy-digital-ocean)
   (roll-back-machine   roll-back-digital-ocean)
   (name                'digital-ocean-environment-type)
   (description         "Provisioning of \"droplets\": virtual machines
 provided by the Digital Ocean virtual private server (VPS) service.")))
```

The `environment-type` record specifies a small amount of metadata
(`name` and `description`), as well as the names of three procedures:
one for remotely evaluating a
[G-Expression](http://guix.gnu.org/manual/en/html_node/G_002dExpressions.html#G_002dExpressions)
on the host (`machine-remote-eval`), one for deploying an
`operating-system` declaration to the host, and one for rolling the
host back one generation.

This might sound like a lot, but the pattern for these high-level
environment types is to somehow obtain a machine running Guix System,
set up an SSH daemon, and then delegate to
`managed-host-environment-type`.  `digital-ocean-remote-eval` is a
pretty good example of this:

```scheme
(define (digital-ocean-remote-eval target exp)
  "Internal implementation of 'machine-remote-eval' for MACHINE instances with
an environment type of 'digital-ocean-environment-type'."
  (mlet* %store-monad ((name (droplet-name target))
                       (network -> (droplet-public-ipv4-network name))
                       (address -> (hash-ref network "ip_address"))
                       (ssh-key -> (digital-ocean-configuration-ssh-key
                                    (machine-configuration target)))
                       (delegate -> (machine
                                     (inherit target)
                                     (environment managed-host-environment-type)
                                     (configuration
                                      (machine-ssh-configuration
                                       (host-name address)
                                       (identity ssh-key)
                                       (system "x86_64-linux"))))))
    (machine-remote-eval delegate exp)))
```

As you can see, you could reasonably go about implementing an
environment type without ever having to learn what a G-Expression is.
Here, `droplet-name` derives the name of the droplet from the
machine's `operating-system` declaration, the information necessary to
connect to the droplet is found using `droplet-public-ipv4-network`,
and that's used to create machine of `managed-host-environment-type`.

#### In conclusion

I sincerely hope that `guix deploy` proves to be a useful to anyone
dealing with system administration or software development.
Transactional upgrades should provide peace of mind to those managing
servers (it's worth noting that few existing tools are capable of
recovering from failed deployments), and I believe that
procedurally-generated deployment configurations could very well be
the future of distribution for software such as web services: Imagine
if setting up a [Mastodon](https://joinmastodon.org) instance were as
easy as downloading a Scheme file and handing it off to `guix deploy`.
The ease of writing code that generates code isn't the only benefit of
using Guile for something like this.  Guile is a general-purpose
programming language, so more advanced tooling can reasonably be built
atop `guix deploy`. A GTK or Emacs DevOps interface, perhaps?  (If
that idea sounds outlandish, consider that the latter has [already
happened](https://emacs-guix.gitlab.io/website/) for the package
management interface.)

It's been a great summer working alongside everyone in the Guix
community.  `guix deploy` is brand new (and a little unstable!), but
we've had enthusiastic adoption by several on the mailing lists who
were quick to report any issues they found.  I'd like to thank
everyone on the `#guix` IRC channel and the mailing lists who got me
up to speed with the code, answered my questions, gave feedback when I
submitted my patches, and put `guix deploy` under the pressure of use
in production.  And of course, I want to thank my mentors Christopher
Lemmer Webber and David Thompson.  I had to make some hard design
decisions, but this was made easier thanks to the guidance of two
experienced Guix veterans.

Oh, and this isn't a goodbye.  I really feel I've found my place as a
Guix contributor, and I can't wait to see what the future will bring
for `guix deploy`.  Catch ya on the mailing lists!

#### Editor's note

Thank you for all of your hard work, Jakob!

#### About GNU Guix

[GNU Guix](https://www.gnu.org/software/guix) is a transactional package
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
