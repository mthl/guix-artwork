title: Running a Guix XFCE Desktop on CentOS 7
date: 2019-11-17 19:00
author: Marius Bakke
tags: XFCE, foreign distribution
---

This tutorial will show how to run a fully fledged XFCE desktop environment
installed with Guix on top of an existing GNU/Linux distribution.  This guide
uses CentOS 7 as the base operating system and assumes that Xorg is already
configured and running on VT2 under a different user account.

We will borrow Xorg and `xinit` from the host distribution and run Guix XFCE on
virtual terminal 4 as user 'alice'.  No system-wide configuration files need to
be touched (apart from the Guix install), but we do make a couple of changes
for convenience.

#### From scratch to XFCE

If Guix is not already installed, go grab the
[installation script](https://git.savannah.gnu.org/cgit/guix.git/plain/etc/guix-install.sh)
and run it as `sudo bash guix-install.sh`.

The script creates `/gnu/store/` and `/var/guix/` and configures a system service
for `guix-daemon`.  By default the daemon runs from the 'root' users Guix; we
won't be using the root account in this guide, so let's start by making the
guix-daemon service refer to our local user 'alice' instead.

```sh
sudo sed -i 's/root/alice/' /etc/systemd/system/guix-daemon.service
```

Now every time Alice runs 'guix pull', the daemon gets updated too.  If you
installed Guix just now, make sure to run `guix pull` before proceeding further.

Next we'll add some lines to Alices `.bash_profile` to set up PATH and related
variables:

```sh
GUIX_PROFILE="${HOME}/.guix-profile"
[[ -L "${GUIX_PROFILE}" ]] && . "${GUIX_PROFILE}/etc/profile"

export PATH="${HOME}/.config/guix/current/bin:${PATH}"
export INFOPATH="${HOME}/.config/guix/current/share/info:${INFOPATH}"
export MANPATH="${HOME}/.guix-profile/share/man:/usr/share/man"

export XDG_CONFIG_DIRS="${HOME}/.desktop-profile/etc/xdg:${HOME}/.guix-profile/etc/xdg"
export XDG_DATA_DIRS="${HOME}/.desktop-profile/share:${HOME}/.guix-profile/share"
```

This will look familiar if you have used Guix on a foreign distribution before.
The `XDG_` variables tell XFCE where to look for installed programs and things
like autostart files: we want minimal interference from the host system, so we
"hard code" them to refer to just our Guix profiles.

We will install XFCE and related programs to a
[separate Guix profile](https://guix.gnu.org/cookbook/en/html_node/Guix-Profiles-in-Practice.html)
that can be updated and rolled back independently of the main user profile.
That allows us to distinguish between "stable desktop environment" and "end user
packages".  To keep things manageable, we create a _manifest_ for the desktop
profile that can be kept in version control, and which allows us to reproduce
the exact same environment in the future (even on a different computer!).

`~/desktop-manifest.scm`:
```scheme
(specifications->manifest
 '("xfce" "xfce4-session" "xfconf" "xfce4-battery-plugin"
   "pulseaudio" "xfce4-volumed-pulse" "xfce4-notifyd"
   ;; Helpful graphical programs.
   "mousepad" "orage"
   ;; System configuration utilities.
   "xbacklight" "pavucontrol" "stow"
   ;; For HTTPS access.
   "nss-certs"
   ;; These utilities are provided by the host, but we want the Guix versions
   ;; because they are likely better integrated and up to date.
   "fontconfig" "bash-completion" "gnupg" "man-db" "git"))
```

Create the initial profile generation:

```sh
guix package -p ~/.desktop-profile -m ~/desktop-manifest.scm
```

That installs a union of all packages listed in the manifest to
`~/.desktop-profile`, and creates a script we will use to "activate" it later.
To update this profile, simply invoke the same command again after running
`guix pull` or modifying the manifest.

Before XFCE can be started, we need to create a configuration file for the X
server to ensure the host executable is used, and we will tell it to to stay
on virtual terminal 4.  We also create a `.xinitrc` script that automatically
starts XFCE every time `xinit` is invoked.

`~/.xserverrc`:
```sh
exec /usr/bin/Xorg -novtswitch -nolisten tcp "$@" vt$XDG_VTNR
```

`~/.xinitrc`:
```sh
#!/bin/sh

# Get the default xinit configuration for CentOS.
. /etc/X11/xinit/xinitrc-common

exec startxfce4
```

`.xinitrc` needs to be executable:

```sh
chmod +x ~/.xinitrc
```

Now let's activate the desktop profile and start the X server, using ":1" as
`DISPLAY` (remember that we have another X server running on VT2, occupying the
default ":0" display).

```sh
GUIX_PROFILE=~/.desktop-profile
source ~/.desktop-profile/etc/profile
xinit -- :1
```

Cool, we're in XFCE!  Let's open a terminal and install a browser & some fonts:

```sh
guix install icecat font-liberation font-dejavu
```

To make the newly installed fonts available right away we need to invoke `fc-cache`:

```sh
fc-cache -rv
```

Finally, we'll configure the shell to source scripts installed by Guix so that
bash completions and similar work, by adding these lines at the end of `.bashrc`:

`~/.bashrc`:
```sh
# Source the Guix shell configuration directories, for vte.sh and bash completions.
GUIX_PROFILES=("${HOME}/.desktop-profile"
               "${HOME}/.guix-profile"
               "${HOME}/.config/guix/current")
for profile in "${GUIX_PROFILES[@]}"; do
    for dir in "${profile}/etc/bash_completion.d" "${profile}/etc/profile.d"; do
        if [[ -d "${dir}" ]]; then
            for f in "${dir}"/*; do
                . $f
            done
        fi
    done
done
```

Phew!  It took some work, but by now you should have a working XFCE desktop
environment, with bash completions and all.  If you are content with starting
it manually, skip to "final tweaks" below.  Otherwise, read on.

(If you do not have a working desktop after following these steps, please email
guix-devel@gnu.org so we can adjust the tutorial!)

#### Starting XFCE automatically on boot

We can configure our login shell to start XFCE every time we log in to VT4 by
adding these lines at the end of `~/.bash_profile`:

```sh
# Start Xorg on display :1 when logged in to VT4, unless DISPLAY is already set.
if [[ -z "${DISPLAY}" && "${XDG_VTNR}" == 4 ]]; then
    GUIX_PROFILE="${HOME}/.desktop-profile"
    source "${HOME}/.desktop-profile/etc/profile"
    exec xinit -- :1
fi
```

To avoid the need for typing username and password at the console, instruct the
`getty` service for TTY4 to automatically log in user 'alice':

`/etc/systemd/system/getty@tty4.service.d/override.conf`:
```
[Unit]
After=graphical.target

[Service]
# Delay for a few seconds, to ensure the Xorg server on VT2 starts first.
ExecStartPre=/bin/sleep 3
ExecStart=
ExecStart=-/sbin/agetty --autologin alice --noclear %I $TERM
Restart=on-success
```

Now just switching to VT4 will start XFCE!  To do this when the system boots,
simply enable the `getty@tty4` service:

```sh
sudo systemctl enable getty@tty4.service
```

## Final tweaks

Some issues were found during usage of the XFCE environment.  Launching programs
from the file manager failed because `gio-launch-desktop` was unavailable, and
xfce4-terminal complained that `__vte_prompt_command` was not found.

These problems will be fixed in Guix eventually, but for now we'll work around
them in our manifest:

`~/desktop-manifest.scm`:
```scheme
(specifications->manifest
 '("xfce" "xfce4-session" "xfconf" "xfce4-battery-plugin"
   ...
   "glib:bin"                  ;for 'gio-launch-desktop'
   "vte"))                     ;for vte.sh, required by xfce4-terminal
```

We also found that closing the lid would not send the system to sleep, even
though `xfce4-power-manager --dump` showed no problems.  To work around it,
we told systemd to ignore any "inhibitors" and take care of lid handling itself:

`/etc/systemd/logind.conf`:
```
HandleLidSwitch=suspend
LidSwitchIgnoreInhibited=yes
```

Additionally it is
[strongly recommended](https://guix.gnu.org/manual/en/html_node/Application-Setup.html#Name-Service-Switch-1)
to enable the _name service cache daemon_ if not already running.  On CentOS
this can be done by:

```sh
sudo yum install nscd
```

#### Bonus section: Installing programs with a custom build of Qt

One additional issue was that Qt programs did not work due to the stock CentOS
kernel being too old.  Specifically it lacks the `renameat2()` system call.
Luckily Qt can be configured to not use it.  A patch has been submitted to
Guix, but since we are in a hurry, we will add a procedure to our manifest so
we can use Qt programs (here `wpa-supplicant-gui`) until the Guix fix is merged:

`~/.desktop-manifest.scm`:
```scheme
(use-modules (guix packages)
             (guix utils)
             (gnu)
             (gnu packages admin)
             (gnu packages qt))

(define qtbase/fixed
  (package/inherit
   qtbase
   (arguments
    (substitute-keyword-arguments (package-arguments qtbase)
      ((#:phases phases)
       `(modify-phases ,phases
          (add-after 'unpack 'disable-renameat2
            (lambda _
              (substitute* "src/corelib/configure.json"
                (("config\\.linux && tests\\.renameat2")
                 "false"))
              #t))))))))

(define with-fixed-qt
  ;; This procedure recursively rewrites any references to 'qtbase'
  ;; with our patched version.
  (package-input-rewriting `((,qtbase . ,qtbase/fixed))))

(packages->manifest
 (append (list (with-fixed-qt wpa-supplicant-gui))
         (map specification->package
              '("xfce" "xfce4-session" "xfconf" "xfce4-battery-plugin"
                ...))))
```

...and now `wpa_gui` works after installing the new manifest!

#### Acknowledgements

Special thanks to [Ocean Space Acoustics AS](https://www.osac.no/) for sponsoring this work.

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
