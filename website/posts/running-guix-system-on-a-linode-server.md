title: Running Guix System on a Linode Server
date: 2020-10-06 14:30
author: Joshua Branson, Christopher Lemmer Webber
tags: Virtualization, Devops
---

Christopher Lemmer Webber recently discovered how to run Guix System on a
[Linode](https://www.linode.com/) server.  The below guide details how to set up
your Linode server to run Guix System. We invite you to run your website using
Guix system!

To run Guix on a server hosted by Linode,
start with a recommended Debian server.  We recommend using the default
distro as a way to bootstrap Guix. Create your SSH keys.

```
ssh-keygen
```

Be sure to add your SSH key for easy login to the remote server.
This is trivially done via Linode's graphical interface for adding
SSH keys.  Go to your profile and click add SSH Key.
Copy into it the output of:

```
cat ~/.ssh/<username>_rsa.pub
```

Power the Linode down. In the Linode's _Disks/Configurations_ tab, resize
the Debian disk to be smaller.  30 GB is recommended.

In the Linode settings, choose _Add a disk_ with the following:
- Label: `Guix`
- Filesystem: ext4
- Set it to the remaining size

On the _configuration_ field that comes with the default image, press
_..._ and select _Edit_, then on that menu add to `/dev/sdc` the `Guix`
label.

Now select _Add a Configuration_, with the following:
- Label: `Guix`
- Kernel: GRUB 2 (it's at the bottom!  This step is *important*!
- Block device assignment:
  - `/dev/sda`: Guix
  - `/dev/sdb`: swap
- Root device: `/dev/sda`
- Turn off all the filesystem/boot helpers.

Now power it back up, picking the Debian configuration.  Once it's booted up,
ssh in your server via `ssh root@<your-server-IP-here>`. (You can find your
server IP address in your Linode Summary section.)  Now you can run the
[binary installation as explained in the
manual](https://guix.gnu.org/manual/en/html_node/Binary-Installation.html):

```
sudo apt-get install gpg
wget https://sv.gnu.org/people/viewgpg.php?user_id=15145 -qO - | gpg --import -
wget https://git.savannah.gnu.org/cgit/guix.git/plain/etc/guix-install.sh
chmod +x guix-install.sh
./guix-install.sh
guix pull
```

Now it's time to write out a config for the server.  The key information
is below. Save the resulting file as `guix-config.scm`.

```scheme
(use-modules (gnu)
             (guix modules))
(use-service-modules networking
                     ssh)
(use-package-modules admin
                     certs
                     package-management
                     ssh
                     tls)

(operating-system
  (host-name "my-server")
  (timezone "America/New_York")
  (locale "en_US.UTF-8")
  ;; This goofy code will generate the grub.cfg
  ;; without installing the grub bootloader on disk.
  (bootloader (bootloader-configuration
               (bootloader
                (bootloader
                 (inherit grub-bootloader)
                 (installer #~(const #t))))))
  (file-systems (cons (file-system
                        (device "/dev/sda")
                        (mount-point "/")
                        (type "ext4"))
                      %base-file-systems))
  (swap-devices (list "/dev/sdb"))

  (initrd-modules (cons "virtio_scsi"    ;needed to find the disk
                        %base-initrd-modules))

  (users (cons (user-account
                (name "janedoe")
                (group "users")
                ;; Adding the account to the "wheel" group
                ;; makes it a sudoer.
                (supplementary-groups '("wheel"))
                (home-directory "/home/janedoe"))
               %base-user-accounts))

  (packages (cons* nss-certs            ;for HTTPS access
                   openssh-sans-x
                   %base-packages))

  (services (cons*
             (service dhcp-client-service-type)
             (service openssh-service-type
                      (openssh-configuration
                       (openssh openssh-sans-x)
                       (password-authentication? #f)
                       (authorized-keys
                        `(("janedoe" ,(local-file "janedoe_rsa.pub"))
                          ("root" ,(local-file "janedoe_rsa.pub"))))))
             %base-services)))
```

Replace the following fields in the above configuration:

```scheme
(host-name "my-server")       ; replace with your server name
; if you chose a linode server outside the U.S., then
; use tzselect to find a correct timezone string
(timezone "America/New_York") ; if needed replace timezone
(name "janedoe")              ; replace with your username
("janedoe" ,(local-file "janedoe_rsa.pub")) ; replace with your ssh key
("root" ,(local-file "janedoe_rsa.pub")) ; replace with your ssh key
```

The last line in the above example lets you log into the server as root
and set the initial root password.  After you have done this, you may
delete that line from your configuration and reconfigure to prevent root
login.

Save your ssh public key (eg: `~/.ssh/id_rsa.pub`) as
`<your-username-here>_rsa.pub` and your `guix-config.scm` in the
same directory.  In a new terminal run these commands.

```
sftp root@@<remote server ip address>
put /home/<username>/ssh/id_rsa.pub .
put /path/to/linode/guix-config.scm .
```

In your first terminal, mount the `guix` drive:

```
mkdir /mnt/guix
mount /dev/sdc /mnt/guix
```

Due to the way we set things up above, we do not install GRUB
completely.  Instead we install only our grub configuration file.  So we
need to copy over some of the other GRUB stuff that is already there:

```
mkdir -p /mnt/guix/boot/grub
cp -r /boot/grub/* /mnt/guix/boot/grub/
```

Now initialize the Guix installation:

```
guix system init guix-config.scm /mnt/guix
```

Ok, power it down!
Now from the Linode console, select boot and select _Guix_.

Once it boots, you should be able to log in via SSH!  (The server config
will have changed though.)  You may encounter an error like:

```
$ ssh root@<server ip address>
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@    WARNING: REMOTE HOST IDENTIFICATION HAS CHANGED!     @
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
IT IS POSSIBLE THAT SOMEONE IS DOING SOMETHING NASTY!
Someone could be eavesdropping on you right now (man-in-the-middle attack)!
It is also possible that a host key has just been changed.
The fingerprint for the ECDSA key sent by the remote host is
SHA256:0B+wp33w57AnKQuHCvQP0+ZdKaqYrI/kyU7CfVbS7R4.
Please contact your system administrator.
Add correct host key in /home/joshua/.ssh/known_hosts to get rid of this message.
Offending ECDSA key in /home/joshua/.ssh/known_hosts:3
ECDSA host key for 198.58.98.76 has changed and you have requested strict checking.
Host key verification failed.
```

Either delete `~/.ssh/known_hosts` file, or delete the offending line
starting with your server IP address.

Be sure to set your password and root's password.

```
ssh root@<remote ip address>
passwd  ; for the root password
passwd <username> ; for the user password
```

You may not be able to run the above commands at this point.  If you have issues
remotely logging into your linode box via SSH, then you may still need to set
your root and user password initially by clicking on the _Launch Console_ option
in your linode.  Choose the _Glish_ instead of _Weblish_.  Now you should be
able to ssh into the machine.

Hooray!  At this point you can shut down the server, delete the
Debian disk, and resize the Guix to the rest of the size.
Congratulations!

By the way, if you save it as a disk image right at this point, you'll
have an easy time spinning up new Guix images!  You may need to
down-size the Guix image to 6144 MB, to save it as an image.  Then you
can resize it again to the max size.

That's all for today!  We hope you have fun playing with your brand new
Guix System Server!

> A variant of this guide is available in the [Guix
> Cookbook](https://guix.gnu.org/cookbook/en/html_node/Running-Guix-on-a-Linode-Server.html).

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
