title: Guix on an ARM Board
date: 2019-11-27 12:00
author: Julien Lepiller
tags: ARM
---

Increasingly people discovering Guix want to try it on an ARM board, instead of
their x86 computer.  There might be various reasons for that, from power consumption
to security.  In my case, I found these ARM boards practical for self-hosting,
and I think the unique properties of GNU Guix are making it very suitable for that
purpose.  I have installed GNU Guix on a Cubietruck, so my examples below will be
about that board.  However, you should be able to change the examples for your
own use case.

Installing the Guix System on an ARM board is not as easy as installing it on an
x86 desktop computer: there is no installation image.  However, Guix supports
ARM and can be installed on a foreign distribution running on that architecture.
The trick is to use the Guix installed on that foreign distribution to initialize
the Guix System.  This article will show you how to install the Guix System on
your board, without using an installer image.  As we have previously
[mentioned](https://guix.gnu.org/blog/2017/porting-guixsd-to-armv7/) it is
possible to generate an installation image yourself, if your board is supported.

Most boards can be booted from an existing GNU+Linux distribution.  You will
need to install a distribution (any of them) and install GNU Guix on it, using
e.g. the [installer script](http://guix.gnu.org/manual/en/html_node/Binary-Installation.html).
Then, my plan was to install the Guix System on an external SSD drive, instead
of the SD card, but we will see that both are perfectly possible.

The first part of the article will focus on creating a proper u-boot configuration
and an operating system declaration that suits your board. The second part of this
article will focus on the installation procedure, when there is no installer working
for your system.

Writing a configuration file for an ARM board
---------------------------------------------

A configuration file for an ARM board is not very different from a configuration file
for a desktop or a server running on another architecture.  However, most boards use
the u-boot bootloader and require some less common modules to be available at boot time.

### The root file system

First of all, you should decide where your root file system is going to be installed. In
my case, I wanted to install is on the external SSD, so I chose it:

```scheme
(file-systems
  (cons* (file-system
           (mount-point "/")
           (device "/dev/sda1")
           (type "ext4"))
         %base-file-systems))
```

If you instead want to install the root file system on an SD card, you'll need
to find its device name, usually `/dev/mmcblk0` and the partition number.  The
device corresponding to the first partition should be `/dev/mmcblk0p1`.  In that
case, you would have:

```scheme
(file-systems
  (cons* (file-system
           (mount-point "/")
           (device "/dev/mmcblk0p1")
           (type "ext4"))
         %base-file-systems))
```

### The bootloader

Because of the way the Guix System is designed, you cannot use an already existing bootloader
to boot your system: it wouldn't know where to look for the kernel, because it doesn't know
its store path.  It wouldn't be able to let you boot older generations either.  Most boards
use the u-boot bootloader, so we will focus on that bootloader here.

Contrary to grub, there are multiple variants of u-boot, one per board type.  The installation
procedure for u-boot is also somewhat specific to the board, so there are two things that you
need to take care of: the u-boot package and the bootloader declaration.

Guix already define a few u-boot based bootloaders, such as `u-boot-a20-olinuxino-lime-bootloader`
or `u-boot-pine64-plus-bootloader` among others.  If your board already has a `u-boot-*-bootloader`
defined in `(gnu bootloader u-boot)`, you're lucky and you can skip this part of the article!

Otherwise, maybe the bootloader package is defined in `(gnu packages bootloaders)`, such as
the `u-boot-cubietruck` package.  If so, you're a bit lucky and you can skip creating your
own package definition.

If your board doesn't have a `u-boot-*` package defined, you can create one.  It could be
as simple as `(make-u-boot-package "Cubietruck" "arm-linux-gnueabihf")`.  The first argument
is the board name, as expected by the u-boot build sysetem.  The second argument is the
target triplet that corresponds to the architecture of the board.  You should refer to the
documentation of your board for selecting the correct values.  If you're really unlucky,
you'll need to do some extra work to make the u-boot package you just created work, as is
the case for the `u-boot-puma-rk3399` for instance: it needs additional phases to install
firmware.

You can add the package definition to your operating system configuration file like so,
before the operating-system declaration:

```scheme
(use-modules (gnu packages bootloaders))

(define u-boot-my-board
  (make-u-boot-package "Myboard" "arm-linux-gnueabihf"))

(operating-system
  [...])
```

Then, you need to define the bootloader.  A bootloader is a structure that has a name,
a package, an installer, a configuration file and a configuration file generator.  Fortunately,
Guix already defines a base u-boot bootloader, so we can inherit from it and only redefine a few
things.

The Cubietruck happens to be based on an allwinner core, for which there is already a
u-boot bootloader definition `u-boot-allwinner-bootloader`.  This bootloader is not
usable as is for the Cubietruck, but it defines most of what we need.  In order to get
a proper bootloader for the Cubietruck, we define a new bootloader based on the
Allwinner bootloader definition:

```scheme
(define u-boot-cubietruck-bootloader
  (bootloader
    (inherit u-boot-allwinner-bootloader)
    (package u-boot-cubietruck)))
```

Now that we have our definitions, we can choose where to install the bootloader.  In the
case of the Cubietruck, I decided to install it on the SD card, because it cannot boot from
the SSD directly.  Refer to your board documentation to make sure you install u-boot on
a bootable device.  As we said earlier, the SD card is `/dev/mmcblk0` on my device.

We can now put everything together like so:

```scheme
(use-modules (gnu packages bootloaders))

(define u-boot-cubietruck
  (make-u-boot-package "Cubietruck" "arm-linux-gnueabihf"))

;; u-boot-allwinner-bootloader is not exported by (gnu bootloader u-boot) so
;; we use @@ to get it.  (@ (module) variable) means: get the value of "variable"
;; as defined (and exported) in (module).  (@@ (module) variable) is the same, but
;; it doesn't care whether it is exported or not.
(define u-boot-allwinner-bootloader
  (@@ (gnu bootloader u-boot) u-boot-allwinner-bootloader))

(define u-boot-cubietruck-bootloader
  (bootloader
    (inherit u-boot-allwinner-bootloader)
    (package u-boot-cubietruck)))

(operating-system
  [...]
  (bootloader
    (bootloader-configuration
      (target "/dev/mmcblk0")
      (bootloader u-boot-cubietruck-bootloader)))
  [...])
```

### The kernel modules

In order for Guix to be able to load the system from the initramfs, it will probably need
to load some modules, especially to access the root file system.  In my case, the SSD is
on an ahci device, so I need a driver for it.  The kernel defines `ahci_sunxi` for that
device on any sunxi board.  The SD card itself also requires two drivers: `sunxi-mmc` and
`sd_mod`.

Your own board may need other kernel modules to boot properly, however it is hard to discover
them.  Guix can tell you when a module is missing in your configuration file if it is loaded
as a module.  Most distros however build these modules in the kernel directly, so Guix cannot
detect them reliably.  Another way to find what drivers might be needed is to look at the output
of `dmesg`.  You'll find messages such as:

```
[    5.193684] sunxi-mmc 1c0f000.mmc: Got CD GPIO
[    5.219697] sunxi-mmc 1c0f000.mmc: initialized, max. request size: 16384 KB
[    5.221819] sunxi-mmc 1c12000.mmc: allocated mmc-pwrseq
[    5.245620] sunxi-mmc 1c12000.mmc: initialized, max. request size: 16384 KB
[    5.255341] mmc0: host does not support reading read-only switch, assuming write-enable
[    5.265310] mmc0: new high speed SDHC card at address 0007
[    5.268723] mmcblk0: mmc0:0007 SD32G 29.9 GiB
```

or

```
[    5.614961] ahci-sunxi 1c18000.sata: controller can't do PMP, turning off CAP_PMP
[    5.614981] ahci-sunxi 1c18000.sata: forcing PORTS_IMPL to 0x1
[    5.615067] ahci-sunxi 1c18000.sata: AHCI 0001.0100 32 slots 1 ports 3 Gbps 0x1 impl platform mode
[    5.615083] ahci-sunxi 1c18000.sata: flags: ncq sntf pm led clo only pio slum part ccc 
[    5.616840] scsi host0: ahci-sunxi
[    5.617458] ata1: SATA max UDMA/133 mmio [mem 0x01c18000-0x01c18fff] port 0x100 irq 37
[    5.933494] ata1: SATA link up 3.0 Gbps (SStatus 123 SControl 300)
```

Also note that module names are not consistent between what Guix expects and what is printed by
dmesg, especially when the contain a "-" or a "_".  You will find the correct file name by building
(or using a substitute for) linux-libre beforehand:

```
find `guix build linux-libre`/lib/modules -name '*mmc*'
```

Here, I could find a file named "kernel/drivers/mmc/host/sunxi-mmc.ko", hence the module
name `sunxi-mmc`.  For the other driver, I found a "kernel/drivers/ata/ahci_sunxi.ko",
hence the name `ahci_sunxi`, even if dmesg suggested `ahci-sunxi`.

Once you have found the modules you want to load before mounting the root partition, you can
add them to your operating-system declaration file:

```scheme
(initrd-modules (cons* "sunxi-mmc" "sd_mod" "ahci_sunxi" %base-initrd-modules))
```

Installing the Guix System
--------------------------

### Installing on another drive

In my case, I wanted to install the system on an external SSD, while the currently running
foreign distribution was running from the SD card.  What is nice with this setup is that,
in case of real trouble (you SSD caught fire or broke), you can still boot from the old
foreign system with an installed Guix and all your tools by re-flashing only the bootloader.

In this scenario, we use the foreign system as we would the installer iso, using the manual
installation procedures described in the manual.  Essentially, you have to partition your SSD
to your liking, format your new partations and make sure to reference the correct partition
for the root file system in your configuration file.  Then, initialize the system with:

```bash
mount /dev/sda1 /mnt
mkdir /mnt/etc
$EDITOR /mnt/etc/config.scm # create the configuration file
guix system init /mnt/etc/config.scm /mnt
```

You can now reboot and enjoy your new Guix System!

### Installing on the same drive

Another option is to install the Guix System over the existing foreign distribution, replacing
it entirely.  Note that the root filesystem for the new Guix System is the current root filesystem,
so no need to mount it.  The following will initialize your system:

```bash
$EDITOR /etc/config.scm # create the configuration file
guix system init /etc/config.scm /
```

Make sure to remove the files from the old system.  You should at least get rid of the
old `/etc` directory, like so:

```bash
mv /etc{,.bak}
mkdir /etc
```

Make sure there is an empty /etc, or the new system won't boot properly.  You can
copy your config.scm to the new `/etc` directory.  You can now reboot and enjoy your
new Guix System!

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
