title: Running a Ganeti cluster on Guix
date: 2020-07-17 15:00
author: Marius Bakke
tags: Virtualization, Devops
---
The [latest addition](https://guix.gnu.org/manual/devel/en/guix.html#index-ganeti)
to Guix's ever-growing list of services is a little-known virtualization toolkit
called [Ganeti](http://www.ganeti.org/).  Ganeti is designed to keep virtual
machines running on a cluster of servers even in the event of hardware failures,
and to make maintenance and recovery tasks easy.

It is comparable to tools such as
[Proxmox](https://www.proxmox.com/en/proxmox-ve) or
[oVirt](https://www.ovirt.org/), but has some distinctive features.  One is
that there is no GUI: [third](https://github.com/osuosl/ganeti_webmgr)
[party](https://github.com/grnet/ganetimgr)
[ones](https://github.com/sipgate/ganeti-control-center) exist, but are not
currently packaged in Guix, so you are left with a rich command-line client
and a fully featured
[remote API](http://docs.ganeti.org/ganeti/master/html/rapi.html).

Another interesting feature is that installing Ganeti on its own leaves you
no way to actually deploy any virtual machines.  That probably sounds crazy,
but stems from the fact that Ganeti is designed to be API-driven and automated,
thus it comes with a
[OS API](http://docs.ganeti.org/ganeti/master/html/man-ganeti-os-interface.html)
and users need to install one or more *OS providers* in addition to Ganeti.
OS providers offer a declarative way to deploy virtual machine variants and
should feel natural to Guix users.  At the time of writing, the providers
available in Guix are [debootstrap](https://github.com/ganeti/instance-debootstrap)
for provisioning Debian- and Ubuntu-based VMs, and of course a
[Guix](https://github.com/mbakke/ganeti-instance-guix) provider.

Finally Ganeti comes with a sophisticated _instance allocation framework_ that
efficiently packs virtual machines across a cluster while maintaining N+1
redundancy in case of a failover scenario.  It can also make informed scheduling
decisions based on various cluster tags, such as ensuring primary and secondary
nodes are on different power distribution lines.

(Note: if you are looking for a way to run just a few virtual machines on
your local computer, you are probably better off using
[libvirt](https://guix.gnu.org/manual/en/guix.html#index-libvirt) or even a
[Childhurd](https://guix.gnu.org/manual/devel/en/guix.html#index-hurd-1)
as Ganeti is fairly heavyweight and requires a complicated networking setup.)


# Preparing the configuration

With introductions out of the way, let's see how we can deploy a Ganeti
cluster using Guix.  For this tutorial we will create a two-node cluster
and connect instances to the local network using an
[Open vSwitch](https://www.openvswitch.org/) bridge with no VLANs.  We assume
that each node has a single network interface named `eth0` connected to the
same network, and that a dedicated partition `/dev/sdz3` is available for
virtual machine storage.  It is possible to store VMs on a number of other
storage backends, but a dedicated drive (or rather LVM volume group) is
necessary to use the [DRBD](https://www.linbit.com/drbd/) integration to
replicate VM disks.

We'll start off by defining a few helper services to create the Open vSwitch
bridge and ensure the physical network interface is in the "up" state.  Since
Open vSwich stores the configuration in a database, you might as well run the
equivalent `ovs-vsctl` commands on the host once and be done with it, but we
do it through the configuration system to ensure we don't forget it in the
future when adding or reinstalling nodes.

```scheme
(use-modules (gnu)
             (gnu packages linux)
             (gnu packages networking)
             (gnu services shepherd))

(define (start-interface if)
  #~(let ((ip #$(file-append iproute "/sbin/ip")))
      (invoke/quiet ip "link" "set" #$if "up")))

(define (stop-interface if)
  #~(let ((ip #$(file-append iproute "/sbin/ip")))
      (invoke/quiet ip "link" "set" #$if "down")))

;; This service is necessary to ensure eth0 is in the "up" state on boot
;; since it is otherwise unmanaged from Guix PoV.
(define (ifup-service if)
  (let ((name (string-append "ifup-" if)))
    (simple-service name shepherd-root-service-type
                    (list (shepherd-service
                           (provision (list (string->symbol name)))
                           (start #~(lambda ()
                                      #$(start-interface if)))
                           (stop #~(lambda (_)
                                     #$(stop-interface if)))
                           (respawn? #f))))))

;; Note: Remove vlan_mode to use tagged VLANs.
(define (create-openvswitch-bridge bridge uplink)
  #~(let ((ovs-vsctl (lambda (cmd)
                       (apply invoke/quiet
                              #$(file-append openvswitch "/bin/ovs-vsctl")
                              (string-tokenize cmd)))))
      (and (ovs-vsctl (string-append "--may-exist add-br " #$bridge))
           (ovs-vsctl (string-append "--may-exist add-port " #$bridge " "
                                     #$uplink
                                     " vlan_mode=native-untagged")))))

(define (create-openvswitch-internal-port bridge port)
  #~(invoke/quiet #$(file-append openvswitch "/bin/ovs-vsctl")
                  "--may-exist" "add-port" #$bridge #$port
                  "vlan_mode=native-untagged"
                  "--" "set" "Interface" #$port "type=internal"))

(define %openvswitch-configuration-service
  (simple-service 'openvswitch-configuration shepherd-root-service-type
                  (list (shepherd-service
                         (provision '(openvswitch-configuration))
                         (requirement '(vswitchd))
                         (start #~(lambda ()
                                    #$(create-openvswitch-bridge
                                       "br0" "eth0")
                                    #$(create-openvswitch-internal-port
                                       "br0" "gnt0")))
                         (respawn? #f)))))
```

This defines a `openvswitch-configuration` service object that creates a
logical switch `br0`, connects `eth0` as the "uplink", and creates a logical
port `gnt0` that we will use later as the main network interface for this
system.  We also create an `ifup` service that can bring network interfaces
up and down.  By themselves these variables do nothing, we also have to add
them to our `operating-system` configuration below.

Such a configuration might be suitable for a small home network.  In a
datacenter deployment you would likely use tagged VLANs, and maybe a traditional
Linux bridge instead of Open vSwitch.  You can also forego bridging altogether
with a `routed` networking setup, or do any combination of the three.

With this in place, we can start creating the `operating-system` configuration
that we will use for the Ganeti servers:

```scheme
;; [continued from the above configuration snippet]

(use-service-modules base ganeti linux networking ssh)

(operating-system
  (host-name "node1")
  [...]
  ;; Ganeti requires that each node and the cluster name resolves to an
  ;; IP address.  The easiest way to achieve this is by adding everything
  ;; to the hosts file.
  (hosts-file (plain-file "hosts" "
127.0.0.1       localhost
::1             localhost

192.168.1.200   ganeti.lan
192.168.1.201   node1
192.168.1.202   node2
"))
  (kernel-arguments
   (append %default-kernel-arguments
           '(;; Disable DRBDs usermode helper, as Ganeti is the only entity
             ;; that should manage DRBD.
             "drbd.usermode_helper=/run/current-system/profile/bin/true")))

  (packages (append (map specification->package
                         '("qemu" "drbd-utils" "lvm2"
                           "ganeti-instance-guix"
                           "ganeti-instance-debootstrap"))
                    %base-packages))

  (services (cons* (service ganeti-service-type
                            (ganeti-configuration
                             (file-storage-paths '("/srv/ganeti/file-storage"))
                             (os
                              (list (guix-os %default-guix-variants)
                                    (debootstrap-os
                                     (list (debootstrap-variant
                                            "buster"
                                            (debootstrap-configuration
                                             (suite "buster")))
                                           (debootstrap-variant
                                            "testing+contrib+paravirtualized"
                                            (debootstrap-configuration
                                             (suite "testing")
                                             (hooks
                                              (local-file
                                               "paravirt-hooks"
                                               #:recursive? #t))
                                             (extra-pkgs
                                              (delete "linux-image-amd64"
                                                      %default-debootstrap-extra-pkgs))
                                             (components '("main" "contrib"))))))))))

                   ;; Ensure the DRBD kernel module is loaded.
                   (service kernel-module-loader-service-type
                            '("drbd"))

                   ;; Create a static IP on the "gnt0" Open vSwitch interface.
                   (service openvswitch-service-type)
                   %openvswitch-configuration-service
                   (ifup-service "eth0")
                   (static-networking-service "gnt0" "192.168.1.201"
                                              #:netmask "255.255.255.0"
                                              #:gateway "192.168.1.1"
                                              #:requirement '(openvswitch-configuration)
                                              #:name-servers '("192.168.1.1"))

                   (service openssh-service-type
                            (openssh-configuration
                             (permit-root-login 'without-password)))
                   %base-services)))
```

Here we declare two OS "variants" for the debootstrap OS provider.  Debootstrap
variants rely on a set of scripts (known as "hooks") in the installation process
to do things like configure networking, install bootloader, create users, etc.
In the example above, the "buster" variant uses the default hooks provided by
Guix which configures network and GRUB, whereas the "testing+contrib+paravirtualized"
variant use a local directory next to the configuration file named "paravirt-hooks"
(it is copied into the final system closure).

We also declare a default `guix-os` variant provided by Guix's Ganeti service.

Ganeti veterans may be surprised that each OS variant has its own hooks.  The
Ganeti deployments I've seen use a single set of hooks for all variants,
sometimes with additional logic inside the script based on the variant.  Guix
offers a powerful abstraction that makes it trivial to create per-variant hooks,
obsoleting the need for a big `/etc/ganeti/instance-debootstrap/hooks` directory.
Of course you can still create it if you wish and set the `hooks` property of the
variants to `#f`.

Not all Ganeti options are exposed in the configuration system yet.  If you
find it limiting, you can add custom files using `extra-special-file`, or
ideally extend the `<ganeti-configuration>` data type to suite your needs.
You can also use `gnt-cluster copyfile` and `gnt-cluster command` to distribute
files or run executables, but undeclared changes in `/etc` may be lost on the
next reboot or reconfigure.

# Initializing a cluster

At this stage, you should run `guix system reconfigure` with the new
configuration on all nodes that will participate in the cluster.  If you
do this over SSH or with
[guix deploy](https://guix.gnu.org/blog/2019/managing-servers-with-gnu-guix-a-tutorial/),
beware that `eth0` will lose network connectivity once it is "plugged in to"
the virtual switch, and you need to add any IP configuration to `gnt0`.

The Guix configuration system does not currently support declaring LVM
volume groups, so we will create these manually on each node.  We could
write our own declarative configuration like the `%openvswitch-configuration-service`,
but for brevity and safety reasons we'll do it "by hand":

```
pvcreate /dev/sdz3
vgcreate ganetivg /dev/sdz3
```

On the node that will act as the "master node", run the init command:

**Warning**: this will create new SSH keypairs, both host keys and for the root
user!  You can prevent that by adding `--no-ssh-init`, but then you will need
to distribute `/var/lib/ganeti/known_hosts` to all hosts, and authorize the
Ganeti key for the root user in `openssh-configuration`.  Here we let Ganeti
manage the keys for simplicity.  As a bonus, we can automatically rotate the
cluster keys in the future using `gnt-cluster renew-crypto --new-ssh-keys`.

```
gnt-cluster init \
    --master-netdev=gnt0 \
    --vg-name=ganetivg \
    --enabled-disk-templates=file,plain,drbd \
    --drbd-usermode-helper=/run/current-system/profile/bin/true \
    --enabled-hypervisors=kvm \
    --hypervisor-parameters=kvm:kvm_flag=enabled \
    --nic-parameters=mode=openvswitch,link=br0 \
    --no-etc-hosts \
    ganeti.lan
```

`--no-etc-hosts` prevents Ganeti from automatically updating the `/etc/hosts`
file when nodes are added or removed, which makes little sense on Guix because
it is recreated every reboot/reconfigure.

See the [gnt-cluster manual](http://docs.ganeti.org/ganeti/master/man/gnt-cluster.html)
for information on the available options.  Most can be changed at runtime with
`gnt-cluster modify`.

If all goes well, the command returns no output and you should have the
`ganeti.lan` IP address visible on `gnt0`.  You can run `gnt-cluster verify`
to check that the cluster is in good shape.  Most likely it complains about
something:

```
root@node1 ~# gnt-cluster verify
Submitted jobs 3, 4
Waiting for job 3 ...
Thu Jul 16 18:26:34 2020 * Verifying cluster config
Thu Jul 16 18:26:34 2020 * Verifying cluster certificate files
Thu Jul 16 18:26:34 2020 * Verifying hypervisor parameters
Thu Jul 16 18:26:34 2020 * Verifying all nodes belong to an existing group
Waiting for job 4 ...
Thu Jul 16 18:26:34 2020 * Verifying group 'default'
Thu Jul 16 18:26:34 2020 * Gathering data (1 nodes)
Thu Jul 16 18:26:34 2020 * Gathering information about nodes (1 nodes)
Thu Jul 16 18:26:35 2020 * Gathering disk information (1 nodes)
Thu Jul 16 18:26:35 2020 * Verifying configuration file consistency
Thu Jul 16 18:26:35 2020 * Verifying node status
Thu Jul 16 18:26:35 2020   - ERROR: node node1: hypervisor kvm parameter verify failure (source cluster): Parameter 'kernel_path' fails validation: not found or not a file (current value: '/boot/vmlinuz-3-kvmU')
Thu Jul 16 18:26:35 2020 * Verifying instance status
Thu Jul 16 18:26:35 2020 * Verifying orphan volumes
Thu Jul 16 18:26:35 2020 * Verifying N+1 Memory redundancy
Thu Jul 16 18:26:35 2020 * Other Notes
Thu Jul 16 18:26:35 2020 * Hooks Results
```

When using the KVM hypervisor, Ganeti expects to find a dedicated kernel
image for virtual machines in `/boot`.  For this tutorial we only use fully
virtualized instances (meaning each VM runs its own kernel), so we can set
`kernel_path` to an empty string to make the warning disappear:

```
gnt-cluster modify -H kvm:kernel_path=
```

Now let's add our other machine to the cluster:

```
gnt-node add node2
```

Ganeti will log into the node, copy the cluster configuration and start the
relevant Shepherd services.  You may need to authorize node1's SSH key first.
Run `gnt-cluster verify` again to check that everything is in order:

```
gnt-cluster verify
```

If you used `--no-ssh-init` earlier you will likely get SSH host key warnings here.
In that case you should update `/var/lib/ganeti/known_hosts` with the new node
information, and distribute it with `gnt-cluster copyfile` or by adding it to the
OS configuration.

The above configuration will make three operating systems available:

```
# gnt-os list
Name
debootstrap+buster
debootstrap+testing+contrib+paravirtualized
guix+default
```

Let's try them out.  But first we'll make Ganeti aware of our network
so it can choose a static IP for the virtual machines.

```
# gnt-network add --network=192.168.1.0/24 --gateway=192.168.1.1 lan
# gnt-network connect -N mode=openvswitch,link=br0 lan
```

Now we can add an instance:

```
root@node1 ~# gnt-instance add --no-name-check --no-ip-check \
    -o debootstrap+buster -t drbd --disk 0:size=5G \
    --net 0:network=lan,ip=pool bustervm1
Thu Jul 16 18:28:58 2020  - INFO: Selected nodes for instance bustervm1 via iallocator hail: node1, node2
Thu Jul 16 18:28:58 2020  - INFO: NIC/0 inherits netparams ['br0', 'openvswitch', '']
Thu Jul 16 18:28:58 2020  - INFO: Chose IP 192.168.1.2 from network lan
Thu Jul 16 18:28:58 2020 * creating instance disks...
Thu Jul 16 18:29:03 2020 adding instance bustervm1 to cluster config
Thu Jul 16 18:29:03 2020 adding disks to cluster config
Thu Jul 16 18:29:03 2020  - INFO: Waiting for instance bustervm1 to sync disks
Thu Jul 16 18:29:03 2020  - INFO: - device disk/0:  0.60% done, 5m 26s remaining (estimated)
[...]
Thu Jul 16 18:31:08 2020  - INFO: - device disk/0: 100.00% done, 0s remaining (estimated)
Thu Jul 16 18:31:08 2020  - INFO: Instance bustervm1's disks are in sync
Thu Jul 16 18:31:08 2020  - INFO: Waiting for instance bustervm1 to sync disks
Thu Jul 16 18:31:08 2020  - INFO: Instance bustervm1's disks are in sync
Thu Jul 16 18:31:08 2020 * running the instance OS create scripts...
Thu Jul 16 18:32:09 2020 * starting instance...
```

Ganeti will automatically select the optimal primary and secondary node
for this VM based on available cluster resources.  You can manually
specify primary and secondary nodes with the `-n` and `-s` options.

By default Ganeti assumes that the new instance is already configured in DNS,
so we need `--no-name-check` and `--no-ip-check` to bypass some sanity tests.

Try adding another instance, now using the Guix OS provider with the 'plain'
(LVM) disk backend:

```
gnt-instance add --no-name-check --no-ip-check -o guix+default \
    -t plain --disk 0:size=5G -B memory=1G,vcpus=2 \
    --net 0:network=lan,ip=pool \
    guix1
```

The `guix+default` variant has a configuration that starts an SSH server and
authorizes the hosts SSH key, and configures static networking based on information
from Ganeti.  To use other configuration files, you should declare variants with
the config file as the `configuration` property.  The Guix provider also supports
"OS parameters" that lets you specify a specific Guix commit or branch:

```
gnt-instance add --no-name-check --no-ip-check \
    -o guix+gnome -O "commit=<commit>" \
    -H kvm:spice_bind=0.0.0.0,cpu_type=host \
    -t file --file-storage-dir=/srv/ganeti/file-storage \
    --disk 0:size=20G -B minmem=1G,maxmem=6G,vcpus=3 \
    --net 0:network=lan,ip=pool -n node1 \
    guix2
```

You can connect to a VM serial console using `gnt-instance console <instance>`.
For this last VM we used a hypothetical 'guix+gnome' variant, and added a
graphical [SPICE](https://spice-space.org/) console that you can connect to
remotely using the `spicy` command.

If you are new to Ganeti, the next steps is to familiarize yourself with the
`gnt-` family commands.  Fun stuff to do include `gnt-instance migrate` to move
VMs between hosts, `gnt-node evacuate` to migrate _all_ VMs off a node, and
`gnt-cluster master-failover` to move the master role to a different node.

If you wish to start over for any reason, you can use `gnt-cluster destroy`.


# Final remarks

The declarative nature of Guix maps well to Ganetis OS API.  OS variants can be
composed and inherit from each other, something that is not easily achieved with
traditional configuration management tools.  The author had a lot of fun creating
[native data types](https://guix.gnu.org/manual/devel/en/guix.html#index-debootstrap_002dconfiguration)
in the Guix configuration system for Ganetis OS configuration, and it made me
wonder whether other parts of Ganeti could be made declarative such as aspects
of instance and cluster configuration.  In any case I'm happy and excited to
finally be able to use Guix as a Ganeti host OS.

Like most services in Guix, Ganeti comes with a
[system test](https://guix.gnu.org/blog/2016/guixsd-system-tests/)
that runs in a VM and ensures that things like initializing a cluster work.
The continuous integration system
[runs this automatically](https://ci.guix.gnu.org/search?query=ganeti) whenever
a dependency is updated, and provides comfort that both the package and service
is in a good shape.  Currently it has
[rudimentary service tests](https://git.savannah.gnu.org/cgit/guix.git/tree/gnu/tests/ganeti.scm#n117),
but it can conceivably be extended to provision a real cluster inside Ganeti
and try things like master-failover and live migration.

So far only the KVM hypervisor has been tested.  If you use LXC or Xen with
Ganeti, please reach out to `guix-devel@gnu.org` and share your experience.

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
