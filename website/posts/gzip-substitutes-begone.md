title: Sunsetting gzip substitutes availability
date: 2022-02-09 9:00
author: Maxim Cournoyer
tags: Build farm
---

Starting next month (2022/03/01), the build farm known as
[ci.guix.gnu.org](https://ci.guix.gnu.org/) will no longer offer
gzip-compressed binary substitutes. The Guix daemon has known to use
lzip for substitutes [since
2019](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=66229b04ae0ee05779b93d77900a062b8e0e8770);
unless you are running a very outdated daemon, you have no need to
worry about this change.

This idea was first discussed about a year ago, when it was found that
gzip-compressed substitutes [accounted for about only 1% of the
downloaded
substitutes](https://lists.gnu.org/archive/html/guix-devel/2021-03/msg00333.html).
Since then, the daemon [has gained support for
zstd](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=016299d85935cb269ae74c206c245ce23796160e)
on top of gzip and lzip, and the build farm has happily generated
compressed substitutes for all of these compression schemes.

While migrating the storage array of the Berlin-hosted build farm to
faster electronic storage, it was discovered that the cached
substitutes use about 16 TiB of storage space, which means they occupy
most of the new 22 TiB array, of which gzip substitutes account for
6.5 TiB:

```
$ du -sh /var/cache/guix/publish/{gzip,lzip,zstd}
6.5T    /var/cache/guix/publish/gzip
4.8T    /var/cache/guix/publish/lzip
4.0T    /var/cache/guix/publish/zstd
```

Letting go of gzip substitutes will allow such storage space to be
re-purposed to more pressing needs (such as a growing `/gnu/store`).
In the future, we may consider offering only zstd-compressed
substitutes, as zstd offers a competitive compression ratio compared
to lzip (as can be glimpsed above) while being faster to both compress
and decompress.
