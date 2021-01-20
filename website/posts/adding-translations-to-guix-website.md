title: Adding translations to Guix’ website
date: 2021-01-20 12:00
author: Florian Pelz
tags: Community
---
As part of [GNU](https://www.gnu.org), Guix aims to bring freedom to
computer users all over the world, no matter the languages they
(prefer to) speak.  For example, Guix users asking for
[help](https://guix.gnu.org/help) can expect an answer even if they do
so in languages other than English.

We also offer translated software for people more comfortable with a
language other than English.  Thanks to many people who contribute
translations, GNU Guix and the packages it distributes can be used in
various languages, which we value greatly.  We are happy to announce
that Guix’ website can now be translated in the same manner.  If you
want to get a glimpse on how the translation process works, first from
a translator’s, then from a programmer’s perspective, read on.

The process for translators is kept simple.  Like lots of other free
software packages, Guix uses
[GNU Gettext](https://www.gnu.org/software/gettext) for its
translations, with which translatable strings are extracted from the
source code to so-called PO files.  If this is new to you, the magic
behind the translation process is best understood by taking a look at
one of them.
[Download](https://docs.weblate.org/en/latest/user/files.html) a PO
file for your language [at the Fedora Weblate
instance](https://translate.fedoraproject.org/projects/guix/).

Even though PO files are text files, changes should not be made with a
text editor but with PO editing software.  Weblate integrates PO
editing functionality.  Alternatively, translators can use any of
various free-software tools for filling in translations, of which
[Poedit](https://poedit.net) is one example, and (after logging in)
[upload](https://docs.weblate.org/en/latest/user/files.html) the
changed file.  There also is a [special PO editing
mode](https://www.emacswiki.org/emacs/PoMode) for users of
[GNU Emacs](https://www.gnu.org/software/emacs).  Over time
translators find out what software they are happy with and what
features they need.

Help with translations is much appreciated.  Since Guix integrates
with the wider free software ecosystem, if you intend to become a
translator, it is worth taking a look at the styleguides and the work
of other translators.  You will find some [at your language’s team at
the Translation Project (TP)](https://translationproject.org/team).

So much for the translation of ordinary source code.  With
[Po4a](https://po4a.org), we can also use Gettext’s tooling to
translate Guix’ [manual](https://guix.gnu.org/manual/) and
[cookbook](https://guix.gnu.org/cookbook/).  But all this was not true
of its web presence.  That’s why after a [lengthy
process,](https://issues.guix.info/issue/26302) the website of
GNU Guix has undergone an update.  It now supports translation into
other languages.  Such support is known as internationalization
(“i18n”).

Guix’ website is written in a variant of HTML (in which web pages are
usually written) that integrates better with the [Scheme programming
language](https://schemers.org).  Instead of XML tags, we use
[SXML](https://www.gnu.org/software/guile/manual/html_node/SXML.html).
This allows web authors to mix code and text.  It looks like this:

```scheme
`(section
  (h2 "On packaging")
  (p
   "Packages are "
   (a (@ (href ,(manual-url "Defining-Packages.html"))) "defined")
   " as native "
   (a (@ (href ,(gnu-url "software/guile"))) "Guile")
   " modules."))
```

However, this mixing makes it more difficult to extract the strings to
be translated.  We therefore cannot take the same approach as
[gnu.org](https://www.gnu.org), which uses a software package called
[GNUnited Nations](https://www.gnu.org/software/gnun) to extract from
pure HTML mark-up.  Translators are not always coders and we would
prefer to show them only the textual part, like this:

```
msgid "Packages are <1>defined</1> as native <2>Guile</2> modules."
```

Our new, custom i18n system does this.  The website authors need to
mark translatable expressions in the same way ordinary strings are
[usually marked in Guile Scheme
programs:](https://www.gnu.org/software/guile/manual/html_node/Gettext-Support.html#Gettext-Support)

```scheme
`(section
  ,(G_ `(h2 "On packaging"))
  ;; TRANSLATORS: Defining Packages is a section name
  ;; in the English (en) manual.
  ,(G_ `(p
         "Packages are "
         ,(G_ (manual-href "defined" (G_ "en") (G_ "Defining-Packages.html")))
         " as native "
         ,(G_ `(a (@ (href ,(gnu-url "software/guile"))) "Guile"))
         " modules.")))
```

Translators can arbitrarily change the ordering:

```
#. TRANSLATORS: Defining Packages is a section name
#. in the English (en) manual.
#: apps/base/templates/about.scm:64
msgid "Packages are <1>defined<1.1>en</1.1><1.2>Defining-Packages.html</1.2></1> as native <2>Guile</2> modules."
msgstr "Pakete werden als reine <2>Guile</2>-Module <1>definiert<1.1>de</1.1><1.2>Pakete-definieren.html</1.2></1>."
```

Details are [documented
here](https://git.savannah.gnu.org/cgit/guix/guix-artwork.git/tree/website/i18n-howto.txt).
We hope it strikes the right balance between simplicity for the
website’s developers and translator comfort.  Still missing is a way
to translate blog posts like the one you are reading here.

With ideas for and by a more diverse community, we can look forward to
a bright multi-lingual future.  Please get in touch with [your
language’s team](https://translationproject.org/team) or [us Guix
developers](https://guix.gnu.org/contact) if you want to help make
Guix’ website available in your language as well!

#### About GNU Guix

[GNU Guix](https://guix.gnu.org) is a transactional package manager and
an advanced distribution of the GNU system that [respects user
freedom](https://www.gnu.org/distros/free-system-distribution-guidelines.html).
Guix can be used on top of any system running the Hurd or the Linux
kernel, or it can be used as a standalone operating system distribution
for i686, x86_64, ARMv7, and AArch64 machines.

In addition to standard package management features, Guix supports
transactional upgrades and roll-backs, unprivileged package management,
per-user profiles, and garbage collection.  When used as a standalone
GNU/Linux distribution, Guix offers a declarative, stateless approach to
operating system configuration management.  Guix is highly customizable
and hackable through [Guile](https://www.gnu.org/software/guile)
programming interfaces and extensions to the
[Scheme](https://schemers.org) language.
