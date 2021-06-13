title: Reproducible data processing pipelines
author: Ludovic Courtès
tags: Scheme API, Reproducibility, Talks
date: 2021-06-11 17:00:00
---

Last week, [we at Guix-HPC](https://hpc.guix.info) published [videos of
a workshop on reproducible software
environments](https://hpc.guix.info/events/2021/atelier-reproductibilit%C3%A9-environnements/)
we organized on-line.  The videos are well worth watching—especially if
you’re into reproducible research, and especially if you speak French or
want to practice.  This post, though, is more of a meta-post: it’s about
how we processed these videos.  “A workshop on reproducibility _ought to
have_ a reproducible video pipeline”, we thought.  So this is what we
[did](https://gitlab.inria.fr/guix-hpc/website/-/blob/master/doc/atelier-reproductibilit%C3%A9/render-videos.scm)!

# From BigBlueButton to WebM

Over the last year and half, perhaps you had the “opportunity” to
participate in an on-line conference, or even to organize one.  If so,
chances are that you already know
[BigBlueButton](https://bigbluebutton.org/) (BBB), the free software
video conferencing suite initially designed for on-line teaching.  In a
nutshell, it allows participants to chat (audio, video, and keyboard),
and speakers can share their screen or a PDF slide deck.  Organizers can
also record the session.

BBB then creates a link to recorded sessions with a custom JavaScript
player that replays everything: typed chat, audio and video (webcams),
shared screens, and slide decks.  This BBB replay a bit too rough though
and often not the thing you’d like to publish after the conference.
Instead, you’d rather do a bit of editing: adjusting the start and end
time of each talk, removing live chat from what’s displayed (which
allows you to remove info that personally identifies participants,
too!), and so forth.  Turns out this kind of post-processing is a bit of
work, primarily because BBB does “the right thing” of recording each
stream separately, in the most appropriate form: webcam and screen
shares are recorded as separate videos, chat is recorded as text with
timings, slide decks is recorded as a bunch of PNGs plus timings, and
then there’s a bunch of XML files with metadata putting it all together.

Anyway, with a bit of searching, we quickly found the handy
[bbb-render](https://github.com/plugorgau/bbb-render) tool, which can
first
[download](https://github.com/plugorgau/bbb-render/blob/master/download.py)
all these files and then
[assemble](https://github.com/plugorgau/bbb-render/blob/master/make-xges.py)
them using the Python interface to the [GStreamer Editing Services
(GES)](https://gstreamer.freedesktop.org/documentation/gst-editing-services/index.html).
Good thing: we don’t have to figure out all these things; we “just” have
to run these two scripts in an environment with the right dependencies.
And guess what: we know of a great tool to control execution
environments!

# A “deployment-aware Makefile”

So we have a process that takes input files—those PNGs, videos, and XML
files—and produces output files—WebM video files.  As developers we
immediately recognize a pattern and the timeless tool to deal with it:
[`make`](https://www.gnu.org/software/make).  The web already seems to
contain countless BBB post-processing makefiles (and shell scripts,
too).  We were going to contribute to this while we suddenly realized
that we know of _another_ great tool to express such processes: Guix!
Bonus: while a makefile would address just the tip of the
iceberg—running bbb-render—Guix can also take care of the tedious task
of deploying the _right_ environment to run bbb-render in.

What we did was to write some sort of a _deployment-aware makefile_.
It’s still a relatively unconventional way to use Guix, but one that’s
very convenient.  We’re talking about videos, but really, you could use
the same approach for any kind of processing graph where you’d be
tempted to just use `make`.

The end result here is a [Guix
file](https://gitlab.inria.fr/guix-hpc/website/-/blob/6977da4618814c790e767618da5cf9ec2cab0742/doc/atelier-reproductibilit%C3%A9/render-videos.scm)
that returns a _manifest_—a list of videos to “build”.  You can build
the videos with:

```
guix build -m render-videos.scm
```

Overall, the file defines a bunch of functions (_procedures_ in
traditional Scheme parlance), each of which takes input files and
produces output files.  More accurately, these functions returns objects
that _describe_ how to build their output from the input files—similar
to how a [makefile
rule](https://www.gnu.org/software/make/manual/html_node/Rule-Introduction.html)
describes how to build its target(s) from its prerequisite(s).  (The
reader familiar with functional programming may recognize a monad here,
and indeed, those build descriptions can be thought of as monadic values
in a hypothetical “Guix build” monad; technically though, they’re
regular Scheme values.)

Let’s take a guided tour of this 300-line file.

# Rendering

The [first
step](https://gitlab.inria.fr/guix-hpc/website/-/blob/6977da4618814c790e767618da5cf9ec2cab0742/doc/atelier-reproductibilit%C3%A9/render-videos.scm#L23-75)
in this file describes where bbb-render can be found and how to run it
to produce a GES “project” file, which we’ll use later to render the
video:

```scheme
(define bbb-render
  (origin
    (method git-fetch)
    (uri (git-reference (url "https://github.com/plugorgau/bbb-render")
                        (commit "a3c10518aedc1bd9e2b71a4af54903adf1d972e5")))
    (file-name "bbb-render-checkout")
    (sha256
     (base32 "1sf99xp334aa0qgp99byvh8k39kc88al8l2wy77zx7fyvknxjy98"))))

(define rendering-profile
  (profile
   (content (specifications->manifest
             '("gstreamer" "gst-editing-services" "gobject-introspection"
               "gst-plugins-base" "gst-plugins-good"
               "python-wrapper" "python-pygobject" "python-intervaltree")))))

(define* (video-ges-project bbb-data start end
                            #:key (webcam-size 25))
  "Return a GStreamer Editing Services (GES) project for the video,
starting at START seconds and ending at END seconds.  BBB-DATA is the raw
BigBlueButton directory as fetched by bbb-render's 'download.py' script.
WEBCAM-SIZE is the percentage of the screen occupied by the webcam."
  (computed-file "video.ges"
                 (with-extensions (list (specification->package "guile-gcrypt"))
                  (with-imported-modules (source-module-closure
                                          '((guix build utils)
                                            (guix profiles)))
                    #~(begin
                        (use-modules (guix build utils) (guix profiles)
                                     (guix search-paths) (ice-9 match))

                        (define search-paths
                          (profile-search-paths #+rendering-profile))

                        (for-each (match-lambda
                                    ((spec . value)
                                     (setenv
                                      (search-path-specification-variable
                                       spec)
                                      value)))
                                  search-paths)

                        (invoke "python"
                                #+(file-append bbb-render "/make-xges.py")
                                #+bbb-data #$output
                                "--start" #$(number->string start)
                                "--end" #$(number->string end)
                                "--webcam-size"
                                #$(number->string webcam-size)))))))
```

First it defines the source code location of bbb-render as an
[“origin”](https://guix.gnu.org/manual/en/html_node/origin-Reference.html).
Second, it defines `rendering-profile` as a
[“profile”](https://guix.gnu.org/manual/en/html_node/Getting-Started.html#index-profile)
containing all the packages needed to run bbb-render’s `make-xges.py`
script.  The `specification->manifest` procedure creates a _manifest_
from a set of packages specs, and likewise `specification->package`
returns the package that matches a given spec.  You can try these things at
the [`guix
repl`](https://guix.gnu.org/manual/en/html_node/Invoking-guix-repl.html)
prompt:

```
$ guix repl
GNU Guile 3.0.7
Copyright (C) 1995-2021 Free Software Foundation, Inc.

Guile comes with ABSOLUTELY NO WARRANTY; for details type `,show w'.
This program is free software, and you are welcome to redistribute it
under certain conditions; type `,show c' for details.

Enter `,help' for help.
scheme@(guix-user)> ,use(guix profiles)
scheme@(guix-user)> ,use(gnu)
scheme@(guix-user)> (specification->package "guile@2.0")
$1 = #<package guile@2.0.14 gnu/packages/guile.scm:139 7f416be776e0>
scheme@(guix-user)> (specifications->manifest '("guile" "gstreamer" "python"))
$2 = #<<manifest> entries: (#<<manifest-entry> name: "guile" version: "3.0.7" …> #<<manifest-entry> name: "gstreamer" version: "1.18.2" …> …)
```

Last, it defines `video-ges-project` as a function that takes the BBB
raw data, a start and end time, and produces a `video.ges` file.  There
are three key elements here:

  1. [`computed-file`](https://guix.gnu.org/manual/en/html_node/G_002dExpressions.html#index-computed_002dfile)
     is a function to produce a file, `video.ges` in this case, by
     running the code you give it as its second argument—the *recipe*,
     in makefile terms.
  2. The recipe passed to `computed-file` is a
     [_G-expression_](https://guix.gnu.org/manual/en/html_node/G_002dExpressions.html)
     (or “gexp”), introduced by this fancy `#~` (hash tilde) notation.
     G-expressions are a way to _stage_ code, to mark it for eventual
     execution.  Indeed, that code will only be executed if and when we
     run `guix build` (without `--dry-run`), and only if the result is
     not already in [the
     store](https://guix.gnu.org/manual/en/html_node/The-Store.html).
  3. The gexp refers to `rendering-profile`, to `bbb-render`, to
     `bbb-data` and so on by _escaping_ with the `#+` or `#$` syntax
     (they’re equivalent, unless doing cross-compilation).  During
     build, these reference items in the store, such as
     `/gnu/store/…-bbb-render`, which is itself the result of “building”
     the origin we’ve seen above.  The `#$output` reference corresponds
     to the build result of this `computed-file`, the complete file name
     of `video.ges` under `/gnu/store`.

That’s quite a lot already!  Of course, this real-world example is
more intimidating than the toy examples you’d find in the manual, but
really, pretty much everything’s there.  Let’s see in more detail at
what’s inside this gexp.

The gexp first imports a bunch of helper modules with [build
utilities](https://guix.gnu.org/manual/en/html_node/Build-Utilities.html)
and tools to manipulate profiles and search path environment variables.
The `for-each` call iterates over search path environment
variables—`PATH`, `PYTHONPATH`, and so on—, setting them so that the
`python` command is found and so that the needed Python modules are
found.

The `with-imported-modules` form above indicates that the `(guix build
utils)` and `(guix profiles)` modules, which are part of Guix, along
with their dependencies (their _closure_), need to be imported in the
build environment.  What about `with-extensions`?  Those `(guix …)`
module indirectly depend on additional modules, provided by the
`guile-gcrypt` package, hence this spec.

Next comes the
[`ges->webm`](https://gitlab.inria.fr/guix-hpc/website/-/blob/6977da4618814c790e767618da5cf9ec2cab0742/doc/atelier-reproductibilit%C3%A9/render-videos.scm#L77-106)
function which, as the name implies, takes a `.ges` file and produces a
WebM video file by invoking `ges-launch-1.0`.  The end result is a video
containing the recording’s audio, the webcam and screen share (or slide
deck), but not the chat.

# Opening and closing

We have a WebM video, so we’re pretty much done, right?  But… we’d also
like to have an opening, showing the talk title and the speaker’s name,
as well as a closing.  How do we get that done?

Perhaps a bit of a sledgehammer, but it turns out that we chose to
produce those still images with LaTeX/Beamer, from
[these](https://gitlab.inria.fr/guix-hpc/website/-/blob/6977da4618814c790e767618da5cf9ec2cab0742/doc/atelier-reproductibilit%C3%A9/opening.tex)
[templates](https://gitlab.inria.fr/guix-hpc/website/-/blob/6977da4618814c790e767618da5cf9ec2cab0742/doc/atelier-reproductibilit%C3%A9/closing.tex).

We need again several processing steps:

  1. We first define the
     [`latex->pdf`](https://gitlab.inria.fr/guix-hpc/website/-/blob/6977da4618814c790e767618da5cf9ec2cab0742/doc/atelier-reproductibilit%C3%A9/render-videos.scm#L140-166)
     function that takes a template `.tex` file, a speaker name and
     title.  It copies the template, replaces placeholders with the
     speaker name and title, and runs `pdflatex` to produce the PDF.
  2. The
     [`pdf->bitmap`](https://gitlab.inria.fr/guix-hpc/website/-/blob/6977da4618814c790e767618da5cf9ec2cab0742/doc/atelier-reproductibilit%C3%A9/render-videos.scm#L168-175)
     function takes a PDF and returns a suitably-sized JPEG.
  3. [`image->webm`](https://gitlab.inria.fr/guix-hpc/website/-/blob/6977da4618814c790e767618da5cf9ec2cab0742/doc/atelier-reproductibilit%C3%A9/render-videos.scm#L177-200)
     takes that JPEG and invokes `ffmpeg` to render it as WebM, with the
     right resolution, frame rate, and audio track.
	 
With that in place, we define a sweet and small function that produces
the opening WebM file for a given talk:

```scheme
(define (opening title speaker)
  (image->webm
   (pdf->bitmap (latex->pdf (local-file "opening.tex") "opening.pdf"
                            #:title title #:speaker speaker)
                "opening.jpg")
   "opening.webm" #:duration 5))
```

We need one last function,
[`video-with-opening/closing`](https://gitlab.inria.fr/guix-hpc/website/-/blob/6977da4618814c790e767618da5cf9ec2cab0742/doc/atelier-reproductibilit%C3%A9/render-videos.scm#L216-236),
that given a talk, an opening, and a closing, concatenates them by
invoking `ffmpeg`.

# Putting it all together

Now we have all the building blocks!

We use
[`local-file`](https://guix.gnu.org/manual/en/html_node/G_002dExpressions.html#index-local_002dfile)
to refer to the raw BBB data, taken from disk:

```scheme
(define raw-bbb-data/monday
  ;; The raw BigBlueButton data as returned by './download.py URL', where
  ;; 'download.py' is part of bbb-render.
  (local-file "bbb-video-data.monday" "bbb-video-data"
              #:recursive? #t))

(define raw-bbb-data/tuesday
  (local-file "bbb-video-data.tuesday" "bbb-video-data"
              #:recursive? #t))
```

No, the raw data is not in the Git repository (it’s too big and contains
personally-identifying information about participants), so this assumes
that there’s a `bbb-video-data.monday` and a `bbb-video-data.tuesday` in
the same directory as `render-videos.scm`.

For good measure, we define a
[`<talk>`](https://gitlab.inria.fr/guix-hpc/website/-/blob/6977da4618814c790e767618da5cf9ec2cab0742/doc/atelier-reproductibilit%C3%A9/render-videos.scm#L243-251)
data type:

```scheme
(define-record-type <talk>
  (talk title speaker start end cam-size data)
  talk?
  (title     talk-title)
  (speaker   talk-speaker)
  (start     talk-start)           ;start time in seconds
  (end       talk-end)             ;end time
  (cam-size  talk-webcam-size)     ;percentage used for the webcam
  (data      talk-bbb-data))       ;BigBlueButton data
```

… such that we can easily [define
talks](https://gitlab.inria.fr/guix-hpc/website/-/blob/6977da4618814c790e767618da5cf9ec2cab0742/doc/atelier-reproductibilit%C3%A9/render-videos.scm#L263-288),
along with
[`talk->video`](https://gitlab.inria.fr/guix-hpc/website/-/blob/6977da4618814c790e767618da5cf9ec2cab0742/doc/atelier-reproductibilit%C3%A9/render-videos.scm#L297-311),
which takes a talk and return a complete, final video:

```scheme
(define (talk->video talk)
  "Given a talk, return a complete video, with opening and closing."
  (define file-name
    (string-append (canonicalize-string (talk-speaker talk))
                   ".webm"))

  (let ((raw (ges->webm (video-ges-project (talk-bbb-data talk)
                                           (talk-start talk)
                                           (talk-end talk)
                                           #:webcam-size
                                           (talk-webcam-size talk))
                        file-name))
        (opening (opening (talk-title talk) (talk-speaker talk))))
    (video-with-opening/closing file-name raw
                                opening closing.webm)))
```

The [very last
bit](https://gitlab.inria.fr/guix-hpc/website/-/blob/6977da4618814c790e767618da5cf9ec2cab0742/doc/atelier-reproductibilit%C3%A9/render-videos.scm#L313-319)
iterates over the talks and returns a manifest containing all the final
videos.  Now we can build the ready-to-be-published videos, all at once:

```
$ guix build -m render-videos.scm
[… time passes…]
/gnu/store/…-emmanuel-agullo.webm
/gnu/store/…-francois-rue.webm
…
```

[Voilà!](https://hpc.guix.info/events/2021/atelier-reproductibilité-environnements/)

![Image of an old TV screen showing a video opening.](/static/blog/img/2021-video-tv-screen.png)

# Why all the fuss?

OK, maybe you’re thinking “this is just another hackish script to fiddle
with videos”, and that’s right!  It’s also worth mentioning another
approach: [Racket’s video language](https://lang.video/), which is
designed to manipulate video abstractions, similar to GES but with a
sweet high-level functional interface.

But look, this one’s different: it’s
self-contained, it’s reproducible, and it has the right abstraction
level.  Self-contained is a big thing; it means you can run it and it
knows what software to deploy, what environment variables to set, and so
on, for each step of the pipeline.  Granted, it could be simplified with
appropriate high-level interfaces in Guix.  But remember: the
alternative is a makefile (“deployment-unaware”) completed by a `README`
file giving a vague idea of the dependencies needed.  The reproducible
bit is pretty nice too (especially for a workshop _on_ reproducibility).
It also means there’s caching: videos or intermediate byproducts already
in the store don’t need to be recomputed.  Last, we have access to a
general-purpose programming language where we can _build abstractions_,
such as the `<talk>` data type, that makes the whole thing more pleasant
to work with and more maintainable.

Hopefully that’ll inspire you to have a reproducible video pipeline for
your next on-line event, or maybe that’ll inspire you to replace your
old makefile and shelly habits for data processing!

High-performance computing (HPC) people might be wondering how to go
from here and build “computing-resource-aware” or
“storage-resource-aware” pipelines where each computing step could be
submitted to the job scheduler of an HPC cluster and use distributed
file systems for intermediate results rather than `/gnu/store`.  If
you’re one of these folks, do take a look at how the [Guix Workflow
Language](https://guixwl.org/) addresses these issues.

# Acknowledgments

Thanks to Konrad Hinsen for valuable feedback on an earlier draft.


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
