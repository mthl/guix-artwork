title: Improve Internationalization Support for the Guix Data Service
date: 2020-07-23 12:00
author: Danjela Lura
tags: Outreachy, Guix Data Service
---

The first half of my [Outreachy](https://www.outreachy.org/)
internship is already over and I am really excited to share my
experience. Over the past weeks I’ve had the opportunity to work on
the [Guix Data Service](https://data.guix.gnu.org/), watch myself
change, and accomplish way more than I thought I would.

The Guix Data Service processes, stores and provides data about Guix
over time. It provides a complementary interface to Guix itself by
having a web interface and API to browse and access the data.

The work I have done so far revolves around storing translated [lint
checker descriptions][guix-lint-docs] as well as package synopsis and
descriptions in the Guix Data Service PostgreSQL database and making
them available through the Guix Data Service web interface.

[guix-lint-docs]: https://guix.gnu.org/manual/en/html_node/Invoking-guix-lint.html

Initially the Guix Data Service database had translated versions of
lint warning messages available, but they were not accessible through
the web interface, so I made that possible during the [contribution
period](https://www.outreachy.org/docs/applicant/#make-contributions).

Working on making lint warning messages available on the web interface
made it easier for me to understand how translations for lint checker
descriptions and package synopsis and descriptions would be stored in
the database and later on be made available through the Guix Data
Service web interface.  At this point, the Guix Data Service supports
package synopsis and descriptions as well as lint checker descriptions
in various locales.

![Guix Data Service page for the audacity package, in the Spanish
locale](/static/blog/img/guix-data-service-audacity.png)

Hopefully these changes will provide the Guix Data Service users with
a more feasible way to interact with Guix data.

I have to note that this is my first internship and I was initially
reluctant to believe that I would be able to tackle or successfully
accomplish the tasks I was assigned, but with my mentor’s help and
guidance I managed to.  So far it has been a rewarding experience
because it has helped me make progress in so many aspects, whilst
contributing to a project that will potentially increase inclusion.

While working on this project, I’ve significantly improved my Guile,
SQL, and Git skills and I am now more aware of how software
localization is achieved.  In addition to getting more technically
skilled, this internship has taught me how to manage time and emotions
when dealing with more than one activity at a time.

Now that a good share of what was initially planned to be done is
accomplished, my mentor suggested working on something using the Guix
Data Service data and I will be engaged in that during the remaining
half.

These first 7 weeks of my internship have gone by really fast, but I
have enjoyed everything and I am so eager to experience what's to
come.
