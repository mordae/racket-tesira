#lang scribble/manual

@title{Tesira Client}
@author+email["Jan Dvořák" "mordae@anilinux.org"]

This library allows for basic communication with Biamp Tesira devices over
their telnet interfaces. It implements message framing, but leaves all RPC
specifics to the user.

It is strongly suggested to consult relevant materials freely available on
Biamp's website. Especially the @bold{Tesira Text Protocol} specification.


@; vim:set ft=scribble sw=2 ts=2 et:
