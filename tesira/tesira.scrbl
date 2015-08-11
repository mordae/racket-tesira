#lang scribble/manual

@title{Tesira Client}
@author+email["Jan Dvořák" "mordae@anilinux.org"]

@(define tesira-link "http://www.biamp.com/products/tesira/")
@(define ttp-link "http://support.biamp.com/Videos/Tesira_Text_Protocol")

This library allows for basic communication with
@hyperlink[tesira-link]{Biamp Tesira} devices over their telnet interfaces.
It implements message framing, but leaves all RPC specifics to the user.

It is strongly suggested to consult relevant materials freely available on
Biamp's website. Especially the @hyperlink[ttp-link]{Tesira Text Protocol}
specification.


@; vim:set ft=scribble sw=2 ts=2 et:
