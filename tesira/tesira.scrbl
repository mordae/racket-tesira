#lang scribble/manual

@require[(for-label tesira)
         (for-label typed/racket/base)]

@title{Tesira Client}
@author+email["Jan Dvořák" "mordae@anilinux.org"]


@defmodule[tesira]

@(define tesira-link "http://www.biamp.com/products/tesira/")
@(define ttp-link "http://support.biamp.com/Videos/Tesira_Text_Protocol")

This library allows for basic communication with
@hyperlink[tesira-link]{Biamp Tesira} devices over their telnet interfaces.
It implements message framing, but leaves all RPC specifics to the user.

It is strongly suggested to consult relevant materials freely available on
Biamp's website. Especially the @hyperlink[ttp-link]{Tesira Text Protocol}
specification.


@defidform[Tesira]{
  Type for tesira client object.
}

@defidform[Tesira-Response]{
  Type for values obtained from the device.
}

@defidform[TExpr]{
  Type for arguments that can be passed to @racket[tesira-send].
  Very similar to JSON expressions, but not completely.
}

@defproc[(tesira? (v Any)) Boolean]{
  Predicate identifying @racket[Tesira] instances.
}

@defproc[(tesira-connect (host String)
                         (port Positive-Integer 23)
                         (notify (-> Tesira-Response Void) void))
         Tesira]{
  Connect to a remote device via it's Telnet interface.

  When a different @racket[notify] function is given, it will be called
  every time an asynchronous notification arrives in the middle of a
  @racket[tesira-send] call.
}

@defproc[(tesira-send (a-tesira Tesira)
                      (alias Symbol)
                      (verb Symbol)
                      (attr Symbol)
                      (arg TExpr) ...) Tesira-Response]{
  Perform remote call and return the result.

  The client object is locked for the duration of the call and any concurrent
  call will be blocked until the first one receives it's reply.

  Concurrent use of this function together with @racket[tesira-listen] is not
  recommended as @racket[tesira-listen] will lock the client object until it
  receives a notification message. Consider using another instance to receive
  changes through.
}

@defproc[(tesira-listen (a-tesira Tesira)) Tesira-Response]{
  Listen to a single asynchronous notification.
}


@; vim:set ft=scribble sw=2 ts=2 et:
