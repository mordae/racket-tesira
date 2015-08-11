# Tesira Client

This library allows for basic communication with [Biamp Tesira]() devices over
their telnet interfaces. It implements message framing, but leaves all RPC
specifics to the user.

It is strongly suggested to consult relevant materials freely available on
Biamp's website. Especially the [Tesira Text Protocol]() specification.

## Example

```racket
(require tesira)

(define my-tesira
  (tesira-connect "10.0.0.135" 23))

(tesira-send my-tesira 'SESSION 'get 'aliases)
```

Check [documentation]() for more details.

[Biamp Tesira]: http://www.biamp.com/products/tesira/
[Tesira Text Protocol]: http://support.biamp.com/Videos/Tesira_Text_Protocol
[documentation]: http://pkg-build.racket-lang.org/doc/tesira/index.html
