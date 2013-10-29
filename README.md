# FOSS
A TLS router in Erlang with pluggable routing logic.

## About
This is a TLS router that uses
[SNI](https://en.wikipedia.org/wiki/Server_Name_Indication) to route
connections to the correct endpoint.

The routing logic is kept pluggable, this is so this router can be
built into a larger system. It comes with a simple routing logic built
in, but it is not meant to be used in any production-grade systems.

Keep in mind this server does *not* do TLS termination, that's left to
the backend server.

## Dependencies
This server uses [`ranch`](https://github.com/extend/ranch) and
[sni_parser](https://github.com/omarkj/sni_parser) to parse the SNI.

## Developing
You will need [`rebar`](https://github.com/rebar/rebar) in your `PATH`.

``` bash
$ git clone ssh://git@bitbucket.org/omarkj/foss.git
$ cd foss/
$ rebar get-deps compile
```

### Running the tests
``` bash
$ rebar ct skip_deps=true
```

## Next steps
By writing a
[ALPN](http://tools.ietf.org/html/draft-ietf-tls-applayerprotoneg-02)
extension parser this server could be extended to route for many
different protocols on the same port.
