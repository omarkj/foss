# FOSS
A TLS router in Erlang with pluggable routing logic.

## About
This is a TLS router that uses
[SNI](https://en.wikipedia.org/wiki/Server_Name_Indication) to route
connections to the correct endpoint.

The routing logic is kept pluggable this is so this router can be
built into a larger system.

This server does *not* do TLS termination, that's left to the backend
server thus providing end-to-end TLS.

## Dependencies
This server uses [`ranch`](https://github.com/extend/ranch) and
[sni_parser](https://github.com/omarkj/sni_parser) to parse the SNI.

## Developing
You will need [`rebar`](https://github.com/rebar/rebar) in your `PATH`.

``` bash
$ git clone https://github.com/omarkj/foss.git
$ cd foss/
$ rebar get-deps compile
```

### Running the tests

You need `openssl s_client` to be able to run the tests. Erlang SSL
doesn't support SNI so s_client has to be used to do the SSL test
connection. Your OpenSSL installation needs to support TLS 1.0+.

``` bash
$ rebar ct skip_deps=true
```

## Licence

See `LICENSE`. It's MIT.
