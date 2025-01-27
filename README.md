# kafcod

KAFka CODecs.

Auto-generated Kafka encoders and decoders.

## Scope

This project provides encoders and decoders for the Kafka wire protocol. See <https://kafka.apache.org/protocol.html>.

A lot of things are someone else's problem. This library *does not*:
- provide any network processing, buffering, etc.
  - there is a basic `kafcod_connection` module, which is fine for simple purposes, such as examples and integration tests.
  - it's expected that you'll write your own, more-advanced implementation, with asynchronous calls, telemetry, etc.
- apply any meaning to the wire protocol, such as negotiating API versions, etc.
- care about serialization of the Kafka record headers and values: You want to use JSON? Knock yourself out. Erlang term
  format? That's cool, too.
- care about your application's logic.

It assumes that any messages to be decoded are complete. This means that the caller must sort out the message size
prefixing, either manually, or by using Erlang's `{packet, 4}` option.

## Caveats

- There are probably a few minor bugs in the codecs, but mostly in whether fields are nullable or not, and in the
  error reporting.

## Generating the codecs

The encoders and decoders are auto-generated from the JSON schema files in the upstream Kafka source code. To regenerate
them:

```sh
# replace the path with where you've got https://github.com/apache/kafka/ checked out.
make codecs KAFKA_SOURCE_ROOT="$HOME/Source/apache/kafka"
```

### Important!

Very occasionally, there are changes to the field names. Because of the way that the schemas are versioned, this affects
all versions of a particular message, and is a breaking change w.r.t. code generation.

**Review the changes before committing newly-generated codecs!**

## Design Notes

- Why generate the encoders and decoders automatically?
  - Because otherwise we'd have to hand-write new ones whenever the schema changed, and we wanted to take advantage of
    newer messages.
- Why generate Erlang source code? Why not just spit out Core Erlang AST?
  - I considered generating the BEAM files directly, at build time, from the JSON, by generating the AST. It turns out
    that it's hard to manually verify (because it's hard to read), and it makes the generator kinda ugly. The aws_erlang
    project generates source code (both Erlang and Elixir), so I figured that was a reasonable way to do it here as
    well.
  - You can hack on the generated source code to see if a fix or an improvement is worthwhile without needing to work
    out how to implement it in the generator. Obviously, it'll be overwritten when you next regenerate the codecs, but
    it's still useful.
- Why not use a templating engine -- aws_erlang uses EEx, for example?
  - I looked into that (I tried bbmustache and eel, because this is an Erlang project), but the JSON schema files are
    awkward to process -- mostly because of the version range stuff -- in anything other than a real programming
    language.
  - Having decided that was necessary, and worried that there might be other problems with a simplistic templating
    language, I went with the all-Erlang solution.
- Why all of the macros?
  - You might figure out a better way to encode or decode a primitive; the macros decouple the generated codecs from
    that decision. This (hopefully) means less churn.
  - They also make it relatively easy to (e.g.) selectively enable/disable tracing during the build, without
    regenerating the codecs.

## Hacking on the codec generator

*Pro-tip:* try to change as little as possible at each step, and then look at the diffs for the generated codecs to see
if they're sane. It's a lot of machine-generated boilerplate; don't try to change it all at once.

Squash the generated codecs back to a single commit once you're happy, though -- saves on churn in the repository.

Changes to the generators should *not* be squashed into the codec commit, though -- it makes the diffs really hard to
read.

## Unit tests

The unit tests can't cover every type of request and response message, but they attempt to exercise a mixture of field
types and encoders. They aim to be exhaustive for encoding and decoding primitive types (integers, strings, arrays,
etc.).

Where they assert the result of a message encoder, or form the input to a decoder, this has usually been verified in one of two ways:

1. by writing a test harness that encodes the message and sends it to Kafka and checking that the message decodes
   correctly in Wireshark.
2. by using Wireshark to snoop on "normal" (kcat, e.g.) requests and responses and capturing the messages from
   Wireshark.

## Examples

There are some examples in the `examples/` directory. Honestly, they're less "examples" and more "spikes", but feel free
to take a look.

## Enabling decoder tracing

Near the bottom of the `src/decoders.hrl` file, there's an example of how to implement tracing for the compact array
decoder. Implementing other tracing is left as an exercise. PRs are accepted :-)

To run the unit tests with tracing, enable debug logging in `eunit.config`, and then run something like the following:

```sh
rebar3 as trace eunit --test=metadata_response_tests:v9_test
```

The `trace` profile turns on decoder tracing (see `rebar.config`). The `-test` option specifies a single test to run
(otherwise the output is far too noisy).
