# Benchmarking

## Installing erlperf

To benchmark kafcod vs. kafire, I used `erlperf`. See https://github.com/max-au/erlperf. To install it:

```sh
git clone git@github.com:max-au/erlperf.git
cd erlperf
rebar3 as prod escriptize
```

This leaves you with an `erlperf` script in the current directory. I'll assume it's in your PATH for the later snippets.
I symlinked it into my `$HOME/bin` directory, for what it's worth.

## Benchmarking kafcod

Kafire has some captured messages in the `kafire/test/terms_files` directory. For example, we can time how long it takes
`kafcod` to decode a `Fetch` response.

First we build `kafcod`:

```sh
cd /path/to/kafcod
make app
```

Then we run the benchmark; this is for a moderately-sized fetch response:

```sh
ERL_LIBS=.:deps erlperf \
  --init 'application:ensure_all_started(kafcod).' \
  --init_runner '{ok, [Bin]} = file:consult("../kafire/test/fetch_v6_response_0004.terms"), Bin.' \
  'r(Bin) -> fetch_response:decode_fetch_response_6(Bin).'
```

We get the following output, which tells us that it ran about 232,000 iterations per second, taking about 4 ms per
iteration. Not bad.

```
Code                                                           ||        QPS       Time
r(Bin) -> fetch_response:decode_fetch_response_6(Bin).          1     232 Ki    4314 ns
```

## Benchmarking kafire

First we build `kafire`:

```sh
cd /path/to/kafire
make app
```

Then we run the same benchmark:

```sh
ERL_LIBS=.:deps erlperf \
  --init 'ok = kafire_packet:new().' \
  --init_runner '{ok, [Bin]} = file:consult("./test/fetch_v6_response_0004.terms"), Bin.' \
  'r(Bin) -> kafire_packet:decode(1, 6, Bin).'
```

...and we get the following output, which tells us that kafcod is about twice as fast as kafire for raw decode
performance on this sample:

```
Code                                               ||        QPS       Time
r(Bin) -> kafire_packet:decode(1, 6, Bin).          1     114 Ki    8799 ns
```
