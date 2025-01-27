# group_consumer

## Running it

```sh
$ rebar3 auto
```

## Start a group consumer

```erlang
group_consumer:start_consumer(Broker, ClientId, GroupId, Topics).
```

For example:

```erlang
group_consumer:start_consumer({"localhost", 9092}, <<"group_consumer">>, <<"group">>, [<<"p">>, <<"q">>]).
```

```erlang
group_consumer:start_consumer({"localhost", 8001}, <<"group_consumer">>, <<"kafcod_group">>, [<<"docify_request">>]).
```

## Produce a message

```sh
echo "value" | kcat -b localhost:9092 -P -t p
```

## Key-value pairs

```sh
echo "key1=value1|key2=value2" | kcat -b localhost:9092 -P -D '|' -K '=' -t q
```
