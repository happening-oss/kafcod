# kcat examples

Create a topic named `cats`:

```sh
kafta topic create cats     # default: 10 partitions, replication factor 3
```

## Produce

```sh
# Key: null; Value: "siamese"
echo "sylvester" | kcat -b localhost:9092 -P -t cats

# -D separates multiple records. They go to separate partitions, so you'll probably see two ProduceRequest messages
# (because each partition belongs to a separate broker).
echo "sylvester|tigger" | kcat -b localhost:9092 -P -D '|' -t cats

# If you specify a partition explicitly, there'll be a single ProduceRequest message.
echo "sylvester|tigger" | kcat -b localhost:9092 -P -D '|' -t cats -p 1
```

```sh
# null values
echo -n "sylvester=|tom=" | kcat -b localhost:9092 -P -D '|' -K '=' -Z -t cats -p 1
```

```sh
# Key/Value pairs
# Sylvester likes (to eat) Tweety; Tigger likes Extract of Malt (apparently).
echo "sylvester=tweety pie|tigger=extract of malt" | \
    kcat -b localhost:9092 -P -D '|' -K '=' -t cats
```

```sh
# Headers
echo "sylvester=tweety" | \
    kcat -b localhost:9092 -P -K '=' -H "owner=Looney Tunes" -t cats
echo "tigger=extract of malt" | \
    kcat -b localhost:9092 -P -K '=' -H "owner=Disney" -t cats
```
