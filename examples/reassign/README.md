# reassign

The `reassign` script shows how to use the `AlterPartitionReassignments`, `ListPartitionReassignments` and
`ElectLeaders` Kafka messages.

More importantly, it provides an alternative to `kafka-reassign-partitions.sh`, where you don't need to generate a JSON
file with the reassignment plan.

## Building it

In the `examples/reassign` directory, run `make`. It will generate an escript.

## Examples

Move the listed partitions of a particular topic to different brokers:

```sh
./reassign \
    --bootstrap localhost:9092 reassign-partitions \
    --topic customer_campaign \
    --partition 40 42 43 44 54 56 58 \
    --to 107 100 101 102 300 301 302 \
    --pick 3 \
    --shuffle \
    --keep-leader
```

## Details

- `--bootstrap`: specify the bootstrap broker.
- `--topic`: specify the topic.
- `--partition`: specify the partition or partitions to reassign; you can specify multiple partitions and you can
  specify the option multiple times.
- `--to`: specify the IDs of the brokers to reassign the partitions to. Without `--pick` or `--shuffle`, this is taken
  verbatim as the list of replicas; the first item is the leader.
- `--pick`: choose this many brokers for replication. Without this, all of the brokers are used. For a replication
  factor of 3, use `--pick 3`.
- `--shuffle`: for each partition, shuffle the list of replicas. Without this, all of the partitions will be assigned
  directly, meaning that they'll all have the same leader and the same replicas (you probably don't want this).
- `--keep-leader`: keep the existing leader, even if it's not in the `--to` list.

Note that Kafka reassigns the partitions in the background. To see the list of outstanding requests, use `./reassign
list-partition-reassignments`.
