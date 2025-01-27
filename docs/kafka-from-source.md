# Running Kafka from Source Code

Very occasionally, you'll need to run against a Kafka broker that you've built from the source code. This can be useful
if you want to add extra logging or to step through it with a debugger, for instance.

To do this, clone the github.com/apache/kafka repository and build it:

```sh
git clean -dffx  # sometimes useful if the broker won't start and you want to take a scorched earth approach.
./gradlew jar
```

Run Zookeeper:

```sh
./bin/zookeeper-server-start.sh config/zookeeper.properties
```

Run a single broker:

```sh
./bin/kafka-server-start.sh config/server.properties
```
