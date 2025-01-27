# Supported API Versions

Kafcod deliberately does NOT restrict your use of API keys and API versions. If you use an API key or API version that's
not supported by the broker, the broker will simply drop your connection.

The current stable version of Kafka is 3.7.0. The version used in the 'dev01' cluster is currently 3.4.0.

Wireshark's Kafka protocol dissector doesn't always support the latest versions of the protocol messages. Moreover, even
though it (e.g.) claims to support JoinGroup v7, it doesn't successfully parse v5 requests.

To make it easier to capture and parse Kafka network traffic, it makes sense to restrict ourselves to the versions
supported by both the broker and by Wireshark.

| Message                      | Key | Kafka 3.4.0   | Kafka 3.5.0   | Wireshark 4.4.0 |
|------------------------------|-----|---------------|---------------|-----------------|
| Produce                      | 0   | 0-9           | 0-9           | 0-11            |
| Fetch                        | 1   | 0-12          | 0-15          | 0-16            |
| ListOffsets                  | 2   | 0-6           | 0-8           | 0-8             |
| Metadata                     | 3   | 0-11          | 0-12          | 0-12            |
| LeaderAndIsr                 | 4   | 0-5           | 0-7           | 0-7             |
| StopReplica                  | 5   | 0-3           | 0-4           | 0-4             |
| UpdateMetadata               | 6   | 0-7           | 0-8           | 0-8             |
| ControlledShutdown           | 7   | 0-3           | 0-3           | 0-3             |
| OffsetCommit                 | 8   | 0-8           | 0-8           | 0-8             |
| OffsetFetch                  | 9   | 0-7           | 0-8           | 0-9             |
| FindCoordinator              | 10  | 0-3           | 0-4           | 0-5             |
| JoinGroup                    | 11  | 0-7           | 0-9           | 0-9             |
| Heartbeat                    | 12  | 0-4           | 0-4           | 0-4             |
| LeaveGroup                   | 13  | 0-4           | 0-5           | 0-5             |
| SyncGroup                    | 14  | 5             | 0-5           | 0-5             |
| DescribeGroups               | 15  | 5             | 0-5           | 0-5             |
| ListGroups                   | 16  | 0-4           | 0-4           | 0-3             |
| SaslHandshake                | 17  | 1             | 0-1           | 0-1             |
| ApiVersions                  | 18  | 3             | 0-3           | 0-3             |
| CreateTopics                 | 19  | 7             | 0-7           | 0-7             |
| DeleteTopics                 | 20  | 6             | 0-6           | 0-6             |
| DeleteRecords                | 21  | 2             | 0-2           | 0-1             |
| InitProducerId               | 22  | 4             | 0-4           | 0-5             |
| OffsetForLeaderEpoch         | 23  | 4             | 0-4           | 0-3             |
| AddPartitionsToTxn           | 24  | 3             | 0-3           | 0-1             |
| AddOffsetsToTxn              | 25  | 3             | 0-3           | 0-1             |
| EndTxn                       | 26  | 3             | 0-3           | 0-1             |
| WriteTxnMarkers              | 27  | 1             | 0-1           | 0               |
| TxnOffsetCommit              | 28  | 3             | 0-3           | 0-3             |
| DescribeAcls                 | 29  | 2             | 0-3           | 0-2             |
| CreateAcls                   | 30  | 2             | 0-3           | 0-2             |
| DeleteAcls                   | 31  | 2             | 0-3           | 0-2             |
| DescribeConfigs              | 32  | 4             | 0-4           | 0-4             |
| AlterConfigs                 | 33  | 2             | 0-2           | 0-1             |
| AlterReplicaLogDirs          | 34  | 2             | 0-2           | 0-1             |
| DescribeLogDirs              | 35  | 2             | 0-4           | 0-1             |
| SaslAuthenticate             | 36  | 2             | 0-2           | 0-2             |
| CreatePartitions             | 37  | 3             | 0-3           | 0-2             |
| CreateDelegationToken        | 38  | 2             | 0-3           | 0-2             |
| RenewDelegationToken         | 39  | 2             | 0-2           | 0-2             |
| ExpireDelegationToken        | 40  | 2             | 0-2           | 0-2             |
| DescribeDelegationToken      | 41  | 2             | 0-3           | 0-2             |
| DeleteGroups                 | 42  | 2             | 0-2           | 0-2             |
| ElectLeaders                 | 43  | 2             | 0-2           | 0-2             |
| IncrementalAlterConfigs      | 44  | 1             | 0-1           | 0-1             |
| AlterPartitionReassignments  | 45  | 0             | 0             | 0               |
| ListPartitionReassignments   | 46  | 0             | 0             | 0               |
| OffsetDelete                 | 47  | 0             | 0             | 0               |
| DescribeClientQuotas         | 48  | 1             | 0-1           | not supported   |
| AlterClientQuotas            | 49  | 1             | 0-1           | not supported   |
| DescribeUserScramCredentials | 50  | 0             | 0             | not supported   |
| AlterUserScramCredentials    | 51  | 0             | 0             | not supported   |
| DescribeQuorum               | 55  | not supported | not supported | not supported   |
| AlterPartition               | 56  | 0             | 0-3           | not supported   |
| UpdateFeatures               | 57  | 0             | 0-1           | not supported   |
| Envelope                     | 58  |               | 0             | not supported   |
| DescribeCluster              | 60  | 0             | 0             | 0-1             |
| DescribeProducers            | 61  | 0             | 0             | not supported   |
| UnregisterBroker             | 64  | not supported | not supported | not supported   |
| DescribeTransactions         | 65  | not supported | 0             | not supported   |
| ListTransactions             | 66  | not supported | 0             | not supported   |
| AllocateProducerIds          | 67  | not supported | 0             | 0               |
| ConsumerGroupHeartbeat       | 68  | not supported | not supported | not supported   |
| ConsumerGroupDescribe        | 69  | not supported | not supported | not supported   |
| GetTelemetrySubscriptions    | 71  | not supported | not supported | not supported   |
| PushTelemetry                | 72  | not supported | not supported | not supported   |
| ListClientMetricsResources   | 74  | not supported | not supported | not supported   |
