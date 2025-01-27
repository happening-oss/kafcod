# Session Timeouts

Session timeouts are used to decide when a group consumer is dead. If a consumer doesn't send a heartbeat message to the
group coordinator frequently enough, that consumer is considered to be dead, and the coordinator triggers a group
rebalance.

TODO: Is it a single missed heartbeat, or more than one? No. It seems to be "no heartbeat received within session
timeout".

The session timeout is specified in the JoinGroup request. If consumers provide different values, it's unclear which
value is used by the coordinator, but I'm going to guess it's the one from the chosen leader.

TODO: Check the above.

In the Java and C clients, the session timeout is controlled by the client `session.timeout.ms` setting. It defaults to
45 seconds.

Lower values improve detection of dead consumers, at the risk of intermittent network issues or latency causing group
churn. Higher values are more resilient to network issues, but it takes longer to detect a dead consumer.

The broker enforces a min/max value with the `group.min.session.timeout.ms` and `group.max.session.timeout.ms`
configuration settings. These default to 6 seconds and 30 minutes, respectively.

The client is expected to send a heartbeat message. The frequency is configured by the client `heartbeat.interval.ms`
setting, which defaults to 3 seconds. It must be lower than `session.timeout.ms`.

More frequent heartbeat messages cause extra network traffic. Less frequent heartbeat messages cause it to take longer
for the group coordinator to tell consumers about group rebalance events.
