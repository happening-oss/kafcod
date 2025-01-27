-module(group_membership_SUITE).

% Can't use 'let it crash' here, because I've seen a weird thing where we have two members,
% and they both crash, and then they alternate being the leader, and we just keep going round.
% I think it'd probably work if we persisted the member ID, but that means keeping the process.
% (or having the member ID persisted in the supervisor).
% How to guard against this happening again? How did I reproduce it in the first place?

% BUG: Without the handling of REBALANCE_IN_PROGRESS above, we get elected leader. Then
% we heartbeat and get told about the rebalance. We crash, calling LeaveGroup below.
% This causes the other guy to be elected leader immediately.
% Then we restart, causing him to get REBALANCE_IN_PROGRESS, crash and leave the group.
% We get elected leader; he restarts, and so on.

% To reproduce that:
% 1. Given brokers.
% 2. One member joins the group.
% 3. It gets elected leader.
% 4. Another member joins the group.
% 5. Profit.

% How to test this in a single process, given that the child ID is the group name?
% Maybe we can have more than one group_consumer_app_sup (renamed to group_consumer_sup_sup).
% Maybe we don't use the group ID as the child ID. If we returned a ref, that would work, but it makes stop_consumer
% harder to use. Or the child ID is {GroupId, Ref}, which means we can at least find them in which_children.
% Or the test orchestrates multiple peer nodes? Not done that previously, but it could work.

two_members_leader_gets_rebalance_in_progress(Config) ->
    % Assert that a broker exists.
    % Start a group consumer.
    % Wait until it has joined the group successfully. One of:
    %  - introspection shows that it's heartbeating; how to tell?
    %  - querying the broker shows a single member.
    % Start another group consumer.
    % How to verify that everything's fine? Maybe:
    %  - Lack of errors/crashes.
    %  - Eventually the generation ID should be stable.
    %  - Do we get any metrics from anything about how many rebalance events have happened?
    %    Yes: kafka_server_group_coordinator_metrics_group_completed_rebalance_rate{} should be < 0.1,
    %    based on looking at the metric in Grafana.
    %  - Listing consumers should have the expected number of consumers.
    ok.

coordinator_has_moved(Config) ->
    % While heartbeating, move the __consumer_offsets topic to another broker. That causes
    % the coordinator to also move. We have to move the whole thing, because we don't know which partition goes with which group.
    % Well, we don't have to *move* the whole thing to a different set of brokers; we can just ensure the partition replicas get rotated so there's a new leader.
    ok.

coordinator_dies(Config) ->
    % What it says.
    ok.

consumer_offsets(Config) ->
    % We need some tests around offsets (maybe not a CT test):
    % - what if there are no records returned?
    % - are the batches definitely in order?
    % - can we capture whether it's last-offset or next-offset that must be committed?
    ok.
