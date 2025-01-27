-module(range_assignor).

%%% Simplified implementation of Java's RangeAssignor.
%%% - NOT rack-aware.
%%% - does NOT use group instance ID.

-export([
    name/0,
    metadata/1,
    assign/2
]).

name() -> <<"range">>.

-type topic() :: binary().
-type metadata() :: #{topics := [topic()], user_data := binary()}.
-spec metadata(Topics :: [topic()]) -> metadata().

%% Topics is the list of topics that we want to subscribe to.
%% UserData is currently unused.
%% Kafire uses it to tell all members about everyone's assignments, in case the leader changes.
%% We assume version 0 of the consumer subscription.
%% Later versions allow us to specify what we already own, a rack ID, etc.
metadata(Topics) ->
    #{
        topics => Topics, user_data => <<>>
    }.

-type member_id() :: binary().
-type member() :: #{
    member_id := member_id(), group_instance_id := binary() | null, metadata := metadata()
}.
-type assignments() :: [
    {
        MemberId :: member_id(),
        AssignedPartitions :: [#{topic := topic(), partitions := [non_neg_integer()]}]
    }
].
-spec assign(
    Members :: [member()],
    TopicPartitions :: [#{name := topic(), partitions := [non_neg_integer()]}]
) -> assignments().

%% Given a list of members and topics/partitions, assign the partitions to the members.
%% This assignor aims to co-locate partitions of several topics.
%% That is: partition 0 of each topic should go to a single member, partition 1 of
%% each topic should go to a single (possibly different) member.
%% C1 :: (TA, P0), (TB, P0), (TA, P1), (TB, P1)
%% C2 :: (TA, P2), (TB, P2)
assign(Members, TopicPartitions) ->
    Acc = #{MemberId => [] || #{member_id := MemberId} <- Members},
    maps:to_list(assign(lists:sort(Members), TopicPartitions, Acc)).

assign(Members0, TopicPartitions, Acc0) ->
    lists:foldl(
        fun(#{name := Topic, partitions := Partitions}, Acc1) ->
            % Only include the members that want this topic.
            Members = lists:filter(
                fun(_M = #{metadata := #{topics := Topics}}) ->
                    lists:member(Topic, Topics)
                end,
                Members0
            ),
            MemberCount = length(Members),
            Shares = group_consumer_lists:share(Partitions, MemberCount),
            assign_to(Topic, lists:zip(Members, Shares), Acc1)
        end,
        Acc0,
        TopicPartitions
    ).

assign_to(Topic, [{#{member_id := MemberId}, Share} | Rest], Acc) ->
    Acc2 = maps:update_with(
        MemberId,
        fun(MemberAssignment) ->
            [#{topic => Topic, partitions => Share} | MemberAssignment]
        end,
        Acc
    ),
    assign_to(Topic, Rest, Acc2);
assign_to(_Topic, [], Acc) ->
    Acc.
