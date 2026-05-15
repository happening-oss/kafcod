-module(kafcod_error).
-export([is_retriable/1]).
-include("error_code.hrl").

%% Returns true if the Kafka error code indicates a transient failure that
%% may succeed on retry. Based on Kafka's RetriableException hierarchy in the
%% Java client.
-spec is_retriable(ErrorCode :: integer()) -> boolean().

is_retriable(?CORRUPT_MESSAGE) -> true;
is_retriable(?UNKNOWN_TOPIC_OR_PARTITION) -> true;
is_retriable(?LEADER_NOT_AVAILABLE) -> true;
is_retriable(?NOT_LEADER_OR_FOLLOWER) -> true;
is_retriable(?REQUEST_TIMED_OUT) -> true;
is_retriable(?REPLICA_NOT_AVAILABLE) -> true;
is_retriable(?NETWORK_EXCEPTION) -> true;
is_retriable(?COORDINATOR_LOAD_IN_PROGRESS) -> true;
is_retriable(?COORDINATOR_NOT_AVAILABLE) -> true;
is_retriable(?NOT_COORDINATOR) -> true;
is_retriable(?NOT_ENOUGH_REPLICAS) -> true;
is_retriable(?NOT_ENOUGH_REPLICAS_AFTER_APPEND) -> true;
is_retriable(?NOT_CONTROLLER) -> true;
is_retriable(?CONCURRENT_TRANSACTIONS) -> true;
is_retriable(?KAFKA_STORAGE_ERROR) -> true;
is_retriable(?FETCH_SESSION_ID_NOT_FOUND) -> true;
is_retriable(?INVALID_FETCH_SESSION_EPOCH) -> true;
is_retriable(?LISTENER_NOT_FOUND) -> true;
is_retriable(?FENCED_LEADER_EPOCH) -> true;
is_retriable(?UNKNOWN_LEADER_EPOCH) -> true;
is_retriable(?OFFSET_NOT_AVAILABLE) -> true;
is_retriable(?PREFERRED_LEADER_NOT_AVAILABLE) -> true;
is_retriable(?ELIGIBLE_LEADERS_NOT_AVAILABLE) -> true;
is_retriable(?ELECTION_NOT_NEEDED) -> true;
is_retriable(?UNSTABLE_OFFSET_COMMIT) -> true;
is_retriable(?THROTTLING_QUOTA_EXCEEDED) -> true;
is_retriable(?UNKNOWN_TOPIC_ID) -> true;
is_retriable(?INCONSISTENT_TOPIC_ID) -> true;
is_retriable(?FETCH_SESSION_TOPIC_ID_ERROR) -> true;
is_retriable(?SHARE_SESSION_NOT_FOUND) -> true;
is_retriable(?INVALID_SHARE_SESSION_EPOCH) -> true;
is_retriable(_) -> false.
