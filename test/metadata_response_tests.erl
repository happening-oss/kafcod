-module(metadata_response_tests).
-include_lib("eunit/include/eunit.hrl").

v1_no_topics_test() ->
    Capture =
        <<0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 200, 0, 14, 49, 57, 50, 46, 49, 54, 56, 46, 53, 56, 46,
            49, 51, 54, 0, 0, 35, 133, 0, 6, 114, 97, 99, 107, 45, 98, 0, 0, 0, 100, 0, 14, 49, 57,
            50, 46, 49, 54, 56, 46, 53, 56, 46, 49, 51, 54, 0, 0, 35, 132, 0, 6, 114, 97, 99, 107,
            45, 97, 0, 0, 1, 44, 0, 14, 49, 57, 50, 46, 49, 54, 56, 46, 53, 56, 46, 49, 51, 54, 0,
            0, 35, 134, 0, 6, 114, 97, 99, 107, 45, 99, 0, 0, 1, 44, 0, 0, 0, 0>>,
    Decoded = {
        #{
            correlation_id => 1,
            brokers =>
                [
                    #{
                        host => <<"192.168.58.136">>,
                        node_id => 200,
                        port => 9093,
                        rack => <<"rack-b">>
                    },
                    #{
                        host => <<"192.168.58.136">>,
                        node_id => 100,
                        port => 9092,
                        rack => <<"rack-a">>
                    },
                    #{
                        host => <<"192.168.58.136">>,
                        node_id => 300,
                        port => 9094,
                        rack => <<"rack-c">>
                    }
                ],
            controller_id => 300,
            topics => []
        },
        <<>>
    },
    ?assertEqual(Decoded, metadata_response:decode_metadata_response_1(Capture)).

v1_with_topics_test() ->
    Capture =
        <<0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 200, 0, 14, 49, 57, 50, 46, 49, 54, 56, 46, 53, 56, 46,
            49, 51, 54, 0, 0, 35, 133, 0, 6, 114, 97, 99, 107, 45, 98, 0, 0, 0, 100, 0, 14, 49, 57,
            50, 46, 49, 54, 56, 46, 53, 56, 46, 49, 51, 54, 0, 0, 35, 132, 0, 6, 114, 97, 99, 107,
            45, 97, 0, 0, 1, 44, 0, 14, 49, 57, 50, 46, 49, 54, 56, 46, 53, 56, 46, 49, 51, 54, 0,
            0, 35, 134, 0, 6, 114, 97, 99, 107, 45, 99, 0, 0, 1, 44, 0, 0, 0, 1, 0, 0, 0, 7, 116,
            111, 112, 105, 99, 95, 49, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 200, 0, 0, 0, 3, 0,
            0, 0, 200, 0, 0, 1, 44, 0, 0, 0, 100, 0, 0, 0, 3, 0, 0, 0, 200, 0, 0, 1, 44, 0, 0, 0,
            100, 0, 0, 0, 0, 0, 1, 0, 0, 1, 44, 0, 0, 0, 3, 0, 0, 1, 44, 0, 0, 0, 100, 0, 0, 0, 200,
            0, 0, 0, 3, 0, 0, 1, 44, 0, 0, 0, 100, 0, 0, 0, 200, 0, 0, 0, 0, 0, 2, 0, 0, 0, 100, 0,
            0, 0, 3, 0, 0, 0, 100, 0, 0, 0, 200, 0, 0, 1, 44, 0, 0, 0, 3, 0, 0, 0, 100, 0, 0, 0,
            200, 0, 0, 1, 44, 0, 0, 0, 0, 0, 3, 0, 0, 0, 200, 0, 0, 0, 3, 0, 0, 0, 200, 0, 0, 0,
            100, 0, 0, 1, 44, 0, 0, 0, 3, 0, 0, 0, 200, 0, 0, 0, 100, 0, 0, 1, 44>>,
    Decoded = {
        #{
            correlation_id => 1,
            brokers =>
                [
                    #{
                        host => <<"192.168.58.136">>,
                        node_id => 200,
                        port => 9093,
                        rack => <<"rack-b">>
                    },
                    #{
                        host => <<"192.168.58.136">>,
                        node_id => 100,
                        port => 9092,
                        rack => <<"rack-a">>
                    },
                    #{
                        host => <<"192.168.58.136">>,
                        node_id => 300,
                        port => 9094,
                        rack => <<"rack-c">>
                    }
                ],
            controller_id => 300,
            topics =>
                [
                    #{
                        error_code => 0,
                        is_internal => false,
                        name => <<"topic_1">>,
                        partitions =>
                            [
                                #{
                                    error_code => 0,
                                    isr_nodes => [200, 300, 100],
                                    leader_id => 200,
                                    partition_index => 0,
                                    replica_nodes => [200, 300, 100]
                                },
                                #{
                                    error_code => 0,
                                    isr_nodes => [300, 100, 200],
                                    leader_id => 300,
                                    partition_index => 1,
                                    replica_nodes => [300, 100, 200]
                                },
                                #{
                                    error_code => 0,
                                    isr_nodes => [100, 200, 300],
                                    leader_id => 100,
                                    partition_index => 2,
                                    replica_nodes => [100, 200, 300]
                                },
                                #{
                                    error_code => 0,
                                    isr_nodes => [200, 100, 300],
                                    leader_id => 200,
                                    partition_index => 3,
                                    replica_nodes => [200, 100, 300]
                                }
                            ]
                    }
                ]
        },
        <<>>
    },
    ?assertEqual(Decoded, metadata_response:decode_metadata_response_1(Capture)).

v9_test() ->
    % v9 is the first flexible version; let's check we're getting tag buffers correct.
    Capture =
        <<0, 0, 0, 1, 0, 0, 0, 0, 0, 4, 0, 0, 0, 200, 15, 49, 57, 50, 46, 49, 54, 56, 46, 53, 56,
            46, 49, 51, 54, 0, 0, 35, 133, 7, 114, 97, 99, 107, 45, 98, 0, 0, 0, 0, 100, 15, 49, 57,
            50, 46, 49, 54, 56, 46, 53, 56, 46, 49, 51, 54, 0, 0, 35, 132, 7, 114, 97, 99, 107, 45,
            97, 0, 0, 0, 1, 44, 15, 49, 57, 50, 46, 49, 54, 56, 46, 53, 56, 46, 49, 51, 54, 0, 0,
            35, 134, 7, 114, 97, 99, 107, 45, 99, 0, 23, 49, 88, 118, 56, 87, 80, 119, 111, 83, 85,
            75, 68, 50, 100, 45, 101, 113, 68, 106, 77, 120, 119, 0, 0, 1, 44, 1, 0, 0, 31, 160,
            0>>,
    Decoded = {
        #{
            correlation_id => 1,
            brokers => [
                #{
                    host => <<"192.168.58.136">>,
                    node_id => 200,
                    port => 9093,
                    rack => <<"rack-b">>
                },
                #{
                    host => <<"192.168.58.136">>,
                    node_id => 100,
                    port => 9092,
                    rack => <<"rack-a">>
                },
                #{
                    host => <<"192.168.58.136">>,
                    node_id => 300,
                    port => 9094,
                    rack => <<"rack-c">>
                }
            ],
            cluster_authorized_operations => 8096,
            cluster_id => <<"1Xv8WPwoSUKD2d-eqDjMxw">>,
            controller_id => 300,
            throttle_time_ms => 0,
            topics => []
        },
        <<>>
    },
    ?assertEqual(Decoded, metadata_response:decode_metadata_response_9(Capture)).

v12_test() ->
    Capture =
        <<0, 0, 0, 1, 0, 0, 0, 0, 0, 4, 0, 0, 0, 200, 15, 49, 57, 50, 46, 49, 54, 56, 46, 53, 56,
            46, 49, 51, 54, 0, 0, 35, 133, 7, 114, 97, 99, 107, 45, 98, 0, 0, 0, 0, 100, 15, 49, 57,
            50, 46, 49, 54, 56, 46, 53, 56, 46, 49, 51, 54, 0, 0, 35, 132, 7, 114, 97, 99, 107, 45,
            97, 0, 0, 0, 1, 44, 15, 49, 57, 50, 46, 49, 54, 56, 46, 53, 56, 46, 49, 51, 54, 0, 0,
            35, 134, 7, 114, 97, 99, 107, 45, 99, 0, 23, 49, 88, 118, 56, 87, 80, 119, 111, 83, 85,
            75, 68, 50, 100, 45, 101, 113, 68, 106, 77, 120, 119, 0, 0, 1, 44, 6, 0, 0, 8, 116, 111,
            112, 105, 99, 45, 99, 99, 202, 118, 237, 29, 121, 71, 145, 156, 147, 50, 81, 172, 134,
            234, 1, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 200, 0, 0, 0, 0, 4, 0, 0, 0, 200, 0, 0, 0, 100,
            0, 0, 1, 44, 4, 0, 0, 0, 200, 0, 0, 0, 100, 0, 0, 1, 44, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0,
            1, 44, 0, 0, 0, 0, 4, 0, 0, 1, 44, 0, 0, 0, 200, 0, 0, 0, 100, 4, 0, 0, 1, 44, 0, 0, 0,
            200, 0, 0, 0, 100, 1, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 100, 0, 0, 0, 0, 4, 0, 0, 0, 100, 0,
            0, 1, 44, 0, 0, 0, 200, 4, 0, 0, 0, 100, 0, 0, 1, 44, 0, 0, 0, 200, 1, 0, 0, 0, 0, 0, 0,
            3, 0, 0, 0, 200, 0, 0, 0, 0, 4, 0, 0, 0, 200, 0, 0, 1, 44, 0, 0, 0, 100, 4, 0, 0, 0,
            200, 0, 0, 1, 44, 0, 0, 0, 100, 1, 0, 0, 0, 13, 248, 0, 0, 0, 8, 116, 111, 112, 105, 99,
            45, 98, 183, 200, 67, 97, 232, 52, 67, 120, 174, 252, 214, 55, 96, 249, 227, 197, 0, 5,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 100, 0, 0, 0, 0, 4, 0, 0, 0, 100, 0, 0, 0, 200, 0, 0, 1, 44,
            4, 0, 0, 0, 100, 0, 0, 0, 200, 0, 0, 1, 44, 1, 0, 0, 0, 0, 0, 0, 2, 0, 0, 1, 44, 0, 0,
            0, 0, 4, 0, 0, 1, 44, 0, 0, 0, 100, 0, 0, 0, 200, 4, 0, 0, 1, 44, 0, 0, 0, 100, 0, 0, 0,
            200, 1, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 100, 0, 0, 0, 0, 4, 0, 0, 0, 100, 0, 0, 1, 44, 0,
            0, 0, 200, 4, 0, 0, 0, 100, 0, 0, 1, 44, 0, 0, 0, 200, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
            200, 0, 0, 0, 0, 4, 0, 0, 0, 200, 0, 0, 1, 44, 0, 0, 0, 100, 4, 0, 0, 0, 200, 0, 0, 1,
            44, 0, 0, 0, 100, 1, 0, 0, 0, 13, 248, 0, 0, 0, 10, 116, 104, 101, 95, 116, 111, 112,
            105, 99, 150, 152, 42, 99, 4, 115, 79, 205, 138, 177, 248, 204, 58, 32, 124, 187, 0, 5,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 100, 0, 0, 0, 0, 4, 0, 0, 0, 100, 0, 0, 1, 44, 0, 0, 0, 200,
            4, 0, 0, 0, 100, 0, 0, 1, 44, 0, 0, 0, 200, 1, 0, 0, 0, 0, 0, 0, 2, 0, 0, 1, 44, 0, 0,
            0, 0, 4, 0, 0, 1, 44, 0, 0, 0, 200, 0, 0, 0, 100, 4, 0, 0, 1, 44, 0, 0, 0, 200, 0, 0, 0,
            100, 1, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 100, 0, 0, 0, 0, 4, 0, 0, 0, 100, 0, 0, 0, 200, 0,
            0, 1, 44, 4, 0, 0, 0, 100, 0, 0, 0, 200, 0, 0, 1, 44, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
            200, 0, 0, 0, 0, 4, 0, 0, 0, 200, 0, 0, 0, 100, 0, 0, 1, 44, 4, 0, 0, 0, 200, 0, 0, 0,
            100, 0, 0, 1, 44, 1, 0, 0, 0, 13, 248, 0, 0, 0, 8, 116, 111, 112, 105, 99, 45, 97, 136,
            5, 150, 7, 156, 101, 75, 81, 187, 133, 137, 113, 249, 70, 115, 130, 0, 5, 0, 0, 0, 0, 0,
            0, 0, 0, 1, 44, 0, 0, 0, 0, 4, 0, 0, 1, 44, 0, 0, 0, 200, 0, 0, 0, 100, 4, 0, 0, 1, 44,
            0, 0, 0, 200, 0, 0, 0, 100, 1, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 200, 0, 0, 0, 0, 4, 0, 0,
            0, 200, 0, 0, 0, 100, 0, 0, 1, 44, 4, 0, 0, 0, 200, 0, 0, 0, 100, 0, 0, 1, 44, 1, 0, 0,
            0, 0, 0, 0, 3, 0, 0, 1, 44, 0, 0, 0, 0, 4, 0, 0, 1, 44, 0, 0, 0, 100, 0, 0, 0, 200, 4,
            0, 0, 1, 44, 0, 0, 0, 100, 0, 0, 0, 200, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 100, 0, 0, 0,
            0, 4, 0, 0, 0, 100, 0, 0, 1, 44, 0, 0, 0, 200, 4, 0, 0, 0, 100, 0, 0, 1, 44, 0, 0, 0,
            200, 1, 0, 0, 0, 13, 248, 0, 0, 0, 8, 116, 111, 112, 105, 99, 95, 49, 73, 3, 134, 225,
            163, 96, 70, 126, 159, 51, 248, 89, 218, 159, 248, 116, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            200, 0, 0, 0, 0, 4, 0, 0, 0, 200, 0, 0, 1, 44, 0, 0, 0, 100, 4, 0, 0, 0, 200, 0, 0, 1,
            44, 0, 0, 0, 100, 1, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 100, 0, 0, 0, 0, 4, 0, 0, 0, 100, 0,
            0, 0, 200, 0, 0, 1, 44, 4, 0, 0, 0, 100, 0, 0, 0, 200, 0, 0, 1, 44, 1, 0, 0, 0, 0, 0, 0,
            3, 0, 0, 0, 200, 0, 0, 0, 0, 4, 0, 0, 0, 200, 0, 0, 0, 100, 0, 0, 1, 44, 4, 0, 0, 0,
            200, 0, 0, 0, 100, 0, 0, 1, 44, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 44, 0, 0, 0, 0, 4, 0,
            0, 1, 44, 0, 0, 0, 100, 0, 0, 0, 200, 4, 0, 0, 1, 44, 0, 0, 0, 100, 0, 0, 0, 200, 1, 0,
            0, 0, 13, 248, 0, 0>>,
    Decoded = {
        #{
            correlation_id => 1,
            brokers => [
                #{
                    host => <<"192.168.58.136">>,
                    node_id => 200,
                    port => 9093,
                    rack => <<"rack-b">>
                },
                #{
                    host => <<"192.168.58.136">>,
                    node_id => 100,
                    port => 9092,
                    rack => <<"rack-a">>
                },
                #{
                    host => <<"192.168.58.136">>,
                    node_id => 300,
                    port => 9094,
                    rack => <<"rack-c">>
                }
            ],
            cluster_id => <<"1Xv8WPwoSUKD2d-eqDjMxw">>,
            controller_id => 300,
            throttle_time_ms => 0,
            topics =>
                [
                    #{
                        error_code => 0,
                        is_internal => false,
                        name => <<"topic-c">>,
                        partitions =>
                            [
                                #{
                                    error_code => 0,
                                    isr_nodes => [200, 100, 300],
                                    leader_epoch => 0,
                                    leader_id => 200,
                                    offline_replicas => [],
                                    partition_index => 0,
                                    replica_nodes => [200, 100, 300]
                                },
                                #{
                                    error_code => 0,
                                    isr_nodes => [300, 200, 100],
                                    leader_epoch => 0,
                                    leader_id => 300,
                                    offline_replicas => [],
                                    partition_index => 1,
                                    replica_nodes => [300, 200, 100]
                                },
                                #{
                                    error_code => 0,
                                    isr_nodes => [100, 300, 200],
                                    leader_epoch => 0,
                                    leader_id => 100,
                                    offline_replicas => [],
                                    partition_index => 2,
                                    replica_nodes => [100, 300, 200]
                                },
                                #{
                                    error_code => 0,
                                    isr_nodes => [200, 300, 100],
                                    leader_epoch => 0,
                                    leader_id => 200,
                                    offline_replicas => [],
                                    partition_index => 3,
                                    replica_nodes => [200, 300, 100]
                                }
                            ],
                        topic_authorized_operations => 3576,
                        topic_id => <<"63ca76ed-1d79-4791-9c93-3251ac86ea01">>
                    },
                    #{
                        error_code => 0,
                        is_internal => false,
                        name => <<"topic-b">>,
                        partitions =>
                            [
                                #{
                                    error_code => 0,
                                    isr_nodes => [100, 200, 300],
                                    leader_epoch => 0,
                                    leader_id => 100,
                                    offline_replicas => [],
                                    partition_index => 0,
                                    replica_nodes => [100, 200, 300]
                                },
                                #{
                                    error_code => 0,
                                    isr_nodes => [300, 100, 200],
                                    leader_epoch => 0,
                                    leader_id => 300,
                                    offline_replicas => [],
                                    partition_index => 2,
                                    replica_nodes => [300, 100, 200]
                                },
                                #{
                                    error_code => 0,
                                    isr_nodes => [100, 300, 200],
                                    leader_epoch => 0,
                                    leader_id => 100,
                                    offline_replicas => [],
                                    partition_index => 3,
                                    replica_nodes => [100, 300, 200]
                                },
                                #{
                                    error_code => 0,
                                    isr_nodes => [200, 300, 100],
                                    leader_epoch => 0,
                                    leader_id => 200,
                                    offline_replicas => [],
                                    partition_index => 1,
                                    replica_nodes => [200, 300, 100]
                                }
                            ],
                        topic_authorized_operations => 3576,
                        topic_id => <<"b7c84361-e834-4378-aefc-d63760f9e3c5">>
                    },
                    #{
                        error_code => 0,
                        is_internal => false,
                        name => <<"the_topic">>,
                        partitions =>
                            [
                                #{
                                    error_code => 0,
                                    isr_nodes => [100, 300, 200],
                                    leader_epoch => 0,
                                    leader_id => 100,
                                    offline_replicas => [],
                                    partition_index => 0,
                                    replica_nodes => [100, 300, 200]
                                },
                                #{
                                    error_code => 0,
                                    isr_nodes => [300, 200, 100],
                                    leader_epoch => 0,
                                    leader_id => 300,
                                    offline_replicas => [],
                                    partition_index => 2,
                                    replica_nodes => [300, 200, 100]
                                },
                                #{
                                    error_code => 0,
                                    isr_nodes => [100, 200, 300],
                                    leader_epoch => 0,
                                    leader_id => 100,
                                    offline_replicas => [],
                                    partition_index => 3,
                                    replica_nodes => [100, 200, 300]
                                },
                                #{
                                    error_code => 0,
                                    isr_nodes => [200, 100, 300],
                                    leader_epoch => 0,
                                    leader_id => 200,
                                    offline_replicas => [],
                                    partition_index => 1,
                                    replica_nodes => [200, 100, 300]
                                }
                            ],
                        topic_authorized_operations => 3576,
                        topic_id => <<"96982a63-0473-4fcd-8ab1-f8cc3a207cbb">>
                    },
                    #{
                        error_code => 0,
                        is_internal => false,
                        name => <<"topic-a">>,
                        partitions =>
                            [
                                #{
                                    error_code => 0,
                                    isr_nodes => [300, 200, 100],
                                    leader_epoch => 0,
                                    leader_id => 300,
                                    offline_replicas => [],
                                    partition_index => 0,
                                    replica_nodes => [300, 200, 100]
                                },
                                #{
                                    error_code => 0,
                                    isr_nodes => [200, 100, 300],
                                    leader_epoch => 0,
                                    leader_id => 200,
                                    offline_replicas => [],
                                    partition_index => 2,
                                    replica_nodes => [200, 100, 300]
                                },
                                #{
                                    error_code => 0,
                                    isr_nodes => [300, 100, 200],
                                    leader_epoch => 0,
                                    leader_id => 300,
                                    offline_replicas => [],
                                    partition_index => 3,
                                    replica_nodes => [300, 100, 200]
                                },
                                #{
                                    error_code => 0,
                                    isr_nodes => [100, 300, 200],
                                    leader_epoch => 0,
                                    leader_id => 100,
                                    offline_replicas => [],
                                    partition_index => 1,
                                    replica_nodes => [100, 300, 200]
                                }
                            ],
                        topic_authorized_operations => 3576,
                        topic_id => <<"88059607-9c65-4b51-bb85-8971f9467382">>
                    },
                    #{
                        error_code => 0,
                        is_internal => false,
                        name => <<"topic_1">>,
                        partitions =>
                            [
                                #{
                                    error_code => 0,
                                    isr_nodes => [200, 300, 100],
                                    leader_epoch => 0,
                                    leader_id => 200,
                                    offline_replicas => [],
                                    partition_index => 0,
                                    replica_nodes => [200, 300, 100]
                                },
                                #{
                                    error_code => 0,
                                    isr_nodes => [100, 200, 300],
                                    leader_epoch => 0,
                                    leader_id => 100,
                                    offline_replicas => [],
                                    partition_index => 2,
                                    replica_nodes => [100, 200, 300]
                                },
                                #{
                                    error_code => 0,
                                    isr_nodes => [200, 100, 300],
                                    leader_epoch => 0,
                                    leader_id => 200,
                                    offline_replicas => [],
                                    partition_index => 3,
                                    replica_nodes => [200, 100, 300]
                                },
                                #{
                                    error_code => 0,
                                    isr_nodes => [300, 100, 200],
                                    leader_epoch => 0,
                                    leader_id => 300,
                                    offline_replicas => [],
                                    partition_index => 1,
                                    replica_nodes => [300, 100, 200]
                                }
                            ],
                        topic_authorized_operations => 3576,
                        topic_id => <<"490386e1-a360-467e-9f33-f859da9ff874">>
                    }
                ]
        },
        <<>>
    },
    ?assertEqual(Decoded, metadata_response:decode_metadata_response_12(Capture)).
