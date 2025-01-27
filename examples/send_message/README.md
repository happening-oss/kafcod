# send_message

Simple test harness for `kafcod`, intended for sending messages to a broker, such that you can visually check them in
(e.g.) Wireshark.

## Edit

To try out a different message type, edit `send_message.erl` as follows:

1. Change the line that encodes a message, so that it encodes your chosen message.
2. Change the line that decodes the response, so that it decodes your expected response message.
3. Build it; run it (see below).

## Build

    $ rebar3 escriptize

## Run

    $ _build/default/bin/send_message
