# A note about the unit tests

A large number of the unit tests use captured packets from a real broker (see docs/wireshark-captures.md). These are
intended for two things:

1. Do the codecs actually work?
2. To characterise what's actually _in_ a packet from a "real" client or broker. This is why some of them decode
   requests and encode replies, even though we're mostly expecting to be used in a client, where you'd expect the
   opposite.
