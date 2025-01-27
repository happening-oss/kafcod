# Capturing from Wireshark

1. Capture some traffic. Probably on loopback.
2. In the dissector view (bottom left), right-click on "API Key: Metadata (3)" (for example) and select Copy -> Bytes as a Hex Stream.
3. You'll get something like `000000190003000c00000003000772646b61666b610000000000010000` copied to the clipboard.
4. In an Erlang shell, evaluate
   `rp(binary:decode_hex(<<"000000190003000c00000003000772646b61666b610000000000010000">>))`. You'll get something like
   `<<0,0,0,25,0,3,0,12,0,0,0,3,0,7,114,100,107,97,102,107,97,0,0,0,0,0,1,0,0>>`.
5. Copy that to your unit test.
6. Delete the first 4 bytes (here `0,0,0,25,`); these are the length prefix and you don't want those.
