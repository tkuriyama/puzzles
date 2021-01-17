
# tsuru

Exercise in parsing Pcap file with Haskell.

## parser (v1)

Naive implementation.

- fully decode file using incremental pattern
- naive reordering over entire set of messages, ignoring 3 second sliding window
- reordering and printing require two passes through messages (after decoding)

## parser_v2

- rewrite reordering with integrated processing (printing), using a (naive) list to maintain a buffer and flushing messages older than 3 seconds


