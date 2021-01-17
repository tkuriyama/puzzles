
# tsuru

Exercise in parsing Pcap file with Haskell.

### parser

Naive implementation.

- fully decode file using incremental pattern 
- naive reordering, ignoring 3 second sliding window 
- reordering and printing require two passes through messages, after decoding

## parser_v2

Some optimizations...

- reordering with integrated processing (printing), using a deque to maintain a buffer and flushing messages older than 3 seconds
