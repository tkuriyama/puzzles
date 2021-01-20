
# tsuru

Exercise in parsing Pcap file with Haskell.

## parser (v1)

Naive implementation.

- fully decode file using incremental pattern
- naive reordering over entire set of messages, ignoring 3 second sliding window
- reordering and printing require two passes through messages (after decoding)

*Sample run time, printing to terminal with reordering: n/a*

## parser_v2

- rewrite reordering with integrated processing (printing), using a (naive) list to maintain a buffer and flushing messages older than 3 seconds

*Sample run time, printing to terminal with reordering: ~4s*

## parser_v3

- rewrite processing pipeline to perform single pass through the file

*Sample run time, printing to terminal with reordering: ~2.8s*
