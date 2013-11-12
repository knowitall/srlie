SRLIE
=====
SRLIE is a component of Open IE 4.0 that automatically identifies n-ary extractions from English sentences.
SRLIE is designed for Web-scale information extraction, where target relations are not specified in advance.

SRLIE constructs richer extractions than OLLIE.  It builds extractions from Semantic Role Labelling (SRL)
SRL frames are not extractions themselves because they contain many frame arguments that are not considered
extraction arguments in Open IE.  SRL also does not identify argument boundaries or expand relation verbs
into relation phrases.

## Process

SRLIE is composed of a layer of functions on the input sentence.

1.  First the sentences is processed by a dependency parser.
1.  Next the dependency graph is run through a SRL system to produce SRL Frames.
2.  Then the frames are sent through SRLIE to produce n-ary extractions.  This involves filtering some SRL frames, determing argument boundaries, and constructing a relation phrase.
  a.  Optionally the n-ary extractions can be sent through a conversion to produce triples.
  b.  Optionally the n-ary extractions can be sent through a conversion to produce nested n-ary extractions.

## Concurrency

When operating at web scale, parallelism is essential.  While the base SRLIE extractor is immutable and
thread safe, the underlying SRL system provided by ClearNLP is not threadsafe.
