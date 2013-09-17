SRLIE
=====
SRLIE is a component of Open IE 4.0 that automatically identifies n-ary extractions from English sentences.
SRLIE is designed for Web-scale information extraction, where target relations are not specified in advance.

SRLIE constructs richer extractions than OLLIE.  It builds extractions from Semantic Role Labelling (SRL)
SRL frames are not extractions themselves because they contain many frame arguments that are not considered
extraction arguments in Open IE.  SRL also does not identify argument boundaries or expand relation verbs
into relation phrases.

## Concurrency

When operating at web scale, parallelism is essential.  While the base SRLIE extractor is immutable and
thread safe, the underlying SRL system provided by ClearNLP is not threadsafe.
