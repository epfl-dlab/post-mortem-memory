# Post-mortem memory of public figures in news and social media

This repository contains code and data for the paper "Post-mortem memory of public figures in news and social media" by Robert West, Jure Leskovec, and Christopher Potts.

The main notebook that executes all analyses described in the main paper and in the Supplemental Information is [`code/post_mortem_memory.Rmd`](code/post_mortem_memory.Rmd), which is also available as an [executed and rendered version](https://epfl-dlab.github.io/post-mortem-memory/code/post_mortem_memory.html).

Most data files are self-descriptive. The core files containing information about the mention frequency of deceased public figures are these:

* `num_dead_mentions_per_day_TWITTER.tsv.gz` counts, for each person (column 1) and each day (column 2), the number of tweets (column 4) in which the person was mentioned *k* times (column 3) that day, where *k* can take on a value of 1, 2, or 3. Note that a value of *k*=3 refers to the count of tweets in which the person was mentioned *at least* (not exactly) 3 times.
