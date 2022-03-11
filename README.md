# Postmortem memory of public figures in news and social media

This repository contains code and data for the following paper ([available online](https://doi.org/10.1073/pnas.2106152118)):

> Robert West, Jure Leskovec, and Christopher Potts. **Postmortem memory of public figures in news and social media.** *Proceedings of the National Academy of Sciences,* September 2021, 118 (38) e2106152118.


The notebook that executes all analyses described in the main paper and in the Supplemental Information is `post_mortem_memory.Rmd`, which is also available as an [executed and rendered version](https://epfl-dlab.github.io/post-mortem-memory/post_mortem_memory.html).

Most data files are self-descriptive. The core files containing information about the mention frequency of deceased public figures are these:

* `num_dead_mentions_per_day_NEWS.tsv.gz` counts, for each person (column 1) and each day (column 2), the number of news articles (column 4) in which the person was mentioned *k* times (column 3) that day, where *k* can take on a value of 1, 2, or 3. Here, *k* = 3 means "at least 3 mentions per article", whereas *k* = 2 means "exactly 2 mentions per article" and *k* = 1 means "exactly 1 mention per article".
* `num_dead_mentions_per_day_TWITTER.tsv.gz` is analogous, but counts tweets, rather than news articles.

Additionally, a spreadsheet with time series plots and curve statistics for all 2,362 public figures is available [here](https://docs.google.com/spreadsheets/d/1QUfqYMGmrQ_edxTwhK6mIucZ-VGoa_8Bb59G5jh4BjM).
