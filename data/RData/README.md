When running the main notebook for the first time, it will create serialized, binary data files in this directory. Subsequent runs will then read data from the serialized files, which is much faster than reading data from the raw .tsv files every time.

