library(Matrix)

BASEDIR <- sprintf('%s/github/post-mortem-memory', Sys.getenv('HOME'))
DATADIR <- sprintf('%s/data', BASEDIR)

# The number of time units ('chunks') before and after death.
N <- 360

split_at <- function(str, sep) strsplit(str, sep)[[1]]

fancy_mid <- function(mid) sub('<http://rdf.freebase.com/ns/m.(.*)>', '/m/\\1', mid)
long_mid <- function(mid) sub('/m/(.*)', '<http://rdf.freebase.com/ns/m.\\1>', mid)

fancy_wiki <- function(wiki) sub('<http://en.wikipedia.org/wiki/(.*)>', '\\1', wiki)
long_wiki <- function(wiki) sprintf('<http://en.wikipedia.org/wiki/%s>', wiki)

FANCY_CURVE_CHAR_NAMES <- c('Pre-mortem mean', 'Short-term boost', 'Long-term boost', 'Halving time')
names(FANCY_CURVE_CHAR_NAMES) <- c('mean_before', 'peak_mean_boost', 'perm_boost', 'time_till_half')

# A list of all days.
MIN_DATE <- as.Date("2008-08-01")
MAX_DATE <- as.Date("2014-09-30")
DAYS <- as.character(seq(MIN_DATE, MAX_DATE, by="+1 day"))
MAX_ABS_RELDATE <- as.numeric(MAX_DATE - MIN_DATE)

# Our Twitter data set is reasonably large only from 2009-06-11 on.
TWITTER_START_DATE <- as.Date("2009-06-11")

# We build a matrix x that has a row per person, and a column per day since death. There are
# nreldates = 2*(MAX_DATE-MIN_DATE)+1 such possible dates: the extreme cases are someone dying on
# MIN_DATE or MAX_DATE, so the relDates are [0 : MAX_DATE-MIN_DATE] in the former case, and
# [MIN_DATE-MAX_DATE : 0] in the latter case, for a total range of [MIN_DATE-MAX_DATE :
# MAX_DATE-MIN_DATE]; there are 2*(MAX_DATE-MIN_DATE)+1 values in this range.
# Values of relDate can be negative, zero, or positive. However, since R indices must be
# positive, we must translate relDates to get indices; the translation is as such:
# idx = relDate + MAX_DATE - MIN_DATE + 1
NRELDATES <- 2 * as.numeric(MAX_DATE - MIN_DATE) + 1

# The days on which our Spinn3r client must have been broken (there are still several tens of
# thousands of articles with dates from these days, but they were crawled on days outside of this
# window).
EMPTY_DAYS <- c(
  "2009-05-19", "2009-07-15", "2010-04-05", "2010-04-20", "2010-04-21", "2010-04-22", "2010-04-23", "2010-04-24",
  "2010-04-25", "2010-04-26", "2010-04-27", "2010-04-28", "2010-04-29", "2010-04-30", "2010-05-01", "2010-05-02", 
  "2010-05-03", "2010-05-04", "2010-05-05", "2010-05-06", "2010-05-07", "2010-05-08", "2010-05-09", "2010-05-10",
  "2010-05-11", "2010-05-12", "2010-05-13", "2010-05-16", "2010-05-17", "2010-05-18", "2010-05-19", "2010-05-20",
  "2010-05-21", "2010-05-22", "2010-05-23", "2010-05-24", "2010-05-25", "2010-05-26", "2010-05-27", "2010-05-28",
  "2010-05-29", "2010-05-30", "2010-05-31", "2010-06-01", "2010-06-02", "2010-06-03", "2010-06-04", "2010-06-05",
  "2010-06-06", "2010-06-07", "2010-06-08", "2010-06-09", "2010-06-10", "2010-06-11", "2010-06-12", "2010-06-13",
  "2010-06-14", "2010-06-15", "2010-06-16", "2010-06-17", "2010-06-18", "2010-06-19", "2010-06-20", "2010-06-21",
  "2010-06-22", "2010-06-23", "2010-06-24", "2010-06-25", "2010-06-26", "2010-06-27", "2010-06-28", "2010-06-29",
  "2010-06-30", "2010-07-01", "2010-07-02", "2010-07-03", "2010-07-04", "2010-07-05", "2010-07-06", "2010-07-07",
  "2010-07-08", "2010-07-09", "2010-07-10", "2010-07-11", "2010-07-12", "2010-07-13", "2011-12-03")

# The dates of death.
death_dates_tbl <- read.table(pipe(sprintf('gunzip -c %s/date_of_death.nt', DATADIR)),
                              comment.char='', sep='\t', quote='',
                              col.names=c('mid', '_rel', 'date', '_period'))[,c('mid', 'date')]
death_dates_tbl <- death_dates_tbl[grepl('#date>$', death_dates_tbl$date),]
death_dates_tbl$date <- substring(death_dates_tbl$date, 2, 11)
death_dates <- death_dates_tbl$date
names(death_dates) <- death_dates_tbl$mid

# Our Twitter data set is reasonably large only from 2009-06-11 on.
# Get the mids of the people that died between then and MAX_DATE (Sept. 2014).
died_in_window <- names(death_dates[as.Date(death_dates) >= TWITTER_START_DATE
                                    & as.Date(death_dates) <= MAX_DATE])

# A mapping from mids to Wikipedia names.
wiki_tbl <- read.table(pipe(sprintf('gunzip -c %s/names.DEAD.UNAMBIGUOUS.tsv.gz', DATADIR)),
                       comment.char='', sep='\t', quote='',
                       col.names=c('mid', 'names', 'aliases', 'curid', 'wiki'),
                       stringsAsFactors=FALSE)
wiki_tbl$wiki <- sapply(wiki_tbl$wiki, function(x) split_at(x, '\\|')[1])
mid_to_wiki <- wiki_tbl$wiki
names(mid_to_wiki) <- wiki_tbl$mid
mid_to_wiki <- mid_to_wiki[!is.na(mid_to_wiki)]

# A mapping from mids to names.
wiki_to_mid <- names(mid_to_wiki)
names(wiki_to_mid) <- mid_to_wiki

# Properties of dead people.
props <- read.table(pipe(sprintf('gunzip -c %s/dead_people_properties.tsv.gz', DATADIR)),
                    comment.char='', sep='\t', quote='', header=TRUE, fill=TRUE,
                    col.names=c('person', 'cause_of_death', 'date_of_death', 'place_of_death', 'date_of_burial',
                                'place_of_burial', 'date_of_cremation', 'place_of_cremation', 'date_of_birth',
                                'place_of_birth', 'nationality', 'profession', 'religion', 'ethnicity',
                                'notable_types', 'gender'))
props$mid <- sub('(<.*>).*', '\\1', props$person)
props$gender <- as.factor(sub('<.*>(.*)', '\\1', props$gender))
death_years <- sub('"(....).*', '\\1', props$date_of_death)
birth_years <- sub('"(....).*', '\\1', props$date_of_birth)
death_years[!grepl("....", death_years)] <- NA
birth_years[!grepl("....", birth_years)] <- NA
props$age <- as.numeric(death_years) - as.numeric(birth_years)
rownames(props) <- props$mid

# Load the taxonomies.
tax_causes <- read.table(sprintf('%s/taxonomy_causes_of_death.tsv', DATADIR),
                         header=TRUE, sep='\t', comment.char='', quote='')
rownames(tax_causes) <- paste(tax_causes$mid, tax_causes$cause_of_death, sep='')
tax_types <- read.table(sprintf('%s/taxonomy_notable_types.tsv', DATADIR),
                        header=TRUE, sep='\t', comment.char='', quote='')
rownames(tax_types) <- paste(tax_types$mid, tax_types$notable_type, sep='')

# Some causes of death are missing from Janice's table. Add them manually.
natural_manual <- "Viral pneumonia|Smallpox|Dementia with Lewy bodies|Heart valve disease|Creutzfeldt–Jakob disease|T-Cell Lymphoma|Adrenocortical carcinoma|Huntington's disease|Congenital heart defect|Squamous-cell carcinoma|Atypical teratoid rhabdoid tumor|Alveolar rhabdomyosarcoma|Appendix cancer|Pyelonephritis|Polymyalgia rheumatica|Polycythemia|Leiomyosarcoma|Astrocytoma"
unnatural_manual <- "Smoke inhalation|Racing Accident|Lightning|Casualty of war|Cocaine overdose|Poisoning|Shootout|Murder–suicide|Accidental drug overdose|Blast injury"

# Create the regexes for (un)natural deaths.
natural_regex <- sprintf(">(%s|%s)($|\\|)",
                         paste(tax_causes$cause.of.death[tax_causes$level1=='natural'],
                               collapse='|'), natural_manual)
unnatural_regex <- sprintf(">(%s|%s)($|\\|)",
                           paste(tax_causes$cause.of.death[tax_causes$level1=='unnatural'],
                                 collapse='|'), unnatural_manual)

# -1 = unnatural, 0 = unknown/conflicting, 1 = natural.
get_cause_of_death_map <- function() {
  natural_death_mids <- props$mid[which(grepl(natural_regex, props$cause_of_death))]
  unnatural_death_mids <- props$mid[which(grepl(unnatural_regex, props$cause_of_death))]
  map <- (props$mid %in% natural_death_mids) - (props$mid %in% unnatural_death_mids)
  # If a person has a natural and an unnatural cause, we want to list them under unnatural death.
  map[props$mid %in% natural_death_mids & props$mid %in% unnatural_death_mids] <- -1
  names(map) <- props$mid
  return(map)
}

# e.g., <http://rdf.freebase.com/ns/m.025698c>Baseball Player --> <http://rdf.freebase.com/ns/m.025698c>
strip_plaintext_name_from_mid <- function(mid_with_name) {
  sub('(<.*>).*', '\\1', mid_with_name)
}

# tax must be either tax_types or tax_causes;
# data must be a vector with taxonomy entries as values and mids as names.
select_from_tax <- function(level, value, tax, data) {
  col <- sprintf('level%d', level)
  ok_values <- rownames(tax)[tax[,col] == value]
  names(data)[data %in% ok_values]
}

get_num_art <- function(medium) {
  medium <- toupper(medium)
  if (medium != 'NEWS' && medium != 'TWITTER') {
    stop('Medium must be \'NEWS\' or \'TWITTER\'')
  }
  num_art_tbl <- read.table(sprintf('%s/num_articles_per_day_%s.tsv', DATADIR, medium),
                            col.names=c('date', 'num'))
  num_art <- num_art_tbl$num
  names(num_art) <- num_art_tbl$date
  num_art <- num_art[names(num_art) >= as.character(MIN_DATE) & names(num_art) <= as.character(MAX_DATE)]
  num_art[setdiff(DAYS, names(num_art))] <- 0
  num_art <- num_art[order(names(num_art))]
  num_art[EMPTY_DAYS] <- NA
  return(num_art)
}

get_mention_freq_table <- function(medium) {
  medium <- toupper(medium)
  if (medium != 'NEWS' && medium != 'TWITTER') {
    stop('Medium must be \'NEWS\' or \'TWITTER\'')
  }
  file <- sprintf('%s/RData/dead_people_mentions_%s.RData', DATADIR, medium)
  if (!file.exists(file)) {
    data <- read.table(pipe(sprintf('gunzip -c %s/num_dead_mentions_per_day_%s.tsv.gz', DATADIR, medium)),
                       comment.char='', sep='\t', quote='',
                       col.names=c('mid', 'date', 'num_per_doc', 'num_doc'),
                       colClasses=c('character', 'character', 'numeric', 'numeric'))
    # Add a column having the number of days since death.
    data$rel_date <- as.numeric(as.Date(data$date) - as.Date(death_dates[data$mid]))
    save(data, file=file)
  } else {
    load(file)
  }
  return(data)
}

get_rel_date_matrix <- function(medium, data, num_art, chunk_size) {
  medium <- toupper(medium)
  chunks <- floor(((1:NRELDATES)-(NRELDATES+1)/2)/chunk_size)
  sum.na.rm <- function(x) sum(x, na.rm=TRUE)
  if (medium == 'NEWS') {
    min_num_per_doc <- 2
  } else if (medium == 'TWITTER') {
    min_num_per_doc <- 1
  } else {
    stop('Medium must be \'NEWS\' or \'TWITTER\'')
  }
  idx <- which(data$num_per_doc >= min_num_per_doc)
  file <- sprintf('%s/RData/num_mentions_per_rel_date_%s_min_num_per_doc=%s_chunk_size=%s.RData',
                  DATADIR, medium, min_num_per_doc, chunk_size)
  if (!file.exists(file)) {
    x <- do.call(rbind, mclapply(split(data[idx,], data$mid[idx]), function(l) {
      dod <- as.Date(death_dates[l$mid[1]])
      # Aggregate the counts of docs containing 1, 2, >=3 mentions.
      l <- data.frame(t(simplify2array(by(l, l$date, function(X) c(
        as.numeric(as.character(X$rel_date[1])),
        as.numeric(as.character(sum(X$num_doc))))))))
      colnames(l) <- c('rel_date', 'num_doc')
      l$date <- rownames(l)
      # Initialize y, the count vector for the current mid.
      # y has 2*(MAX_DATE-MIN_DATE)+1 entries, of which only MAX_DATE-MIN_DATE+1 are well-defined (as per
      # our discussion of NRELDATES in load_common_data_and_functions.r).
      # For the MAX_DATE-MIN_DATE days for which we have no data, the values are set to NA.
      # We also set to NA the values for the days that have no Spinn3r data (primarily the 2010 hole).
      # For the remaining MAX_DATE-MIN_DATE+1 days, we initialize values to 0.
      offset <- as.numeric(MAX_DATE-MIN_DATE) + 1
      y <- rep(NA, NRELDATES)
      y[as.numeric(as.Date(DAYS)-dod) + offset] <- 0
      y[l$rel_date + offset] <- l$num_doc
      y[as.numeric(as.Date(EMPTY_DAYS)-dod) + offset] <- NA
      # n: the number of docs per day aligned in terms of relative dates.
      n <- rep(0, NRELDATES)
      n[as.numeric(as.Date(DAYS)-dod) + offset] <- num_art
      # Sum within chunks.
      s <- tapply(y, chunks, sum.na.rm) / tapply(n, chunks, sum.na.rm)
      # Set to NA the days before TWITTER_START_DATE in the case of Twitter.
      if (medium == 'twitter') {
        s[as.numeric(as.Date(DAYS[as.Date(DAYS) < TWITTER_START_DATE])-dod) + offset] <- NA
      }
      return(s)
    }))
    x <- as.matrix(x)
    rownames(x) <- unique(data$mid[idx])
    colnames(x) <- as.character(unique(chunks))
    save(x, file=file)
  } else {
    load(file)
  }
  return(x)
}

filter_people <- function(medium, x) {
  N_immed_after <- 100
  num_finite_before <- rowSums(is.finite(x[,colnames(x) %in% -N:-1]))
  num_finite_immed_after <- rowSums(is.finite(x[,colnames(x) %in% 0:(N_immed_after-1)]))
  num_active_before <- rowSums(x[,colnames(x) %in% -N:-1] > 0, na.rm=TRUE)
  mids <- names(which(
    # Keep only people whose boundaries aren't missing.
    is.finite(x[,colnames(x)==N]) & is.finite(x[,colnames(x)==-N])
    # Keep only people that have no missing data in the N_immed_after days immediately after death.
    & num_finite_immed_after == N_immed_after
    # Keep only people that were mentioned on at least 5 days before they died.
    & num_active_before >= 5
    # Keep only people that died after the point from which on we have a lot of Twitter data.
    & rownames(x) %in% died_in_window
    # Discard people with parentheses in their names because those names, although unique in
    # Wikipedia, are unlikely to ever be used in real prose.
    & !grepl('\\(', mid_to_wiki[rownames(x)])))
  return(mids)
}

# Apply Friedman's super smoother, hell yeah.
supersmooth <- function(y) {
  reldates <- as.numeric(names(y))
  suppressWarnings({
    smoothed_left <- supsmu(reldates[reldates<0], y[reldates<0])
    smoothed_right <- supsmu(reldates[reldates>=0], y[reldates>=0])
  })
  smoothed <- c(smoothed_left$y, smoothed_right$y)
  names(smoothed) <- c(smoothed_left$x, smoothed_right$x)
  return(smoothed)
}

normalize_and_smooth <- function(medium, x, num_art, mean_center=TRUE) {
  medium <- toupper(medium)
  # Smoothing term: we add 1 mention (as computed on the highest-volume day) to each day.
  eps <- 1 / max(num_art, na.rm=TRUE)
  if (medium != 'NEWS' && medium != 'TWITTER') {
    stop('Medium must be \'NEWS\' or \'TWITTER\'')
  }
  file <- sprintf('%s/RData/clustering_input_%s%s.RData', DATADIR, medium, if (mean_center) '_MEANCENTERED' else '')
  if (!file.exists(file)) {
    # Select the subset of reldates; keep a month of padding, so smoothing is more robust at the
    # boundaries,
    X <- x[,colnames(x) %in% -(N+30):(N+30)]
    reldates <- as.numeric(colnames(X))
    # Add-eps smoothing.
    X <- X + eps
    # Normalize and smooth all time series.
    X <- t(apply(X, 1, function(y_unnorm) {
      # Log10-transform.
      y <- log10(y_unnorm)
      # Smooth.
      y <- supersmooth(y)
      # Subtract the pre-mortem mean. Now we have log10(y_unnorm/mean*(y_unnorm)), where mean* is the
      # exponential of the mean in log space (i.e., a more robust version of the mean).
      # In the mean computation we exclude the 30 days immediately before death, to mitigate the
      # impact of the final sick days for people whose death was foreseeable.
      if (mean_center) y <- y - mean(y[reldates %in% -N:-30], na.rm=TRUE)
      # Select the subset of reldates.
      y <- y[names(y) %in% -N:N]
      # Interpolate missing values; rule=2 means that at the boundaries the values at the boundaries are
      # imputed for missing values. (But since we require our time series not to end in a missing value,
      # this shouldn't happen in the post-mortem period.)
      y <- approx(names(y), y, -N:N, rule=2)$y
      return(y)
    }))
    colnames(X) <- -N:N
    save(X, file=file)
  } else {
    load(file)
  }
  return(X)
}
