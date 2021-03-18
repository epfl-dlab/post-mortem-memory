source(sprintf('%s/github/post-mortem-memory/code/load_common_data_and_functions.r', Sys.getenv('HOME')))

# We aggregate chunk_size days; a value of 7 means we aggregate by weeks.
CHUNK_SIZE <- 1

# The number of days we consider before and after death.
subrange_before <- -N:-30
subrange_peak <- 0:29
subrange_after <- 30:N

reldates <- as.character(-N:N)

# Normalize the time series to [0,1] and measure the fraction of time it takes until half of the
# total post-mortem volume has been seen. The simplest version maps the min to 0, but this is quite
# sensitive to low outliers, so we also allow for versions where we first set to 0 all values
# less than the specified quantile.
time_till_half <- function(y, thresh_quantile=0) {
  # Start with the day after death, since the news might not be reporting yet on the day of.
  y <- y[as.numeric(names(y)) >= 1]
  y <- pmax(y, quantile(y, thresh_quantile))
  y <- (y - min(y)) / (max(y) - min(y))
  min(which(cumsum(y) >= 0.5*sum(y))) / length(y)
}

###################################################################################################
# Get the data in shape
###################################################################################################

num_art_N <- get_num_art('NEWS')
num_art_T <- get_num_art('TWITTER')

data_N <- get_mention_freq_table('NEWS')
data_T <- get_mention_freq_table('TWITTER')

x_N <- get_rel_date_matrix('NEWS', data_N, num_art_N, CHUNK_SIZE)
x_T <- get_rel_date_matrix('TWITTER', data_T, num_art_T, CHUNK_SIZE)

mids_N <- filter_people('NEWS', x_N)
mids_T <- filter_people('TWITTER', x_T)
mids <- intersect(mids_N, mids_T)

x_N <- x_N[mids,]
x_T <- x_T[mids,]

X_N <- normalize_and_smooth('NEWS', x_N, num_art_N, mean_center=FALSE)
X_T <- normalize_and_smooth('TWITTER', x_T, num_art_T, mean_center=FALSE)

ymin_N <- log10(1 / max(num_art_N, na.rm=TRUE))
ymin_T <- log10(1 / max(num_art_T, na.rm=TRUE))

# Find the people that died (un)natural deaths.
mid_nat <- intersect(mids, names(which(get_cause_of_death_map()==1)))
mid_unnat <- intersect(mids, names(which(get_cause_of_death_map()==-1)))

# Choose if you want to deal with News or Twitter ('NEWS', 'TWITTER').
medium <- 'NEWS'
if (medium == 'NEWS') {
  x <- x_N[,colnames(x_N) %in% -N:N]
  X <- X_N
  ymin <- ymin_N
} else if (medium == 'TWITTER') {
  x <- x_T[,colnames(x_T) %in% -N:N]
  X <- X_T
  ymin <- ymin_T
} else {
  stop('Medium must be \'NEWS\' or \'TWITTER\'')
}

num_active <- rowSums(x > 0, na.rm=TRUE)
num_active_before <- rowSums(x[,colnames(x) %in% -N:-1] > 0, na.rm=TRUE)
num_active_after <- rowSums(x[,colnames(x) %in% 0:N] > 0, na.rm=TRUE)

num_finite_before <- rowSums(is.finite(x[,colnames(x) %in% -N:-1]))
num_finite_after <- rowSums(is.finite(x[,colnames(x) %in% 0:N]))

stats <- data.frame(mid=NA,
                    name=NA,
                    image=NA,
                    num_active_days=NA,
                    active_fraction_before=NA,
                    active_fraction_after=NA,
                    max_smoothed_before=NA,
                    max_raw_before=NA,
                    mean_before=NA,
                    mean_after=NA,
                    mean_after_peak=NA,
                    peak_smoothed=NA,
                    peak_raw=NA,
                    peak_raw_day=NA,
                    time_till_half=NA,
                    time_till_half_thresh=NA,
                    natural_death=NA,
                    unnatural_death=NA,
                    gender=NA,
                    cause_of_death=NA,
                    age=NA,
                    notable_type=NA
                    )[-1,]

for (i in 1:nrow(X)) {
#for (i in 1:10) {
  if (i %% 100 == 0) print(i)
  mid <- rownames(X)[i]
  # mid <- wiki_to_mid['<http://en.wikipedia.org/wiki/Johnny_Tapia>']
  name <- fancy_wiki(mid_to_wiki[mid])
  img_name <- if (!is.na(name)) gsub('(%(25)?..)+', '+', name) else sprintf('NA_%s', gsub('.*/m\\.(.*)>', '\\1', mid))
  raw <- log10(x[mid,])
  smoothed <- X[mid,]
  
  mean_before <- mean(smoothed[names(smoothed) %in% subrange_before], na.rm=TRUE)
  peak_smoothed <- max(smoothed[names(smoothed) %in% subrange_peak])
  peak_raw <- max(raw[names(raw) %in% subrange_peak])
  # -1 such that 0 represents day of death.
  peak_raw_day <- which.max(raw[names(raw) %in% subrange_peak]) - 1
  
  # png(sprintf('%s/plots/mention_freq_curves_%s/%s.png', BASEDIR, medium, img_name),
  #     width=200, height=200, pointsize=10)
  # par(mar=c(2,2,1,1))
  # suppressWarnings(
  #   plot(-N:N, raw, col='gray', main=name, xlab='Days since death', ylab='% mentioning docs',
  #        ylim=c(ymin,-1), panel.first=c(grid(), abline(v=0, col='gray')))
  #   )
  # abline(h=mean_before, col='green')
  # lines(names(smoothed), smoothed, col='black', lwd=2)
  # dev.off()
  stats[mid,] <- c(
    fancy_mid(mid),
    name,
    sprintf('=image("http://infolab.stanford.edu/~west1/death/mention_freq_curves/%s/%s.png")',
            medium, img_name),
    num_active[mid],
    num_active_before[mid]/num_finite_before[mid],
    num_active_after[mid]/num_finite_after[mid],
    max(smoothed[names(smoothed) %in% subrange_before], na.rm=TRUE),
    max(raw[names(raw) %in% subrange_before], na.rm=TRUE),
    mean_before,
    mean(smoothed[names(smoothed) %in% c(subrange_peak, subrange_after)]),
    mean(smoothed[names(smoothed) %in% subrange_after]),
    peak_smoothed,
    peak_raw,
    peak_raw_day,
    time_till_half(smoothed),
    time_till_half(smoothed, 0.25),
    if (mid %in% mid_nat) 1 else 0,
    if (mid %in% mid_unnat) 1 else 0,
    as.character(props[mid, 'gender']),
    as.character(props[mid, 'cause_of_death']),
    props[mid, 'age'],
    as.character(props[mid, 'notable_types'])
  )
}

# Write the data into a CSV file.
write.table(stats, sprintf('%s/mention_freq_for_spreadsheet_%s.csv', DATADIR, medium),
            quote=FALSE, sep='\t', row.names=FALSE)
