source(sprintf('%s/github/post-mortem-memory/code/load_common_data_and_functions.r', Sys.getenv('HOME')))



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

