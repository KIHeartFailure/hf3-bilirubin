# default is to use tidyverse functions
select <- dplyr::select
rename <- dplyr::rename
filter <- dplyr::filter
mutate <- dplyr::mutate
complete <- tidyr::complete

# used for calculation of ci
global_z05 <- qnorm(1 - 0.025)

tmpcols <- RColorBrewer::brewer.pal(11, "BrBG")
global_cols <- tmpcols[c(1, 11, 2, 10, 3, 9, 4, 8, 5, 7, 6)]
global_shapes <- c(19, 15, 18, 17)
global_size <- c(3.5, 3.5, 5, 3.5)
