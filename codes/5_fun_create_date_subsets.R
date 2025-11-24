# function(s) related to bootstrapping

# split dates into approximately equal size

fun_cutdates_subsets = function(dates, n_subsets){

	dates <- as.Date(dates)
	if (any(is.na(dates))) {
	  cat("Removing", sum(is.na(dates)), "NA dates.\n")
	  dates <- dates[!is.na(dates)]
	}

	# Sort dates (chronologically)
	dates_sorted <- sort(dates)

	n <- length(dates_sorted)
	n_target <- ceiling(n / n_subsets)

	# ------------------- FIND ROW INDICES FOR CUTS -------------------
	# We want n_subsets - 1 cut points at rows: n_target, 2*n_target, ...
	cut_rows <- seq(n_target, n, by = n_target)
	cut_rows <- cut_rows[1:(n_subsets - 1)]  # exclude the last (n+1)

	# Find actual row indices closest to ideal cuts
	cut_indices <- sapply(cut_rows, function(r) which.min(abs(seq_along(dates_sorted) - r)))

	# Get the **dates** at those cut points
	cut_dates <- dates_sorted[cut_indices]

	# Full break vector: start, cuts, end+1
	break_dates <- c(min(dates_sorted), cut_dates, max(dates_sorted) + 1)

	return(break_dates)
} # fun_cutdates_subsets
