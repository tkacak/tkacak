# ============================================================
# 04_data_cleaning.R
# Quality control and analysis preparation
#
# The SAME cleaning rules are applied to the LLM samples and to
# the real participant data — otherwise the comparison is biased.
#
# Expected real data: data/real/real_data.csv
#   columns: i1, i2, ..., ik (raw, not reverse-scored)
# ============================================================

clean_sample <- function(data, source_label) {
  n_raw <- nrow(data)

  d <- data |>
    # 1) Set out-of-range values to NA
    mutate(across(all_of(item_names),
                  ~ ifelse(.x < config$likert_min | .x > config$likert_max, NA, .x))) |>
    # 2) Drop respondents with more than 10% missing
    filter(rowMeans(is.na(pick(all_of(item_names)))) <= .10)

  # 3) Careless-responding indices (careless package)
  mat <- as.matrix(d[item_names])
  d <- d |>
    mutate(
      longstring = careless::longstring(mat),  # longest identical-response run
      irv        = careless::irv(mat)          # intra-individual response variability
    ) |>
    # flag/drop straight-liners (same response on every item)
    filter(longstring < n_items)

  # 4) Reverse-score reverse-keyed items
  d <- d |>
    mutate(across(all_of(reverse_items),
                  ~ (config$likert_min + config$likert_max) - .x)) |>
    mutate(source = source_label)

  cat(sprintf("%-35s raw: %4d  clean: %4d\n", source_label, n_raw, nrow(d)))
  d
}

# ---- LLM samples --------------------------------------------------------
llm_files <- list.files("data/processed", pattern = "^llm_.*\\.csv$",
                        full.names = TRUE)
llm_samples <- map(llm_files, function(f) {
  label <- str_remove_all(basename(f), "^llm_|\\.csv$")
  read_csv(f, show_col_types = FALSE) |> clean_sample(label)
}) |> set_names(str_remove_all(basename(llm_files), "^llm_|\\.csv$"))

# ---- Real participant data ----------------------------------------------
real_path <- "data/real/real_data.csv"
if (file.exists(real_path)) {
  real_sample <- read_csv(real_path, show_col_types = FALSE) |>
    clean_sample("real")
} else {
  real_sample <- NULL
  warning("Real data not found (", real_path, "). ",
          "Comparison steps will be limited to the LLM samples only.")
}

all_samples <- c(llm_samples,
                 if (!is.null(real_sample)) list(real = real_sample))
write_rds(all_samples, "data/processed/all_samples.rds")
