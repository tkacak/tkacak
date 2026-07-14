# ============================================================
# 06_comparison.R
# LLM samples ↔ real participants comparison
#
#   A) Item level: mean differences (Cohen's d)
#   B) Correlation matrix similarity (RMSE + between-matrix r)
#   C) Factor similarity: Tucker's congruence coefficient (phi)
#   D) Measurement invariance: configural -> metric -> scalar MG-CFA
#   E) IRT parameter correlations + DIF screening
# ============================================================

analyses    <- readRDS("output/analyses.rds")
all_samples <- readRDS("data/processed/all_samples.rds")

stopifnot("real" %in% names(analyses))  # real data is required
real       <- analyses$real
llm_labels <- setdiff(names(analyses), "real")

compare_to_real <- function(label) {
  llm <- analyses[[label]]

  # ---- A) Item-level Cohen's d -------------------------------------------
  cohens_d <- map_dbl(item_names, function(it) {
    x <- all_samples[[label]][[it]]; y <- all_samples$real[[it]]
    (mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE)) /
      sqrt((var(x, na.rm = TRUE) + var(y, na.rm = TRUE)) / 2)
  })

  # ---- B) Correlation matrix similarity ----------------------------------
  lower  <- lower.tri(llm$correlations)
  r_llm  <- llm$correlations[lower]; r_real <- real$correlations[lower]
  cor_rmse <- sqrt(mean((r_llm - r_real)^2))
  cor_r    <- cor(r_llm, r_real)

  # ---- C) Tucker's congruence coefficient (on EFA loadings) --------------
  tucker <- psych::factor.congruence(llm$efa$loadings, real$efa$loadings)

  # ---- D) Measurement invariance (MG-CFA, WLSMV) --------------------------
  combined <- bind_rows(
    all_samples[[label]] |> select(all_of(item_names), source),
    all_samples$real     |> select(all_of(item_names), source)
  ) |> drop_na()

  configural <- cfa(cfa_model, data = combined, group = "source",
                    ordered = item_names, estimator = "WLSMV")
  metric     <- cfa(cfa_model, data = combined, group = "source",
                    ordered = item_names, estimator = "WLSMV",
                    group.equal = "loadings")
  scalar     <- cfa(cfa_model, data = combined, group = "source",
                    ordered = item_names, estimator = "WLSMV",
                    group.equal = c("loadings", "thresholds"))
  invariance <- semTools::compareFit(configural, metric, scalar)
  # Decision rule: ΔCFI <= .01 and ΔRMSEA <= .015 -> invariance supported

  # ---- E) IRT parameter similarity + DIF -----------------------------------
  a_r <- cor(llm$irt_params$a, real$irt_params$a)          # discrimination
  b_cols <- grep("^b", names(llm$irt_params), value = TRUE)
  b_r <- cor(unlist(llm$irt_params[b_cols]),
             unlist(real$irt_params[b_cols]))              # thresholds/difficulty

  mg <- mirt::multipleGroup(combined[item_names], model = 1,
                            group = combined$source, itemtype = "graded",
                            invariance = c("free_means", "free_var",
                                           item_names),
                            verbose = FALSE)
  dif <- mirt::DIF(mg, which.par = c("a1", "d1", "d2", "d3"),
                   scheme = "drop")   # free each item one at a time

  list(
    summary = tibble(
      model          = label,
      mean_abs_d     = mean(abs(cohens_d)),
      cor_matrix_rmse = cor_rmse,
      cor_matrix_r   = cor_r,
      tucker_phi     = tucker[1, 1],
      irt_a_r        = a_r,
      irt_b_r        = b_r,
      alpha_llm      = llm$reliability$alpha,
      alpha_real     = real$reliability$alpha
    ),
    cohens_d   = tibble(item = item_names, d = cohens_d),
    invariance = invariance,
    dif        = dif
  )
}

results <- map(llm_labels, compare_to_real) |> set_names(llm_labels)
saveRDS(results, "output/comparison.rds")

comparison_table <- map_dfr(results, "summary")
write_csv(comparison_table, "output/comparison_summary.csv")
print(comparison_table)

# ---- Figure: item means, LLM vs real -------------------------------------
plot_data <- imap_dfr(analyses, ~ .x$item_stats |> mutate(source = .y))
p <- plot_data |>
  mutate(item = factor(item, levels = item_names)) |>
  ggplot(aes(item, mean, group = source, colour = source)) +
  geom_line() + geom_point() +
  labs(title = "Item means: LLM samples vs. real participants",
       x = "Item", y = "Mean") +
  theme_minimal()
ggsave("output/item_means.png", p, width = 9, height = 5, dpi = 300)
