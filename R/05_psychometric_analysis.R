# ============================================================
# 05_psychometric_analysis.R
# For each sample (each LLM + real participants), separately:
#   1) Item analysis        3) Latent structure (parallel analysis, EFA, CFA)
#   2) Reliability (α, ω)   4) IRT: Graded Response Model (GRM)
# ============================================================

psychometric_summary <- function(data, label) {
  mat <- data[item_names] |> drop_na()
  cat("\n########", label, "( n =", nrow(mat), ") ########\n")

  # ---- 1) Item analysis -------------------------------------------------
  item_stats <- psych::describe(mat) |>
    as_tibble(rownames = "item") |>
    select(item, mean, sd, skewness = skew, kurtosis)

  rel <- psych::alpha(mat)  # also provides corrected item-total correlations
  item_stats$item_total_r <- rel$item.stats$r.drop

  # ---- 2) Reliability -----------------------------------------------------
  omega_res <- psych::omega(mat, nfactors = 1, plot = FALSE)
  reliability <- tibble(
    source      = label,
    alpha       = rel$total$raw_alpha,
    omega_total = omega_res$omega.tot
  )

  # ---- 3) Latent structure ------------------------------------------------
  # 3a. Number of dimensions: parallel analysis (on polychoric correlations)
  pa <- psych::fa.parallel(mat, cor = "poly", fa = "fa", plot = FALSE)

  # 3b. EFA: with the number of factors suggested by parallel analysis,
  #     oblimin rotation
  efa <- psych::fa(mat, nfactors = max(1, pa$nfact), rotate = "oblimin",
                   fm = "ml", cor = "poly")

  # 3c. CFA: theoretical model, WLSMV estimator for ordinal data
  cfa_fit <- lavaan::cfa(cfa_model, data = mat, ordered = item_names,
                         estimator = "WLSMV")
  fit_indices <- lavaan::fitMeasures(cfa_fit, c("chisq.scaled", "df",
                                                "cfi.scaled", "tli.scaled",
                                                "rmsea.scaled", "srmr"))

  # ---- 4) IRT: Graded Response Model ---------------------------------------
  grm <- mirt::mirt(mat, model = 1, itemtype = "graded", verbose = FALSE)
  irt_params <- mirt::coef(grm, IRTpars = TRUE, simplify = TRUE)$items |>
    as_tibble(rownames = "item")   # a (discrimination) and b's (thresholds)

  list(label = label, n = nrow(mat), item_stats = item_stats,
       reliability = reliability, parallel_nfact = pa$nfact, efa = efa,
       cfa = cfa_fit, cfa_fit_indices = fit_indices, grm = grm,
       irt_params = irt_params, correlations = cor(mat))
}

all_samples <- read_rds("data/processed/all_samples.rds")
analyses <- imap(all_samples, ~ psychometric_summary(.x, .y))
write_rds(analyses, "output/analyses.rds")

# ---- Cross-sample summary table ------------------------------------------
summary_table <- map_dfr(analyses, function(a) {
  a$reliability |>
    mutate(n = a$n,
           suggested_factors = a$parallel_nfact,
           cfi   = a$cfa_fit_indices[["cfi.scaled"]],
           rmsea = a$cfa_fit_indices[["rmsea.scaled"]],
           srmr  = a$cfa_fit_indices[["srmr"]])
})
write_csv(summary_table, "output/psychometric_summary.csv")
print(summary_table)
