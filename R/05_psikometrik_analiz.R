# ============================================================
# 05_psikometrik_analiz.R
# Her örneklem (her LLM + gerçek katılımcılar) için ayrı ayrı:
#   1) Madde analizi        3) Gizil yapı (paralel analiz, AFA, DFA)
#   2) Güvenirlik (α, ω)    4) MTK: Aşamalı Tepki Modeli (GRM)
# ============================================================

psikometrik_ozet <- function(veri, etiket) {
  mat <- veri[madde_adlari] |> drop_na()
  cat("\n########", etiket, "( n =", nrow(mat), ") ########\n")

  # ---- 1) Madde analizi -------------------------------------------------
  madde_ist <- psych::describe(mat) |>
    as_tibble(rownames = "madde") |>
    select(madde, ortalama = mean, ss = sd, carpiklik = skew, basiklik = kurtosis)

  mt <- psych::alpha(mat)  # madde-toplam korelasyonları da buradan
  madde_ist$madde_toplam_r <- mt$item.stats$r.drop

  # ---- 2) Güvenirlik ----------------------------------------------------
  omega_sonuc <- psych::omega(mat, nfactors = 1, plot = FALSE)
  guvenirlik <- tibble(
    kaynak      = etiket,
    alpha       = mt$total$raw_alpha,
    omega_total = omega_sonuc$omega.tot
  )

  # ---- 3) Gizil yapı ----------------------------------------------------
  # 3a. Boyut sayısı: paralel analiz (polikorik korelasyonla)
  pa <- psych::fa.parallel(mat, cor = "poly", fa = "fa", plot = FALSE)

  # 3b. AFA: paralel analizin önerdiği faktör sayısıyla, oblimin döndürme
  afa <- psych::fa(mat, nfactors = max(1, pa$nfact), rotate = "oblimin",
                   fm = "ml", cor = "poly")

  # 3c. DFA: kuramsal model, sıralı (ordinal) veri için WLSMV
  dfa <- lavaan::cfa(dfa_modeli, data = mat, ordered = madde_adlari,
                     estimator = "WLSMV")
  uyum <- lavaan::fitMeasures(dfa, c("chisq.scaled", "df", "cfi.scaled",
                                     "tli.scaled", "rmsea.scaled", "srmr"))

  # ---- 4) MTK: Aşamalı Tepki Modeli -------------------------------------
  grm <- mirt::mirt(mat, model = 1, itemtype = "graded", verbose = FALSE)
  mtk_param <- mirt::coef(grm, IRTpars = TRUE, simplify = TRUE)$items |>
    as_tibble(rownames = "madde")   # a (ayırt edicilik) ve b'ler (eşikler)

  list(etiket = etiket, n = nrow(mat), madde_ist = madde_ist,
       guvenirlik = guvenirlik, paralel_nfakt = pa$nfact, afa = afa,
       dfa = dfa, dfa_uyum = uyum, grm = grm, mtk_param = mtk_param,
       korelasyon = cor(mat))
}

tum_orneklemler <- read_rds("data/islenmis/tum_orneklemler.rds")
analizler <- imap(tum_orneklemler, ~ psikometrik_ozet(.x, .y))
write_rds(analizler, "cikti/analizler.rds")

# ---- Örneklemler arası özet tablosu ------------------------------------
ozet_tablo <- map_dfr(analizler, function(a) {
  a$guvenirlik |>
    mutate(n = a$n,
           onerilen_faktor = a$paralel_nfakt,
           cfi   = a$dfa_uyum[["cfi.scaled"]],
           rmsea = a$dfa_uyum[["rmsea.scaled"]],
           srmr  = a$dfa_uyum[["srmr"]])
})
write_csv(ozet_tablo, "cikti/ozet_psikometri.csv")
print(ozet_tablo)
