# ============================================================
# 06_karsilastirma.R
# LLM örneklemleri ↔ gerçek katılımcılar karşılaştırması
#
#   A) Madde düzeyi: ortalama farkları (Cohen d)
#   B) Korelasyon matrisi benzerliği (RMSE + matrisler arası r)
#   C) Faktör benzerliği: Tucker'ın uyum katsayısı (phi)
#   D) Ölçme değişmezliği: konfigüral -> metrik -> skaler ÇG-DFA
#   E) MTK parametre korelasyonları + DIF taraması
# ============================================================

analizler       <- read_rds("cikti/analizler.rds")
tum_orneklemler <- read_rds("data/islenmis/tum_orneklemler.rds")

stopifnot("gercek" %in% names(analizler))  # gerçek veri şart
gercek  <- analizler$gercek
llm_adlari <- setdiff(names(analizler), "gercek")

karsilastir <- function(ad) {
  llm <- analizler[[ad]]

  # ---- A) Madde düzeyinde Cohen d --------------------------------------
  d_cohen <- map_dbl(madde_adlari, function(md) {
    x <- tum_orneklemler[[ad]][[md]]; y <- tum_orneklemler$gercek[[md]]
    (mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE)) /
      sqrt((var(x, na.rm = TRUE) + var(y, na.rm = TRUE)) / 2)
  })

  # ---- B) Korelasyon matrisi benzerliği --------------------------------
  alt   <- lower.tri(llm$korelasyon)
  r_llm <- llm$korelasyon[alt]; r_ger <- gercek$korelasyon[alt]
  kor_rmse <- sqrt(mean((r_llm - r_ger)^2))
  kor_r    <- cor(r_llm, r_ger)

  # ---- C) Tucker'ın uyum katsayısı (AFA yükleri üzerinden) --------------
  tucker <- psych::factor.congruence(llm$afa$loadings, gercek$afa$loadings)

  # ---- D) Ölçme değişmezliği (ÇG-DFA, WLSMV) ----------------------------
  birlesik <- bind_rows(
    tum_orneklemler[[ad]]   |> select(all_of(madde_adlari), kaynak),
    tum_orneklemler$gercek  |> select(all_of(madde_adlari), kaynak)
  ) |> drop_na()

  konfigural <- cfa(dfa_modeli, data = birlesik, group = "kaynak",
                    ordered = madde_adlari, estimator = "WLSMV")
  metrik     <- cfa(dfa_modeli, data = birlesik, group = "kaynak",
                    ordered = madde_adlari, estimator = "WLSMV",
                    group.equal = "loadings")
  skaler     <- cfa(dfa_modeli, data = birlesik, group = "kaynak",
                    ordered = madde_adlari, estimator = "WLSMV",
                    group.equal = c("loadings", "thresholds"))
  degismezlik <- semTools::compareFit(konfigural, metrik, skaler)
  # Karar ölçütü: ΔCFI <= .01 ve ΔRMSEA <= .015 -> değişmezlik desteklenir

  # ---- E) MTK parametre benzerliği + DIF --------------------------------
  a_r <- cor(llm$mtk_param$a, gercek$mtk_param$a)          # ayırt edicilik
  b_kolon <- grep("^b", names(llm$mtk_param), value = TRUE)
  b_r <- cor(unlist(llm$mtk_param[b_kolon]),
             unlist(gercek$mtk_param[b_kolon]))            # eşik/güçlük

  cg <- mirt::multipleGroup(birlesik[madde_adlari], model = 1,
                            group = birlesik$kaynak, itemtype = "graded",
                            invariance = c("free_means", "free_var",
                                           madde_adlari),
                            verbose = FALSE)
  dif <- mirt::DIF(cg, which.par = c("a1", "d1", "d2", "d3"),
                   scheme = "drop")   # her maddeyi tek tek serbest bırak

  list(
    ozet = tibble(
      model            = ad,
      ort_mutlak_d     = mean(abs(d_cohen)),
      kor_matrisi_rmse = kor_rmse,
      kor_matrisi_r    = kor_r,
      tucker_phi       = tucker[1, 1],
      mtk_a_r          = a_r,
      mtk_b_r          = b_r,
      alpha_llm        = llm$guvenirlik$alpha,
      alpha_gercek     = gercek$guvenirlik$alpha
    ),
    d_cohen = tibble(madde = madde_adlari, d = d_cohen),
    degismezlik = degismezlik,
    dif = dif
  )
}

sonuclar <- map(llm_adlari, karsilastir) |> set_names(llm_adlari)
write_rds(sonuclar, "cikti/karsilastirma.rds")

karsilastirma_tablosu <- map_dfr(sonuclar, "ozet")
write_csv(karsilastirma_tablosu, "cikti/karsilastirma_ozeti.csv")
print(karsilastirma_tablosu)

# ---- Görsel: madde ortalamaları LLM vs gerçek ---------------------------
grafik_verisi <- imap_dfr(analizler, ~ .x$madde_ist |> mutate(kaynak = .y))
p <- grafik_verisi |>
  mutate(madde = factor(madde, levels = madde_adlari)) |>
  ggplot(aes(madde, ortalama, group = kaynak, colour = kaynak)) +
  geom_line() + geom_point() +
  labs(title = "Madde ortalamaları: LLM örneklemleri ve gerçek katılımcılar",
       x = "Madde", y = "Ortalama") +
  theme_minimal()
ggsave("cikti/madde_ortalamalari.png", p, width = 9, height = 5, dpi = 300)
