# ============================================================
# 04_veri_temizleme.R
# Kalite kontrol ve analize hazırlık
#
# Hem LLM örneklemlerine hem de gerçek katılımcı verisine AYNI
# temizlik kuralları uygulanır — aksi hâlde karşılaştırma yanlı olur.
#
# Gerçek veri beklentisi: data/gercek/gercek_veri.csv
#   sütunlar: m1, m2, ..., mk (ham, ters çevrilmemiş yanıtlar)
# ============================================================

temizle <- function(veri, kaynak_etiketi) {
  ham_n <- nrow(veri)

  v <- veri |>
    # 1) Aralık dışı değerleri NA yap
    mutate(across(all_of(madde_adlari),
                  ~ ifelse(.x < config$likert_alt | .x > config$likert_ust, NA, .x))) |>
    # 2) Eksik oranı %10'u aşan yanıtlayıcıyı çıkar
    filter(rowMeans(is.na(pick(all_of(madde_adlari)))) <= .10)

  # 3) Dikkatsiz yanıt göstergeleri (careless paketi)
  mat <- as.matrix(v[madde_adlari])
  v <- v |>
    mutate(
      longstring = careless::longstring(mat),          # en uzun ayni-yanit dizisi
      irv        = careless::irv(mat)                  # birey-içi standart sapma
    ) |>
    # düz-çizgi yanıtlayanları (tüm maddelerde aynı yanıt) işaretle/çıkar
    filter(longstring < n_madde)

  # 4) Ters maddeleri puanla
  v <- v |>
    mutate(across(all_of(ters_maddeler),
                  ~ (config$likert_alt + config$likert_ust) - .x)) |>
    mutate(kaynak = kaynak_etiketi)

  cat(sprintf("%-35s ham: %4d  temiz: %4d\n", kaynak_etiketi, ham_n, nrow(v)))
  v
}

# ---- LLM örneklemleri ---------------------------------------------------
llm_dosyalar <- list.files("data/islenmis", pattern = "^llm_.*\\.csv$",
                           full.names = TRUE)
llm_verileri <- map(llm_dosyalar, function(d) {
  etiket <- str_remove_all(basename(d), "^llm_|\\.csv$")
  read_csv(d, show_col_types = FALSE) |> temizle(etiket)
}) |> set_names(str_remove_all(basename(llm_dosyalar), "^llm_|\\.csv$"))

# ---- Gerçek katılımcı verisi -------------------------------------------
gercek_yolu <- "data/gercek/gercek_veri.csv"
if (file.exists(gercek_yolu)) {
  gercek_verisi <- read_csv(gercek_yolu, show_col_types = FALSE) |>
    temizle("gercek")
} else {
  gercek_verisi <- NULL
  warning("Gerçek veri bulunamadı (", gercek_yolu, "). ",
          "Karşılaştırma adımları yalnızca LLM örneklemleriyle sınırlı kalır.")
}

tum_orneklemler <- c(llm_verileri,
                     if (!is.null(gercek_verisi)) list(gercek = gercek_verisi))
write_rds(tum_orneklemler, "data/islenmis/tum_orneklemler.rds")
