# ============================================================
# 03_llm_veri_toplama.R
# Her persona × her LLM için ölçek yanıtlarının toplanması
#
# - Yapılandırılmış çıktı (chat_structured) kullanılır: model her
#   madde için 1..likert_ust aralığında bir tamsayı döndürmek
#   zorundadır -> ayrıştırma hatası neredeyse sıfır.
# - Madde sırası her personada seçkisizlenir (sıra etkisini kırmak için)
#   ve yanıtlar özgün madde numarasına geri eşlenir.
# - Her modelin ham yanıtları data/ham/ altına anında yazılır;
#   kesinti olursa kaldığı yerden devam eder.
# ============================================================

# ---- Yanıt şeması: madde_1 ... madde_k, her biri tamsayı ---------------
yanit_semasi <- do.call(type_object, c(
  list(.description = "Ankete verilen yanıtlar"),
  setNames(
    replicate(n_madde,
      type_integer(paste0("Yanıt: ", config$likert_alt, "-", config$likert_ust,
                          " arası tamsayı")),
      simplify = FALSE),
    paste0("madde_", seq_len(n_madde))  # sunum sırasına göre 1..k
  )
))

# ---- Tek persona için yanıt al -----------------------------------------
yanit_al <- function(sohbet, p, madde_sirasi) {
  istem <- persona_istemi_olustur(p, madde_sirasi)
  ham <- sohbet$clone()$chat_structured(istem, type = yanit_semasi)

  # Sunum sırasındaki yanıtları özgün madde numarasına geri eşle
  sunum_yaniti <- unlist(ham[paste0("madde_", seq_len(n_madde))])
  yanit <- integer(n_madde)
  yanit[madde_sirasi] <- sunum_yaniti
  setNames(as.list(yanit), madde_adlari)
}

# ---- Ana döngü: model × persona ----------------------------------------
set.seed(config$tohum)
madde_siralari <- map(seq_len(nrow(personalar)),
                      ~ sample(olcek$maddeler$madde_no))

for (j in seq_len(nrow(config$modeller))) {
  m <- config$modeller[j, ]
  etiket <- paste0(m$saglayici, "_", gsub("[^a-zA-Z0-9._-]", "-", m$model))
  ham_dosya <- file.path("data/ham", paste0(etiket, ".rds"))

  # Kaldığı yerden devam: daha önce toplanan yanıtları yükle
  sonuclar <- if (file.exists(ham_dosya)) read_rds(ham_dosya) else list()
  sohbet <- olustur_sohbet(m$saglayici, m$model)
  cat("\n==>", etiket, "| mevcut:", length(sonuclar), "/", nrow(personalar), "\n")

  for (i in seq_len(nrow(personalar))) {
    p <- personalar[i, ]
    if (!is.null(sonuclar[[p$persona_id]])) next  # zaten toplandı

    kayit <- tryCatch(
      c(list(persona_id = p$persona_id, model = etiket,
             zaman = as.character(Sys.time())),
        yanit_al(sohbet, p, madde_siralari[[i]])),
      error = function(e) {
        message("HATA [", p$persona_id, "]: ", conditionMessage(e))
        NULL
      }
    )

    if (!is.null(kayit)) {
      sonuclar[[p$persona_id]] <- kayit
      write_rds(sonuclar, ham_dosya)  # anında kalıcılaştır
    }
    if (i %% 25 == 0) cat("  ", i, "persona tamamlandı\n")
    Sys.sleep(0.3)  # hız sınırı (rate limit) tamponu
  }

  # Uzun formatta olmayan, analize hazır geniş tablo
  bind_rows(sonuclar) |>
    write_csv(file.path("data/islenmis", paste0("llm_", etiket, ".csv")))
}

# İpucu: ellmer >= 0.2'de parallel_chat_structured() ile istekler
# toplu ve eşzamanlı gönderilebilir (çok daha hızlı, ama kaldığı
# yerden devam mantığını kendiniz kurmanız gerekir):
#   istemler <- map2(seq_len(nrow(personalar)), madde_siralari,
#                    ~ persona_istemi_olustur(personalar[.x, ], .y))
#   yanitlar <- parallel_chat_structured(sohbet, istemler, type = yanit_semasi)
