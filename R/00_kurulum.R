# ============================================================
# 00_kurulum.R
# Paketler ve çalışma yapılandırması
# ============================================================

library(tidyverse)   # veri işleme
library(ellmer)      # R'dan LLM çağrıları (Anthropic, OpenAI, Gemini, Ollama...)
library(psych)       # madde analizi, alfa/omega, AFA, paralel analiz, Tucker phi
library(lavaan)      # DFA ve ölçme değişmezliği
library(semTools)    # model karşılaştırma yardımcıları (compareFit)
library(mirt)        # madde tepki kuramı (Aşamalı Tepki Modeli), DIF
library(careless)    # dikkatsiz yanıt göstergeleri (longstring, IRV)
library(jsonlite)    # ham yanıtları saklamak için

# ---- Yapılandırma ------------------------------------------------------
config <- list(
  tohum              = 2026,   # tekrarlanabilirlik için
  n_persona          = 300,    # simülatif birey sayısı (gerçek örnekleme eşleyin!)
  likert_alt         = 1,
  likert_ust         = 4,      # örnek ölçek (RBSÖ) 4'lü Likert
  sicaklik           = 1.0,    # varyansı kısmamak için 1 önerilir
  ozellik_tohumu_kullan = FALSE, # TRUE: personaya özellik düzeyi ipucu ver (döngüsellik riski!)

  # Karşılaştırılacak modeller — her satır ayrı bir "simülatif örneklem" üretir.
  modeller = tribble(
    ~saglayici,  ~model,
    "anthropic", "claude-sonnet-5",
    "openai",    "gpt-4.1",
    "google",    "gemini-2.5-flash"
    # "ollama",  "llama3.1"   # yerel/açık ağırlıklı model eklemek için
  )
)

# ---- Klasörler ---------------------------------------------------------
dir.create("data/ham",      recursive = TRUE, showWarnings = FALSE)
dir.create("data/islenmis", recursive = TRUE, showWarnings = FALSE)
dir.create("data/gercek",   recursive = TRUE, showWarnings = FALSE)
dir.create("cikti",         recursive = TRUE, showWarnings = FALSE)

# ---- Sağlayıcıya göre ellmer sohbet nesnesi ----------------------------
olustur_sohbet <- function(saglayici, model, sicaklik = config$sicaklik) {
  ayar <- params(temperature = sicaklik)
  switch(saglayici,
    anthropic = chat_anthropic(model = model, params = ayar),
    openai    = chat_openai(model = model, params = ayar),
    google    = chat_google_gemini(model = model, params = ayar),
    ollama    = chat_ollama(model = model, params = ayar),
    stop("Bilinmeyen sağlayıcı: ", saglayici)
  )
}
