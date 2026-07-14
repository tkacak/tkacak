# ============================================================
# run_all.R — tüm akışı sırayla çalıştırır
# API anahtarlarını .Renviron'a yazdıktan sonra:
#   source("run_all.R")
# ============================================================

source("R/00_kurulum.R")           # paketler + yapılandırma
source("R/01_olcek_tanimi.R")      # ölçek ve maddeler
source("R/02_persona_uretimi.R")   # simülatif bireyler
source("R/03_llm_veri_toplama.R")  # LLM çağrıları (maliyetli adım!)
source("R/04_veri_temizleme.R")    # kalite kontrol + ters puanlama
source("R/05_psikometrik_analiz.R")# örneklem başına psikometri
source("R/06_karsilastirma.R")     # LLM ↔ gerçek karşılaştırması
