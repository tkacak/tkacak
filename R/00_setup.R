# ============================================================
# 00_setup.R
# Packages and study configuration
# ============================================================

library(tidyverse)   # data wrangling
library(ellmer)      # LLM calls from R (Anthropic, OpenAI, Gemini, Ollama...)
library(psych)       # item analysis, alpha/omega, EFA, parallel analysis, Tucker's phi
library(lavaan)      # CFA and measurement invariance
library(semTools)    # model comparison helpers (compareFit)
library(mirt)        # item response theory (Graded Response Model), DIF
library(careless)    # careless-responding indices (longstring, IRV)
library(jsonlite)    # storing raw responses

# ---- Configuration -----------------------------------------------------
config <- list(
  seed             = 2026,   # for reproducibility
  n_personas       = 300,    # number of simulated respondents (match your real sample!)
  likert_min       = 1,
  likert_max       = 4,      # the example scale (RSES) uses a 4-point Likert
  temperature      = 1.0,    # 1 is recommended so response variance is not suppressed
  use_trait_seed   = FALSE,  # TRUE: give personas a trait-level hint (circularity risk!)

  # Models to compare — each row produces a separate "simulated sample".
  models = tribble(
    ~provider,   ~model,
    "anthropic", "claude-sonnet-5",
    "openai",    "gpt-4.1",
    "google",    "gemini-2.5-flash"
    # "ollama",  "llama3.1"   # add a local / open-weights model
  )
)

# ---- Folders -----------------------------------------------------------
dir.create("data/raw",       recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("data/real",      recursive = TRUE, showWarnings = FALSE)
dir.create("output",         recursive = TRUE, showWarnings = FALSE)

# ---- ellmer chat object by provider ------------------------------------
create_chat <- function(provider, model, temperature = config$temperature) {
  opts <- params(temperature = temperature)
  switch(provider,
    anthropic = chat_anthropic(model = model, params = opts),
    openai    = chat_openai(model = model, params = opts),
    google    = chat_google_gemini(model = model, params = opts),
    ollama    = chat_ollama(model = model, params = opts),
    stop("Unknown provider: ", provider)
  )
}
