# ============================================================
# 00_setup.R
# Packages and study configuration
# ============================================================

library(tidyverse)   # data wrangling
library(ellmer)      # LLM calls from R (Gemini, Groq, Mistral, Ollama, Anthropic...)
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
  # The default rows below all have FREE tiers (no credit card needed):
  #   google  -> aistudio.google.com/apikey   (~1,500 requests/day for Flash)
  #   groq    -> console.groq.com             (~30 req/min, ~1,000 req/day)
  #   mistral -> console.mistral.ai           (2 req/min -> needs pause_sec = 31)
  # pause_sec = seconds to wait between requests (rate-limit buffer).
  models = tribble(
    ~provider,    ~model,                     ~pause_sec,
    "google",     "gemini-2.5-flash",          2,
    "groq",       "llama-3.3-70b-versatile",   2.5,
    "mistral",    "mistral-small-latest",     31
    # Paid / local alternatives:
    # "anthropic", "claude-sonnet-5",          0.3,  # ANTHROPIC_API_KEY (paid)
    # "openai",    "gpt-4.1",                  0.3,  # OPENAI_API_KEY (paid)
    # "openrouter","<any-free-model>",         3,    # OPENROUTER_API_KEY (50 free req/day)
    # "github",    "<model-id>",               3,    # GITHUB_PAT (free daily quota)
    # "ollama",    "llama3.1",                 0     # local, fully free, no key
  )
)

# ---- Folders -----------------------------------------------------------
dir.create("data/raw",       recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("data/real",      recursive = TRUE, showWarnings = FALSE)
dir.create("output",         recursive = TRUE, showWarnings = FALSE)

# ---- ellmer chat object by provider ------------------------------------
# API keys are read from .Renviron:
#   GOOGLE_API_KEY / GEMINI_API_KEY, GROQ_API_KEY, MISTRAL_API_KEY,
#   OPENROUTER_API_KEY, GITHUB_PAT, ANTHROPIC_API_KEY, OPENAI_API_KEY
create_chat <- function(provider, model, temperature = config$temperature) {
  opts <- params(temperature = temperature)
  switch(provider,
    google     = chat_google_gemini(model = model, params = opts),
    groq       = chat_groq(model = model, params = opts),
    mistral    = chat_mistral(model = model, params = opts),
    openrouter = chat_openrouter(model = model, params = opts),
    github     = chat_github(model = model, params = opts),
    anthropic  = chat_anthropic(model = model, params = opts),
    openai     = chat_openai(model = model, params = opts),
    ollama     = chat_ollama(model = model, params = opts),
    stop("Unknown provider: ", provider)
  )
}
