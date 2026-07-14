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
  # rpm = the provider's free-tier requests-per-minute limit. A sliding-window
  # rate limiter (see make_rate_limiter() below) paces requests to stay under
  # this, rather than relying on a fixed guessed delay between calls.
  #   google  -> aistudio.google.com/apikey   (Gemini Flash free tier: 20 req/min)
  #   groq    -> console.groq.com             (Llama 3.3 70B free tier: ~30 req/min)
  #   mistral -> console.mistral.ai           (Experiment tier: 2 req/min)
  models = tribble(
    ~provider,    ~model,                     ~rpm,
    "google",     "gemini-3.5-flash",          20,
    "groq",       "llama-3.3-70b-versatile",   30
    # Enable this when you have a Mistral key (console.mistral.ai):
    # "mistral",   "mistral-small-latest",     2,
    # Paid / local alternatives (no meaningful rpm ceiling -> use a high number):
    # "anthropic", "claude-sonnet-5",          60,  # ANTHROPIC_API_KEY (paid)
    # "openai",    "gpt-4.1",                  60,  # OPENAI_API_KEY (paid)
    # "openrouter","<any-free-model>",         20,  # OPENROUTER_API_KEY (50 free req/day)
    # "github",    "<model-id>",               20,  # GITHUB_PAT (free daily quota)
    # "ollama",    "llama3.1",                 999  # local, fully free, no key
  )
)

# ---- Sliding-window rate limiter ----------------------------------------
# Returns a closure whose $wait() call blocks just long enough to keep the
# number of calls within the trailing 60-second window at or below
# floor(rpm * safety_margin). Call $wait() immediately before every request.
make_rate_limiter <- function(rpm, safety_margin = 0.9) {
  cap <- max(1, floor(rpm * safety_margin))
  call_times <- numeric(0)
  list(
    wait = function() {
      now <- Sys.time()
      call_times <<- call_times[as.numeric(now - call_times, units = "secs") < 60]
      if (length(call_times) >= cap) {
        oldest <- call_times[1]
        sleep_for <- 60 - as.numeric(now - oldest, units = "secs")
        if (sleep_for > 0) {
          message("  rate limiter: ", cap, " req/min cap reached, waiting ",
                  round(sleep_for, 1), "s")
          Sys.sleep(sleep_for)
        }
        now <- Sys.time()
        call_times <<- call_times[as.numeric(now - call_times, units = "secs") < 60]
      }
      call_times <<- c(call_times, Sys.time())
      invisible(NULL)
    }
  )
}

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
