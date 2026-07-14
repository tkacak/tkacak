# ============================================================
# run_all.R — runs the whole pipeline in order
# After putting your API keys in .Renviron:
#   source("run_all.R")
# ============================================================

source("R/00_setup.R")                  # packages + configuration
source("R/01_scale_definition.R")       # scale and items
source("R/02_persona_generation.R")     # simulated respondents
source("R/03_llm_data_collection.R")    # LLM calls (the costly step!)
source("R/04_data_cleaning.R")          # quality control + reverse scoring
source("R/05_psychometric_analysis.R")  # psychometrics per sample
source("R/06_comparison.R")             # LLM ↔ real comparison
