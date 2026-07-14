# ============================================================
# 03_llm_data_collection.R
# Collecting scale responses for each persona × each LLM
#
# - Structured output (chat_structured) is used: the model must
#   return an integer in the 1..likert_max range for every item
#   -> parsing errors are practically zero.
# - Item order is randomized per persona (to break order effects)
#   and responses are mapped back to the original item numbers.
# - Each model's raw responses are written to data/raw/ right
#   away; after an interruption the script resumes where it left off.
# ============================================================

# ---- Response schema: item_1 ... item_k, each an integer ---------------
response_schema <- do.call(type_object, c(
  list(.description = "Responses to the questionnaire"),
  setNames(
    replicate(n_items,
      type_integer(paste0("Response: integer between ", config$likert_min,
                          " and ", config$likert_max)),
      simplify = FALSE),
    paste0("item_", seq_len(n_items))  # in presentation order 1..k
  )
))

# ---- Get responses for a single persona --------------------------------
# limiter$wait() is called right before every request so the trailing
# 60-second call count stays under the provider's rpm cap (see
# make_rate_limiter() in 00_setup.R). As a second line of defense, a 429 /
# quota error still triggers a wait-and-retry instead of skipping the
# persona: the provider's error message usually states exactly how long to
# wait ("Please retry in 49.9s") — that is parsed and used verbatim, since
# it reflects the server's real quota window far better than a guess.
get_responses <- function(chat, p, item_order, limiter,
                          max_retries = 8, default_wait_sec = 60) {
  prompt <- build_persona_prompt(p, item_order)
  attempt <- 0
  repeat {
    attempt <- attempt + 1
    limiter$wait()
    res <- tryCatch(
      chat$clone()$chat_structured(prompt, type = response_schema),
      error = function(e) e
    )
    if (!inherits(res, "error")) {
      # Map responses from presentation order back to original item numbers
      presented <- unlist(res[paste0("item_", seq_len(n_items))])
      responses <- integer(n_items)
      responses[item_order] <- presented
      return(setNames(as.list(responses), item_names))
    }
    # Error branch: retry only rate-limit / quota errors, otherwise give up
    msg <- conditionMessage(res)
    if (grepl("429|quota|rate.?limit|RESOURCE_EXHAUSTED", msg, ignore.case = TRUE) &&
        attempt <= max_retries) {
      suggested <- str_match(msg, "retry in ([0-9.]+)\\s*s")[, 2]
      wait_sec <- if (!is.na(suggested)) ceiling(as.numeric(suggested)) + 2
                  else default_wait_sec
      message("  rate limit hit; waiting ", wait_sec, "s then retrying (",
              attempt, "/", max_retries, ")")
      Sys.sleep(wait_sec)
    } else {
      stop(res)  # non-rate error, or retries exhausted -> bubble up to caller
    }
  }
}

# ---- Main loop: model × persona ----------------------------------------
set.seed(config$seed)
item_orders <- map(seq_len(nrow(personas)),
                   ~ sample(scale_def$items$item_no))

for (j in seq_len(nrow(config$models))) {
  m <- config$models[j, ]
  label <- paste0(m$provider, "_", gsub("[^a-zA-Z0-9._-]", "-", m$model))
  raw_file <- file.path("data/raw", paste0(label, ".rds"))

  # Resume support: load previously collected responses
  results <- if (file.exists(raw_file)) read_rds(raw_file) else list()
  chat <- create_chat(m$provider, m$model)
  limiter <- make_rate_limiter(m$rpm)
  cat("\n==>", label, "| rpm cap:", m$rpm,
      "| collected:", length(results), "/", nrow(personas), "\n")

  for (i in seq_len(nrow(personas))) {
    p <- personas[i, ]
    if (!is.null(results[[p$persona_id]])) next  # already collected

    record <- tryCatch(
      c(list(persona_id = p$persona_id, model = label,
             timestamp = as.character(Sys.time())),
        get_responses(chat, p, item_orders[[i]], limiter)),
      error = function(e) {
        message("ERROR [", p$persona_id, "]: ", conditionMessage(e))
        NULL
      }
    )

    if (!is.null(record)) {
      results[[p$persona_id]] <- record
      write_rds(results, raw_file)  # persist immediately
    }
    if (i %% 25 == 0) cat("  ", i, "personas done\n")
  }

  # Analysis-ready wide table
  bind_rows(results) |>
    write_csv(file.path("data/processed", paste0("llm_", label, ".csv")))
}

# Tip: with ellmer >= 0.2, parallel_chat_structured() sends requests
# in concurrent batches (much faster, but you have to implement the
# resume logic yourself):
#   prompts <- map2(seq_len(nrow(personas)), item_orders,
#                   ~ build_persona_prompt(personas[.x, ], .y))
#   responses <- parallel_chat_structured(chat, prompts, type = response_schema)
