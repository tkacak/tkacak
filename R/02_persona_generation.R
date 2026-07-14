# ============================================================
# 02_persona_generation.R
# Simulated respondent (persona) generation
#
# Principle: personas receive only demographics + a short life
# context; the level of the target latent trait is NOT stated
# (otherwise we would be dictating the latent structure by hand).
# Between-person variance comes from the diversity of the
# profiles and from temperature = 1.
#
# Note: match the marginal distributions below to the demographic
# distribution of the REAL sample you will compare against.
# ============================================================

generate_personas <- function(n = config$n_personas, seed = config$seed) {
  set.seed(seed)

  life_contexts <- c(
    "works a demanding job and struggles to keep a work-life balance",
    "recently moved from a big city to a small town",
    "has a wide circle of friends and often attends social events",
    "has been looking for a job for a long time",
    "lives with family and provides care support for them",
    "recently started a new relationship",
    "recently went through a difficult breakup",
    "is trying to start their own business",
    "is dealing with health problems",
    "is deeply involved in sports and outdoor activities",
    "is preparing for exams and worries about the future",
    "is active as a volunteer in a community organization",
    "recently retired and is trying to structure the day",
    "finds it hard to get time alone in a crowded household",
    "misses relatives who live abroad"
  )

  tibble(
    persona_id = sprintf("P%04d", seq_len(n)),
    age        = sample(18:65, n, replace = TRUE),
    gender     = sample(c("female", "male"), n, replace = TRUE, prob = c(.55, .45)),
    education  = sample(c("primary school", "high school", "associate degree",
                          "bachelor's degree", "graduate degree"),
                        n, replace = TRUE, prob = c(.10, .30, .15, .35, .10)),
    residence  = sample(c("metropolitan area", "city", "town", "village"),
                        n, replace = TRUE, prob = c(.45, .30, .18, .07)),
    ses        = sample(c("low", "middle", "high"), n, replace = TRUE,
                        prob = c(.25, .60, .15)),
    context    = sample(life_contexts, n, replace = TRUE),
    # Optional sensitivity condition: trait-level hint (circularity risk!)
    trait_seed = if (config$use_trait_seed) {
      sample(c("quite low", "below average", "average",
               "above average", "quite high"), n, replace = TRUE)
    } else NA_character_
  )
}

# Builds the LLM prompt for a single persona.
# item_order: randomized presentation order for this persona (vector of item_no)
build_persona_prompt <- function(p, item_order) {
  items <- scale_def$items$text[match(item_order, scale_def$items$item_no)]

  identity <- paste0(
    "You are role-playing the person described below:\n",
    "- Age: ", p$age, "\n",
    "- Gender: ", p$gender, "\n",
    "- Education: ", p$education, "\n",
    "- Place of residence: ", p$residence, "\n",
    "- Socioeconomic status: ", p$ses, "\n",
    "- Life context: This person ", p$context, ".\n",
    if (!is.na(p$trait_seed)) {
      paste0("- General self-evaluation: ", p$trait_seed, ".\n")
    } else ""
  )

  paste0(
    identity, "\n",
    "This person is filling out a research questionnaire. Answer the items ",
    "AS THIS PERSON, from their point of view, in a consistent and realistic ",
    "way. Do not give ideal or socially desirable answers; give the answers ",
    "this person would actually give. Real people do not endorse the same ",
    "extreme on every item; show natural variability consistent with the ",
    "person's profile.\n\n",
    "Instructions: ", scale_def$instructions, "\n\n",
    "Items:\n",
    paste0(seq_along(items), ". ", items, collapse = "\n")
  )
}

personas <- generate_personas()
saveRDS(personas, "data/processed/personas.rds")
cat("Number of personas generated:", nrow(personas), "\n")
