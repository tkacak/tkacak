# ============================================================
# 01_scale_definition.R
# Scale and item pool definition
#
# The Rosenberg Self-Esteem Scale (10 items, 4-point Likert,
# 5 reverse-keyed items) is used as the running example. To
# study your own scale, edit only this file and the Likert
# range in the config.
# ============================================================

scale_def <- list(
  name         = "Rosenberg Self-Esteem Scale",
  instructions = paste0(
    "Please indicate how much you agree with each of the following statements. ",
    "1 = Strongly disagree, 2 = Disagree, 3 = Agree, 4 = Strongly agree."
  ),
  items = tribble(
    ~item_no, ~text,                                                                  ~subscale, ~reverse,
    1,  "On the whole, I am satisfied with myself.",                                  "general", FALSE,
    2,  "At times I think I am no good at all.",                                      "general", TRUE,
    3,  "I feel that I have a number of good qualities.",                             "general", FALSE,
    4,  "I am able to do things as well as most other people.",                       "general", FALSE,
    5,  "I feel I do not have much to be proud of.",                                  "general", TRUE,
    6,  "I certainly feel useless at times.",                                         "general", TRUE,
    7,  "I feel that I am a person of worth, at least on an equal plane with others.","general", FALSE,
    8,  "I wish I could have more respect for myself.",                               "general", TRUE,
    9,  "All in all, I am inclined to feel that I am a failure.",                     "general", TRUE,
    10, "I take a positive attitude toward myself.",                                  "general", FALSE
  )
)

n_items       <- nrow(scale_def$items)
item_names    <- paste0("i", scale_def$items$item_no)
reverse_items <- item_names[scale_def$items$reverse]

# Measurement model for CFA (lavaan) — the theoretical structure is defined here.
# Single-factor example; for multidimensional scales build it from the subscale column.
cfa_model <- paste0("F1 =~ ", paste(item_names, collapse = " + "))
