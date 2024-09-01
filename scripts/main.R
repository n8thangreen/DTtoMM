
library(tibble)
library(dplyr)

decision_tree <- tibble(
  decision = rep(c("Treatment A", "Treatment B"), each = 3),
  outcome = c("Success", "Failure", "Adverse Event", "Success", "Failure", "Adverse Event"),
  probability = c(0.7, 0.2, 0.1, 0.6, 0.3, 0.1),
  cost = c(1000, 2000, 500, 800, 2500, 600),
  effectiveness = c(0.9, 0.5, 0.1, 0.85, 0.4, 0.2)
)

# define S3 class
# constructors

DecisionTree <- function(data) {
  structure(list(data = data), class = c("DecisionTree", "Model"))
}

MarkovModel <- function(x, ...) {
  UseMethod("MarkovModel")
}

# first argument?
MarkovModel.default <- function(model = NA, init_probs, trans_matrix = NA, n_cycles = 10) {
  structure(list(init_probs = init_probs,
                 trans_matrix = trans_matrix,
                 n_cycles = n_cycles),
            class = c("MarkovModel", "Model"))
}

# decorator
MarkovModel.DecisionTree <- function(model, ...) {
  term_probs <- model$term_probs
  init_probs <- map_terminal_to_markov(term_probs, mapping)
  
  nextMethod(generic = MarkovModel, object = model, init_probs, ...)
}

CombinedModel <- function(...) {
  args <- list(...)
  
  if (any(sapply(args, inherits) != "Model")) {
    stop("All arguments must be of class 'Model'")
  }
  
  structure(args, class = "CombinedModel")
}

###

run_model <- function(model, ...) {
  UseMethod("run_model")
}

run_model.DecisionTree <- function(model) {
  model$data %>%
    group_by(decision) %>%
    summarise(
      expected_cost = sum(probability * cost),
      expected_effectiveness = sum(probability * effectiveness)
    )
}

run_model.MarkovModel <- function(model) {
  
}

run_model.CombinedModel <- function(model) {
  
  for (i in 1:length(model)) {
    model_results[[i]] <- run_model(model[[i]])
  }
  
  model_results
}

# helpers

#
get_costs.DecisionTree <- function(model) {
  results$expected_cost
}

#
get_costs.MarkovModel <- function(model) {
  results$cumulative_cost
}

#
get_costs.CombinedModel <- function(model) {
  total_cost <- 0
  for (i in 1:length(model)) {
    total_cost <- total_cost + get_costs(model)
  }
  total_cost
}

# Group terminal node probabilities
# to Markov model starting states
#
map_terminal_to_markov <- function(probs, mapping) {

}

