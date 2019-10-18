library(jsonlite)
library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# LOAD EXPERIMENTAL RESULTS

binary_inference <- fromJSON("../analysis/inference_results/binary.json")
ternary_inference <- fromJSON("../analysis/inference_results/tertiary.json")
quaternary_inference <- fromJSON("../analysis/inference_results/quaternary.json")
quinary_inference <- fromJSON("../analysis/inference_results/quinary.json")

d <- read_csv("../experiment_results.csv")
d$response_type <- recode(d$response_type, quatenary = "quaternary", tertiary = "ternary")

results <- d %>%
  group_by(card_type, guess_type, response_type) %>%
  summarize(nright = sum(response == "Right"), 
            nkindaright = sum(response == "Kinda Right"),
            nneither = sum(response == "Neither"),
            nkindawrong = sum(response == "Kinda Wrong"),
            nwrong = sum(response == "Wrong"),
            total = n(), 
            observed_right = nright / total, 
            observed_neither = nneither/total, 
            observed_kindaright = nkindaright/total, 
            observed_kindawrong = nkindawrong/total, 
            observed_wrong = nwrong/total
  )

# GET MODES OF POSTERIOR DISTRIBUTIONS

estimate_mode <- function(s) {
  d <- density(s)
  return(d$x[which.max(d$y)])
}

get_modes <- function(pos, condition, suppress_pt = FALSE, write = TRUE) {
  df <- bind_rows(pos$posteriors$support) %>% gather() 
  if(suppress_pt == TRUE) {
    df <- df %>% filter(!(key %in% c("pt_prob")))
  }
  df <- df %>% group_by(key) %>%
    summarise(mode = estimate_mode(value))
  df <- setNames(as.numeric(df$mode), df$key)
  df <- data.frame(as.list(df))
  if(write){
    write_json(df, paste("inference_results/",condition,"_parameter_modes.json",sep=""))
  }
  return(toJSON(df))
}

get_modes(binary_inference,"binary", write = TRUE, suppress_pt = TRUE)
get_modes(ternary_inference,"ternary", write = TRUE, suppress_pt = TRUE)
get_modes(quaternary_inference,"quaternary", write = TRUE)
get_modes(quinary_inference,"quinary", write = TRUE)

# RUN FORWARD SAMPLING SCRIPTS TO GET PREDICTIVES...

# LOAD PREDICTIVES

binary_predictions <- fromJSON("inference_results/binary_predictions.json")
ternary_predictions <- fromJSON("inference_results/ternary_predictions.json")
quaternary_predictions <- fromJSON("inference_results/quaternary_predictions.json")
quinary_predictions <- fromJSON("inference_results/quinary_predictions.json")

# REFORMAT PREDICTIONS AND GET CORRELATION

reformat <- function(d, condition) {
  d$condition <- condition
  d$merged <- merge(results %>% filter(response_type == d$condition), d$predictions, by = c("card_type","guess_type"))
  d$reformat <- d$merged %>%
    gather(observation_type, observation, observed_right:observed_wrong)  %>%
    gather(prediction_type, prediction, predicted_right:predicted_wrong)  %>%
    separate(observation_type, c("remove1","response_type"), sep = "_", remove = TRUE) %>%
    separate(prediction_type, c("remove2","prediction_type"), sep = "_", remove = TRUE) %>%
    select(-(c("remove1","remove2"))) %>%
    filter(response_type == prediction_type) %>%
    unite(card_guess, c("card_type","guess_type"))
  
  fit1 <- lm(observation ~ prediction, data = d$reformat)
  
  d$rsq <- summary(fit1)$r.squared
  d$arsq <- summary(fit1)$adj.r.squared
  
  return(d$arsq)
}

# reformat(binary_predictions, "binary")
# reformat(ternary_predictions, "ternary")
# reformat(quaternary_predictions, "quaternary")
# reformat(quinary_predictions, "quinary")

sink('summary.txt')

print("Binary Parameter Estimates: ")
print(get_modes(binary_inference,"binary", write = FALSE, suppress_pt = TRUE))

print("Binary Predictives Adj. R^2: ")
print(reformat(binary_predictions, "binary"))

print("Ternary Parameter Estimates: ")
print(get_modes(ternary_inference,"ternary", write = FALSE, suppress_pt = TRUE))

print("Ternary Predictives Adj. R^2: ")
print(reformat(ternary_predictions, "ternary"))

print("Quaternary Parameter Estimates: ")
print(get_modes(quaternary_inference,"quaternary", write = FALSE))

print("Quaternary Predictives Adj. R^2: ")
print(reformat(quaternary_predictions, "quaternary"))

print("Quinary Parameter Estimates: ")
print(get_modes(quinary_inference,"quinary", write = FALSE))

print("Quinary Predictives Adj. R^2: ")
print(reformat(quinary_predictions, "quinary"))

sink()


