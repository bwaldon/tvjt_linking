library(jsonlite)
library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# LOAD EXPERIMENTAL RESULTS

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

# REFORMAT INFERENCE RESULTS

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
  
  return(d)
}

# LOAD INFERENCE RESULTS

binary_inference <- reformat(fromJSON("../analysis/inference_results/binary.json"), "binary")
ternary_inference <- reformat(fromJSON("../analysis/inference_results/tertiary.json"), "ternary")
quaternary_inference <- reformat(fromJSON("../analysis/inference_results/quaternary.json"), "quaternary")
quinary_inference <- reformat(fromJSON("../analysis/inference_results/quinary.json"), "quinary")

sink('summary.txt')

print("Binary MaxAP values: ")
print(binary_inference$maxap)

print("Binary Predictives Adj. R^2: ")
print(binary_inference$arsq)

print("Ternary MaxAP values: ")
print(ternary_inference$maxap)

print("Ternary Predictives Adj. R^2: ")
print(ternary_inference$arsq)

print("Quaternary MaxAP values: ")
print(quaternary_inference$maxap)

print("Quaternary Predictives Adj. R^2: ")
print(quaternary_inference$arsq)

print("Quinary MaxAP values: ")
print(quinary_inference$maxap)

print("Quinary Predictives Adj. R^2: ")
print(quinary_inference$arsq)

sink()

