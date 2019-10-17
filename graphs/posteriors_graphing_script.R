library(jsonlite)
library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# LOAD INFERENCE RESULTS

binary_inference <- fromJSON("../analysis/inference_results/binary.json")
ternary_inference <- fromJSON("../analysis/inference_results/tertiary.json")
quaternary_inference <- fromJSON("../analysis/inference_results/quaternary.json")
quinary_inference <- fromJSON("../analysis/inference_results/quinary.json")

# GRAPH POSTERIORS

graph_posteriors <- function(pos,title,suppress_pt = FALSE) {
  df <- bind_rows(pos) %>% mutate(alpha = alpha/10) %>% gather()
  if(suppress_pt == TRUE) {
    df <- df %>% filter(!(key %in% c("pt_prob")))
  }
  df$key <- as.character(df$key)
  df$key <- recode(df$key, mu_theta1 = "mu[theta[1]]",
                   mu_theta2 = "mu[theta[2]]",
                   mu_theta3 = "mu[theta[3]]",
                   mu_theta4 = "mu[theta[4]]",
                   pt_prob = "PT",
                   sigma = "sigma",
                   alpha = "alpha/10")
  ggplot(df) +
    aes(y=2*(..density..)/sum(..density..), x=value) +
    geom_density() +
    facet_grid(key~.,scales = "free",labeller = label_parsed) +
    theme(axis.ticks.y =element_blank(),
          axis.text.y = element_blank(),
          panel.background = element_rect(fill = 'white', colour = 'gray'),
          legend.position = "none") +
    labs(x = "", y = "") +
    xlim(0,1) +
    ggtitle(title)
}

graph_posteriors(quaternary_inference$posteriors$support,"Quaternary condition")
ggsave("quaternary_posteriors.jpeg",width = 4, height = 3.5, units = "in", dpi=1000)
graph_posteriors(quinary_inference$posteriors$support, "Quinary condition")
ggsave("quinary_posteriors.jpeg",width = 4, height = 3.5, units = "in", dpi=1000)
graph_posteriors(binary_inference$posteriors$support, "Binary condition", suppress_pt = TRUE)
ggsave("binary_posteriors.jpeg",width = 4, height = 2.1, units = "in", dpi=1000)
graph_posteriors(ternary_inference$posteriors$support, "Ternary condition", suppress_pt = TRUE)
ggsave("ternary_posteriors.jpeg",width = 4, height = 2.1, units = "in", dpi=1000)

