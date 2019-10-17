library(jsonlite)
library(tidyverse)
library(bootstrap)
library(stats4)
library(DescTools)
library(ggthemes)
library(jpeg)
library(grid)
library(gtable)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# LOAD EXPERIMENTAL RESULTS AND IMAGES

cat <- 
  readJPEG("cat_card.jpg") %>%
  rasterGrob(width=0.9)

cat_dog <- 
  readJPEG("catdog_card.jpg")%>%
  rasterGrob(width=0.9)

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
quinary_inference <- reformat(fromJSON("../analysis/inference_results/quinary.json"), "quaternary")

# BINARY GRAPH

binary_summary<-
  d %>%
  filter(response_type=="binary") %>%
  group_by(card_type, guess_type, response) %>%
  summarize(count=n()) %>%
  group_by(card_type, guess_type) %>%
  mutate(total = sum(count), est=count/total)

binary_summary_X_cat <-
  binary_summary %>%
  filter(card_type=="X", guess_type=="X")
binary_summary_X_cat_confint <-
  binary_summary_X_cat$count %>%
  MultinomCI(conf.level = 0.95, method = "sisonglaz") %>% data.frame()
binary_summary_X_cat <- binary_summary_X_cat %>% full_join(binary_summary_X_cat_confint, by="est")

binary_summary_XY_cat <-
  binary_summary %>%
  filter(card_type=="XY", guess_type=="X")
binary_summary_XY_cat_confint <-
  binary_summary_XY_cat$count %>%
  MultinomCI(conf.level = 0.95, method = "sisonglaz") %>% data.frame()
binary_summary_XY_cat <- binary_summary_XY_cat %>% full_join(binary_summary_XY_cat_confint, by="est")

binary_summary_XY_ele <-
  binary_summary %>%
  filter(card_type=="XY", guess_type=="Z")
binary_summary_XY_ele_confint <-
  binary_summary_XY_ele$count %>%
  MultinomCI(conf.level = 0.95, method = "sisonglaz") %>% data.frame()
binary_summary_XY_ele <- binary_summary_XY_ele %>% full_join(binary_summary_XY_ele_confint, by="est") %>% unique()

binary_summary_X_ele <-
  binary_summary %>%
  filter(card_type=="X", guess_type=="Z")
binary_summary_X_ele_confint <-
  binary_summary_X_ele$count %>%
  MultinomCI(conf.level = 0.95, method = "sisonglaz") %>% data.frame()
binary_summary_X_ele <- binary_summary_X_ele %>% full_join(binary_summary_X_ele_confint, by="est") %>% unique()

binary_summary_XY_and <-
  binary_summary %>%
  filter(card_type=="XY", guess_type=="XandY")
binary_summary_XY_and_confint <-
  binary_summary_XY_and$count %>%
  MultinomCI(conf.level = 0.95, method = "sisonglaz") %>% data.frame()
binary_summary_XY_and <- binary_summary_XY_and %>% full_join(binary_summary_XY_and_confint, by="est") %>% unique()

binary_summary_X_and <-
  binary_summary %>%
  filter(card_type=="X", guess_type=="XandY")
binary_summary_X_and_confint <-
  binary_summary_X_and$count %>%
  MultinomCI(conf.level = 0.95, method = "sisonglaz") %>% data.frame()
binary_summary_X_and <- binary_summary_X_and %>% full_join(binary_summary_X_and_confint, by="est") %>% unique()

binary_summary_XY_or <-
  binary_summary %>%
  filter(card_type=="XY", guess_type=="XorY")
binary_summary_XY_or_confint <-
  binary_summary_XY_or$count %>%
  MultinomCI(conf.level = 0.95, method = "sisonglaz") %>% data.frame()
binary_summary_XY_or <- binary_summary_XY_or %>% full_join(binary_summary_XY_or_confint, by="est") %>% unique()

binary_summary_X_or <-
  binary_summary %>%
  filter(card_type=="X", guess_type=="XorY")
binary_summary_X_or_confint <-
  binary_summary_X_or$count %>%
  MultinomCI(conf.level = 0.95, method = "sisonglaz") %>% data.frame()
binary_summary_X_or <- binary_summary_X_or %>% full_join(binary_summary_X_or_confint, by="est") %>% unique()

binary_summary <- bind_rows(binary_summary_X_cat, binary_summary_XY_cat, binary_summary_XY_ele, binary_summary_X_ele, binary_summary_XY_and, binary_summary_X_and, binary_summary_XY_or, binary_summary_X_or)

binary_summary$response <- factor(binary_summary$response, levels = c("Wrong","Right"))
binary_summary$guess_type <- factor(binary_summary$guess_type, levels = c("Z","X","XandY","XorY"))
binary_summary <- binary_summary %>% rename(proportion = "est")

binary_predictions <- binary_inference$reformat %>%
  select(card_guess,prediction_type,prediction) %>%
  separate(card_guess, c("card_type","guess_type"), sep = "_")

binary_predictions$prediction_type <- recode(binary_predictions$prediction_type, right = "Right", wrong = "Wrong")
colnames(binary_predictions)[colnames(binary_predictions)=="prediction_type"] <- "response"

binary_summary_m= merge(binary_summary, binary_predictions, by = c("card_type","guess_type","response")) %>%
  gather("source", "proportion", c("proportion","prediction")) 

binary_summary_m$lwr.ci[binary_summary_m$source == 'prediction'] <- NA
binary_summary_m$upr.ci[binary_summary_m$source == 'prediction'] <- NA

binary_summary_m$guess_type <- recode(binary_summary_m$guess_type, Z = "elephant", X = "cat", XandY = "cat and dog", XorY ="cat or dog")

binary_summary_m = binary_summary_m %>%
  mutate(condition = case_when(
    card_type == "XY" & guess_type %in% c("cat", "cat or dog") ~ "underinformative",
    card_type == "X" & guess_type %in% c("cat", "cat or dog") ~ "true", 
    card_type == "XY" & guess_type == "cat and dog" ~ "true", 
    TRUE ~ "false"
  ))

binary_plot<-
  binary_summary_m %>%
  ggplot(aes(x=response, y=proportion, fill=condition, alpha=source)) +
  geom_bar(stat = "identity", position="dodge",width=0.6,color="gray60") +
  ggtitle(expression(paste("Binary condition (Adj. ", R^2, "= 0.968)"))) +
  facet_grid(card_type~guess_type) +
  labs(x=NULL, y="Proportion of response")+
  theme_few() +
  theme(text = element_text(size=12)) +
  geom_linerange(aes(ymin=lwr.ci,ymax=upr.ci),position=position_dodge(width = 0.6)) +
  guides(fill=FALSE) +
  scale_alpha_discrete(name = "Source", labels = c("Prediction", "Observation"), range = c(0.3, 1)) +
  theme(legend.position = "null") + theme(axis.text.x = element_text(angle=90, hjust = 1))

binary_plot_g <- ggplot_gtable(ggplot_build(binary_plot))

strips <- grep("strip-r", binary_plot_g$layout$name)

new_grobs <- list(cat, cat_dog)

binary_plot_g <- with(binary_plot_g$layout[strips,],
                      gtable_add_grob(binary_plot_g, new_grobs,
                                      t=t, l=l, b=b, r=r, name="cards"))        
binary_plot_g$widths[[12]] <- unit(2.5,"cm")

grid.draw(binary_plot_g)

# TERNARY GRAPH

ternary_summary<-
  d %>%
  filter(response_type=="ternary") %>%
  group_by(card_type, guess_type, response) %>%
  summarize(count=n()) %>%
  group_by(card_type, guess_type) %>%
  mutate(total = sum(count), est=count/total)

ternary_summary_X_cat <-
  ternary_summary %>%
  filter(card_type=="X", guess_type=="X")
ternary_summary_X_cat_confint <-
  ternary_summary_X_cat$count %>%
  MultinomCI(conf.level = 0.95, method = "sisonglaz") %>% data.frame()
ternary_summary_X_cat <- ternary_summary_X_cat %>% full_join(ternary_summary_X_cat_confint, by="est")

ternary_summary_XY_cat <-
  ternary_summary %>%
  filter(card_type=="XY", guess_type=="X")
ternary_summary_XY_cat_confint <-
  ternary_summary_XY_cat$count %>%
  MultinomCI(conf.level = 0.95, method = "sisonglaz") %>% data.frame()
ternary_summary_XY_cat <- ternary_summary_XY_cat %>% full_join(ternary_summary_XY_cat_confint, by="est")

ternary_summary_XY_ele <-
  ternary_summary %>%
  filter(card_type=="XY", guess_type=="Z")
ternary_summary_XY_ele_confint <-
  ternary_summary_XY_ele$count %>%
  MultinomCI(conf.level = 0.95, method = "sisonglaz") %>% data.frame()
ternary_summary_XY_ele <- ternary_summary_XY_ele %>% full_join(ternary_summary_XY_ele_confint, by="est") %>% unique()

ternary_summary_X_ele <-
  ternary_summary %>%
  filter(card_type=="X", guess_type=="Z")
ternary_summary_X_ele_confint <-
  ternary_summary_X_ele$count %>%
  MultinomCI(conf.level = 0.95, method = "sisonglaz") %>% data.frame()
ternary_summary_X_ele <- ternary_summary_X_ele %>% full_join(ternary_summary_X_ele_confint, by="est") %>% unique()

ternary_summary_XY_and <-
  ternary_summary %>%
  filter(card_type=="XY", guess_type=="XandY")
ternary_summary_XY_and_confint <-
  ternary_summary_XY_and$count %>%
  MultinomCI(conf.level = 0.95, method = "sisonglaz") %>% data.frame()
ternary_summary_XY_and <- ternary_summary_XY_and %>% full_join(ternary_summary_XY_and_confint, by="est") %>% unique()

ternary_summary_X_and <-
  ternary_summary %>%
  filter(card_type=="X", guess_type=="XandY")
ternary_summary_X_and_confint <-
  ternary_summary_X_and$count %>%
  MultinomCI(conf.level = 0.95, method = "sisonglaz") %>% data.frame()
ternary_summary_X_and <- ternary_summary_X_and %>% full_join(ternary_summary_X_and_confint, by="est") %>% unique()

ternary_summary_XY_or <-
  ternary_summary %>%
  filter(card_type=="XY", guess_type=="XorY")
ternary_summary_XY_or_confint <-
  ternary_summary_XY_or$count %>%
  MultinomCI(conf.level = 0.95, method = "sisonglaz") %>% data.frame()
ternary_summary_XY_or <- ternary_summary_XY_or %>% full_join(ternary_summary_XY_or_confint, by="est") %>% unique()

ternary_summary_X_or <-
  ternary_summary %>%
  filter(card_type=="X", guess_type=="XorY")
ternary_summary_X_or_confint <-
  ternary_summary_X_or$count %>%
  MultinomCI(conf.level = 0.95, method = "sisonglaz") %>% data.frame()
ternary_summary_X_or <- ternary_summary_X_or %>% full_join(ternary_summary_X_or_confint, by="est") %>% unique()

ternary_summary <- bind_rows(ternary_summary_X_cat, ternary_summary_XY_cat, ternary_summary_XY_ele, ternary_summary_X_ele, ternary_summary_XY_and, ternary_summary_X_and, ternary_summary_XY_or, ternary_summary_X_or)

ternary_summary$response <- factor(ternary_summary$response, levels = c("Wrong","Neither", "Right"))
ternary_summary$guess_type <- factor(ternary_summary$guess_type, levels = c("Z","X","XandY","XorY"))
ternary_summary <- ternary_summary %>% rename(proportion = "est")

ternary_predictions <- ternary_inference$reformat %>%
  select(card_guess,prediction_type,prediction) %>%
  separate(card_guess, c("card_type","guess_type"), sep = "_")

ternary_predictions$prediction_type <- recode(ternary_predictions$prediction_type, right = "Right", wrong = "Wrong", neither = "Neither")
colnames(ternary_predictions)[colnames(ternary_predictions)=="prediction_type"] <- "response"

ternary_summary_m= merge(ternary_summary, ternary_predictions, by = c("card_type","guess_type","response")) %>%
  gather("source", "proportion", c("proportion","prediction")) 

ternary_summary_m$lwr.ci[ternary_summary_m$source == 'prediction'] <- NA
ternary_summary_m$upr.ci[ternary_summary_m$source == 'prediction'] <- NA

ternary_summary_m$guess_type <- recode(ternary_summary_m$guess_type, Z = "elephant", X = "cat", XandY = "cat and dog", XorY ="cat or dog")

ternary_summary_m = ternary_summary_m %>%
  mutate(condition = case_when(
    card_type == "XY" & guess_type %in% c("cat", "cat or dog") ~ "underinformative",
    card_type == "X" & guess_type %in% c("cat", "cat or dog") ~ "true", 
    card_type == "XY" & guess_type == "cat and dog" ~ "true", 
    TRUE ~ "false"
  ))

ternary_plot<-
  ternary_summary_m %>%
  ggplot(aes(x=response, y=proportion, fill=condition, alpha=source)) +
  ggtitle(expression(paste("Ternary condition (Adj. ", R^2, "= 0.977)"))) +
  geom_bar(stat = "identity", position="dodge",width=0.6,color="gray60") +
  scale_y_continuous(breaks=NULL) +
  facet_grid(card_type~guess_type) +
  labs(x=NULL, y=NULL)+
  theme_few() +
  theme(text = element_text(size=12)) +
  # scale_fill_manual(values = c("blue4"), guide=FALSE) +
  geom_linerange(aes(ymin=lwr.ci,ymax=upr.ci),position=position_dodge(width = 0.6)) +
  guides(fill=FALSE) +
  scale_alpha_discrete(name = "Source", labels = c("Prediction", "Observation"), range = c(0.3, 1)) +
  theme(legend.position = "none") + theme(axis.text.x = element_text(angle=90, hjust = 1),
                                                                                axis.text.y = element_blank())

ternary_plot_g <- ggplot_gtable(ggplot_build(ternary_plot))

strips <- grep("strip-r", ternary_plot_g$layout$name)

new_grobs <- list(cat, cat_dog)

ternary_plot_g <- with(ternary_plot_g$layout[strips,],
                       gtable_add_grob(ternary_plot_g, new_grobs,
                                       t=t, l=l, b=b, r=r, name="cards"))        
ternary_plot_g$widths[[12]] <- unit(2.5,"cm")

grid.draw(ternary_plot_g)

# QUATERNARY

quaternary_summary<-
  d %>%
  filter(response_type=="quaternary") %>%
  group_by(card_type, guess_type, response) %>%
  summarize(count=n()) %>%
  group_by(card_type, guess_type) %>%
  mutate(total = sum(count), est=count/total)

quaternary_summary_X_cat <-
  quaternary_summary %>%
  filter(card_type=="X", guess_type=="X")
quaternary_summary_X_cat_confint <-
  quaternary_summary_X_cat$count %>%
  MultinomCI(conf.level = 0.95, method = "sisonglaz") %>% data.frame()
quaternary_summary_X_cat <- quaternary_summary_X_cat %>% full_join(quaternary_summary_X_cat_confint, by="est")

quaternary_summary_XY_cat <-
  quaternary_summary %>%
  filter(card_type=="XY", guess_type=="X")
quaternary_summary_XY_cat_confint <-
  quaternary_summary_XY_cat$count %>%
  MultinomCI(conf.level = 0.95, method = "sisonglaz") %>% data.frame()
quaternary_summary_XY_cat <- quaternary_summary_XY_cat %>% full_join(quaternary_summary_XY_cat_confint, by="est")

quaternary_summary_XY_ele <-
  quaternary_summary %>%
  filter(card_type=="XY", guess_type=="Z")
quaternary_summary_XY_ele_confint <-
  quaternary_summary_XY_ele$count %>%
  MultinomCI(conf.level = 0.95, method = "sisonglaz") %>% data.frame()
quaternary_summary_XY_ele <- quaternary_summary_XY_ele %>% full_join(quaternary_summary_XY_ele_confint, by="est") %>% unique()

quaternary_summary_X_ele <-
  quaternary_summary %>%
  filter(card_type=="X", guess_type=="Z")
quaternary_summary_X_ele_confint <-
  quaternary_summary_X_ele$count %>%
  MultinomCI(conf.level = 0.95, method = "sisonglaz") %>% data.frame()
quaternary_summary_X_ele <- quaternary_summary_X_ele %>% full_join(quaternary_summary_X_ele_confint, by="est") %>% unique()

quaternary_summary_XY_and <-
  quaternary_summary %>%
  filter(card_type=="XY", guess_type=="XandY")
quaternary_summary_XY_and_confint <-
  quaternary_summary_XY_and$count %>%
  MultinomCI(conf.level = 0.95, method = "sisonglaz") %>% data.frame()
quaternary_summary_XY_and <- quaternary_summary_XY_and %>% full_join(quaternary_summary_XY_and_confint, by="est") %>% unique()

quaternary_summary_X_and <-
  quaternary_summary %>%
  filter(card_type=="X", guess_type=="XandY")
quaternary_summary_X_and_confint <-
  quaternary_summary_X_and$count %>%
  MultinomCI(conf.level = 0.95, method = "sisonglaz") %>% data.frame()
quaternary_summary_X_and <- quaternary_summary_X_and %>% full_join(quaternary_summary_X_and_confint, by="est") %>% unique()

quaternary_summary_XY_or <-
  quaternary_summary %>%
  filter(card_type=="XY", guess_type=="XorY")
quaternary_summary_XY_or_confint <-
  quaternary_summary_XY_or$count %>%
  MultinomCI(conf.level = 0.95, method = "sisonglaz") %>% data.frame()
quaternary_summary_XY_or <- quaternary_summary_XY_or %>% full_join(quaternary_summary_XY_or_confint, by="est") %>% unique()

quaternary_summary_X_or <-
  quaternary_summary %>%
  filter(card_type=="X", guess_type=="XorY")
quaternary_summary_X_or_confint <-
  quaternary_summary_X_or$count %>%
  MultinomCI(conf.level = 0.95, method = "sisonglaz") %>% data.frame()
quaternary_summary_X_or <- quaternary_summary_X_or %>% full_join(quaternary_summary_X_or_confint, by="est") %>% unique()

quaternary_summary <- bind_rows(quaternary_summary_X_cat, quaternary_summary_XY_cat, quaternary_summary_XY_ele, quaternary_summary_X_ele, quaternary_summary_XY_and, quaternary_summary_X_and, quaternary_summary_XY_or, quaternary_summary_X_or)

quaternary_summary$response <- factor(quaternary_summary$response, levels = c("Wrong","Kinda Wrong", "Kinda Right", "Right"))
quaternary_summary$guess_type <- factor(quaternary_summary$guess_type, levels = c("Z","X","XandY","XorY"))
quaternary_summary <- quaternary_summary %>% rename(proportion = "est")

quaternary_predictions <- quaternary_inference$reformat %>%
  select(card_guess,prediction_type,prediction) %>%
  separate(card_guess, c("card_type","guess_type"), sep = "_")

quaternary_predictions$prediction_type <- recode(quaternary_predictions$prediction_type, right = "Right", wrong = "Wrong", kindaright = "Kinda Right", kindawrong = "Kinda Wrong")
colnames(quaternary_predictions)[colnames(quaternary_predictions)=="prediction_type"] <- "response"

quaternary_summary_m= merge(quaternary_summary, quaternary_predictions, by = c("card_type","guess_type","response")) %>%
  gather("source", "proportion", c("proportion","prediction")) 

quaternary_summary_m$lwr.ci[quaternary_summary_m$source == 'prediction'] <- NA
quaternary_summary_m$upr.ci[quaternary_summary_m$source == 'prediction'] <- NA

quaternary_summary_m$guess_type <- recode(quaternary_summary_m$guess_type, Z = "elephant", X = "cat", XandY = "cat and dog", XorY ="cat or dog")

quaternary_summary_m = quaternary_summary_m %>%
  mutate(condition = case_when(
    card_type == "XY" & guess_type %in% c("cat", "cat or dog") ~ "underinformative",
    card_type == "X" & guess_type %in% c("cat", "cat or dog") ~ "true", 
    card_type == "XY" & guess_type == "cat and dog" ~ "true", 
    TRUE ~ "false"
  ))

quaternary_plot<-
  quaternary_summary_m %>%
  ggplot(aes(x=response, y=proportion, fill=condition, alpha=source)) +
  geom_bar(stat = "identity", position="dodge",width=0.6,color="gray60") +
  facet_grid(card_type~guess_type) +
  labs(x=NULL, y="Proportion of response")+
  ggtitle(expression(paste("Quaternary condition (Adj. ", R^2, "= 0.931)"))) +
  theme_few() +
  theme(text = element_text(size=12)) +
  # scale_fill_manual(values = c("blue4"), guide=FALSE) +
  geom_linerange(aes(ymin=lwr.ci,ymax=upr.ci),position=position_dodge(width = 0.6)) +
  guides(fill=FALSE) +
  scale_alpha_discrete(name = "Source", labels = c("Prediction", "Observation"), range = c(0.3, 1)) +
  theme(legend.position = "null") + theme(axis.text.x = element_text(angle=90, hjust = 1),
                                          strip.text.y = element_blank())

quaternary_plot_g <- ggplot_gtable(ggplot_build(quaternary_plot))

strips <- grep("strip-r", quaternary_plot_g$layout$name)

new_grobs <- list(cat, cat_dog)

quaternary_plot_g <- with(quaternary_plot_g$layout[strips,],
                          gtable_add_grob(quaternary_plot_g, new_grobs,
                                          t=t, l=l, b=b, r=r, name="cards"))        
quaternary_plot_g$widths[[12]] <- unit(2.5,"cm")

grid.draw(quaternary_plot_g)

#QUINARY

quinary_summary<-
  d %>%
  filter(response_type=="quinary") %>%
  group_by(card_type, guess_type, response) %>%
  summarize(count=n()) %>%
  group_by(card_type, guess_type) %>%
  mutate(total = sum(count), est=count/total)

quinary_summary_X_cat <-
  quinary_summary %>%
  filter(card_type=="X", guess_type=="X")
quinary_summary_X_cat_confint <-
  quinary_summary_X_cat$count %>%
  MultinomCI(conf.level = 0.95, method = "sisonglaz") %>% data.frame()
quinary_summary_X_cat <- quinary_summary_X_cat %>% full_join(quinary_summary_X_cat_confint, by="est")

quinary_summary_XY_cat <-
  quinary_summary %>%
  filter(card_type=="XY", guess_type=="X")
quinary_summary_XY_cat_confint <-
  quinary_summary_XY_cat$count %>%
  MultinomCI(conf.level = 0.95, method = "sisonglaz") %>% data.frame()
quinary_summary_XY_cat <- quinary_summary_XY_cat %>% full_join(quinary_summary_XY_cat_confint, by="est")

quinary_summary_XY_ele <-
  quinary_summary %>%
  filter(card_type=="XY", guess_type=="Z")
quinary_summary_XY_ele_confint <-
  quinary_summary_XY_ele$count %>%
  MultinomCI(conf.level = 0.95, method = "sisonglaz") %>% data.frame()
quinary_summary_XY_ele <- quinary_summary_XY_ele %>% full_join(quinary_summary_XY_ele_confint, by="est") %>% unique()

quinary_summary_X_ele <-
  quinary_summary %>%
  filter(card_type=="X", guess_type=="Z")
quinary_summary_X_ele_confint <-
  quinary_summary_X_ele$count %>%
  MultinomCI(conf.level = 0.95, method = "sisonglaz") %>% data.frame()
quinary_summary_X_ele <- quinary_summary_X_ele %>% full_join(quinary_summary_X_ele_confint, by="est") %>% unique()

quinary_summary_XY_and <-
  quinary_summary %>%
  filter(card_type=="XY", guess_type=="XandY")
quinary_summary_XY_and_confint <-
  quinary_summary_XY_and$count %>%
  MultinomCI(conf.level = 0.95, method = "sisonglaz") %>% data.frame()
quinary_summary_XY_and <- quinary_summary_XY_and %>% full_join(quinary_summary_XY_and_confint, by="est") %>% unique()

quinary_summary_X_and <-
  quinary_summary %>%
  filter(card_type=="X", guess_type=="XandY")
quinary_summary_X_and_confint <-
  quinary_summary_X_and$count %>%
  MultinomCI(conf.level = 0.95, method = "sisonglaz") %>% data.frame()
quinary_summary_X_and <- quinary_summary_X_and %>% full_join(quinary_summary_X_and_confint, by="est") %>% unique()

quinary_summary_XY_or <-
  quinary_summary %>%
  filter(card_type=="XY", guess_type=="XorY")
quinary_summary_XY_or_confint <-
  quinary_summary_XY_or$count %>%
  MultinomCI(conf.level = 0.95, method = "sisonglaz") %>% data.frame()
quinary_summary_XY_or <- quinary_summary_XY_or %>% full_join(quinary_summary_XY_or_confint, by="est") %>% unique()

quinary_summary_X_or <-
  quinary_summary %>%
  filter(card_type=="X", guess_type=="XorY")
quinary_summary_X_or_confint <-
  quinary_summary_X_or$count %>%
  MultinomCI(conf.level = 0.95, method = "sisonglaz") %>% data.frame()
quinary_summary_X_or <- quinary_summary_X_or %>% full_join(quinary_summary_X_or_confint, by="est") %>% unique()

quinary_summary <- bind_rows(quinary_summary_X_cat, quinary_summary_XY_cat, quinary_summary_XY_ele, quinary_summary_X_ele, quinary_summary_XY_and, quinary_summary_X_and, quinary_summary_XY_or, quinary_summary_X_or)

quinary_summary$response <- factor(quinary_summary$response, levels = c("Wrong","Kinda Wrong", "Neither", "Kinda Right", "Right"))
quinary_summary <- quinary_summary %>% rename(proportion = "est")

quinary_predictions <- quinary_inference$reformat %>%
  select(card_guess,prediction_type,prediction) %>%
  separate(card_guess, c("card_type","guess_type"), sep = "_")

quinary_predictions$prediction_type <- recode(quinary_predictions$prediction_type, right = "Right", wrong = "Wrong", kindaright = "Kinda Right", kindawrong = "Kinda Wrong", neither = "Neither")
colnames(quinary_predictions)[colnames(quinary_predictions)=="prediction_type"] <- "response"

quinary_summary_m= merge(quinary_summary, quinary_predictions, by = c("card_type","guess_type","response")) %>%
  gather("source", "proportion", c("proportion","prediction")) 

quinary_summary_m$lwr.ci[quinary_summary_m$source == 'prediction'] <- NA
quinary_summary_m$upr.ci[quinary_summary_m$source == 'prediction'] <- NA

quinary_summary_m$guess_type <- recode(quinary_summary_m$guess_type, Z = "elephant", X = "cat", XandY = "cat and dog", XorY ="cat or dog")

quinary_summary_m$guess_type <- ordered(quinary_summary_m$guess_type, levels= c("elephant","cat","cat and dog","cat or dog"))

quinary_summary_m = quinary_summary_m %>%
  mutate(condition = case_when(
    card_type == "XY" & guess_type %in% c("cat", "cat or dog") ~ "underinformative",
    card_type == "X" & guess_type %in% c("cat", "cat or dog") ~ "true", 
    card_type == "XY" & guess_type == "cat and dog" ~ "true", 
    TRUE ~ "false"
  ))

quinary_plot<-
  quinary_summary_m %>%
  ggplot(aes(x=response, y=proportion, fill=condition, alpha=source)) +
  geom_bar(stat = "identity", position="dodge",width=0.6,color="gray60") +
  ggtitle(expression(paste("Quinary condition (Adj. ", R^2, "= 0.895)"))) +
  facet_grid(card_type~guess_type) +
  scale_y_continuous(breaks=NULL) +
  labs(x=NULL, y=NULL)+
  theme_few() +
  theme(text = element_text(size=12)) +
  # scale_fill_manual(values = c("blue4"), guide=FALSE) +
  geom_linerange(aes(ymin=lwr.ci,ymax=upr.ci),position=position_dodge(width = 0.6)) +
  guides(fill=FALSE) +
  scale_alpha_discrete(name = "Source", labels = c("Prediction", "Observation"), range = c(0.3, 1)) +
  theme(legend.position = "none") + theme(axis.text.x = element_text(angle=90, hjust = 1),
                                          axis.text.y = element_blank())

quinary_plot_g <- ggplot_gtable(ggplot_build(quinary_plot))

strips <- grep("strip-r", quinary_plot_g$layout$name)

new_grobs <- list(cat, cat_dog)

quinary_plot_g <- with(quinary_plot_g$layout[strips,],
                       gtable_add_grob(quinary_plot_g, new_grobs,
                                       t=t, l=l, b=b, r=r, name="cards"))        
quinary_plot_g$widths[[12]] <- unit(2.5,"cm")

grid.draw(quinary_plot_g)

# SAVE GRAPHS

# 7.26 x 5.24

ggsave("ternary_plot.png", width = 5.85, height = 4.19, units = "in", plot = grid.draw(ternary_plot_g), dpi=1000)
ggsave("binary_plot.png",width = 5.85, height = 4.19, units = "in", plot = grid.draw(binary_plot_g), dpi=1000)
ggsave("quaternary_plot.png", width = 5.85, height = 4.19, units = "in", plot = grid.draw(quaternary_plot_g), dpi=1000)
ggsave("quinary_plot.png",width = 5.85, height = 4.19, units = "in", plot = grid.draw(quinary_plot_g), dpi=1000)

