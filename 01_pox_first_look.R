# welcome to me trying to write pretty code to anaylse pox results :)

#load in the programs---------------------------------------------
library(tidyr)
library(dplyr)
library(readr)
library(ggthemr)
library(ggplot2)
library(lubridate)

#load in data-----------------------------------------------------
data <- read_csv("pox_sequence_info_dummy_delete.csv") %>%
  rename_all(tolower)


#Next: correct some variables------------------------------------------
data$pox_iur <- as.factor(data$pox_iur)
data$pox_scale <- as.factor(data$pox_scale)
data$species<- as.factor(data$species)
data$pcr_sex <- as.factor(data$pcr_sex)
data$strain <- as.factor(data$strain)
data$capture_date <- as.numeric(data$capture_date)
data$new_body_condition <- as.numeric(data$new_body_condition)

#set graph parameters---------------------------------------------
ggthemr("flat", layout = "clean", text_size = 14)

#custom colors used when more colors are needed than offered with default ggtherm
custom_colors <- c("#ff0000", "#ff8000", "#ffff00", "#00ff00", "#00ffff", 
                   "#0000ff", "#8000ff", "#ff00ff", "#ff0080")

#FUN STUFF-----------------------------------------------------------

#Species stuff-------------------------------------------------------
data %>%
  ggplot(aes(x = strain, fill = species)) + geom_bar() +
  scale_fill_manual(values = custom_colors)
# species v strain breakdown using strain as x axis

data %>%
  ggplot(aes(x = species, fill = strain)) +
  geom_bar() +
  labs(title = "Count of Species by Strain",
       x = "Species",
       y = "Count") +
  theme_minimal()
#species by strain broken down another way, shows each individual species on x axis

#STATS for Specis by stain

species_table <- table(data$species, data$strain) %>%
  as.data.frame.matrix() %>%
  select(-`mixed infection`)
chisq.test(species_table)
#All species, p-value= 0.1351 not sig :(

#now lets look directly at specific species, I've done this screen previously 
#so code presented here will just be the significant ones

#GAMO
gamo_data <- data %>%
  filter(species == "GAMO")
gamo_table <- table(gamo_data$strain, gamo_data$species) %>%
  as.data.frame.matrix() %>%
  filter(rownames(.) != "mixed infection") %>%
  select("GAMO")
chisq.test(gamo_table)
# p-value=0.03258 yay sig :)

#FOR
for_data <- data %>%
  filter(species == "FOR")
for_table <- table(for_data$strain, for_data$species) %>%
  as.data.frame.matrix() %>%
  filter(rownames(.) != "mixed infection") %>%
  select("FOR")
chisq.test(for_table)
# p-value=0.0265 yay sig :)

#Curiosity about species and severity, whats going on with the fors?
data %>%
  filter(pox_scale != "mixed infection") %>%
  ggplot(aes(x = species, fill = pox_scale)) +
  geom_bar() +
  labs(title = "Count of Species by Severity",
       x = "Species",
       y = "Count") +
  theme_minimal()
#nothing that looks crazy significant, no closer to understanding for-ref 2 relationship

#Lesion size stuff---------------------------------------------------

data %>%
  filter(!is.na(scale)) %>%
  ggplot(aes(x = strain, y = lesion_size)) +
  geom_boxplot()
data_filter <- data %>% filter(strain != "mixed infection")


lm(lesion_size ~ strain, data = data_filter) %>% summary
# Oooooo p-value= 0.0154 sig :))) ave ref 1 lesion= 6.7453, ave ref 2= 5.4141

#wondering how our "scale" metric compares to lesion size metirc
data %>%
  filter(!is.na(scale)) %>%
  ggplot(aes(x = pox_scale, y = lesion_size)) +
  geom_boxplot()

lm(lesion_size ~ pox_scale, data = data_filter) %>% summary
#looks like it is! that feels encouraging, lots of sig things to see when run that code

#severity stuff-----------------------------------------------------------
data %>%
  filter(!is.na(pox_scale)) %>%
  ggplot(aes(x = strain, fill = pox_scale)) +
  geom_bar() +
  labs(x = "Pox Strain", y = "Individuals", fill = "Infection scale") +
  scale_fill_discrete(labels = c("Mild","Moderate","Severe","Very severe")) +
  scale_x_discrete(labels = c("Mixed","GAL 1", "GAL 2"))

#sabrinas code i stole for stats (lots of plot code is also from Sabrina)
data_table <- table(data$pox_scale, data$strain) %>%
  as.data.frame.matrix %>%
  select(-"mixed infection")
chisq.test(data_table)  
# keeping the four categories gives us p = 0.04035

# Lets group things into AB vs CD (this is more complicated code)
pox <- data %>%
  mutate(group = case_when(pox_scale == "A" ~ "mild",
                           pox_scale == "B" ~ "mild",
                           pox_scale == "C" ~ "severe",
                           pox_scale == "D" ~ "severe"))
pox_table2 <- table(pox$group, pox$strain) %>%
  as.data.frame.matrix %>%
  select(-"mixed infection")
# run a chisq test for small samples in a 2x2 (aka fisher's exact test)

fisher.test(pox_table2)
# p-value = 0.4303

# Sex effects------------------------------------------------------
finch <- data %>% filter(species != "GAMO") %>% filter(pcr_sex != "FAILED")
finch %>%
  ggplot(aes(x = pcr_sex, fill = strain)) +
  geom_bar() +
  labs(
    title = "Count of Sex by Strain",
    x = "Sex",
    y = "Count"
  ) +
  theme_minimal()
finch_table <- table(finch$pcr_sex, finch$strain) %>%
  as.data.frame.matrix %>%
  select(-'mixed infection')
chisq.test(finch_table)
#p-value 0.1557

gamo <- data %>% filter(species == "GAMO") %>% filter(pcr_sex != "FAILED")
gamo %>%
  ggplot(aes(x = pcr_sex, fill = strain)) +
  geom_bar() +
  labs(
    title = "Count of Sex by Strain",
    x = "Sex",
    y = "Count"
  ) +
  theme_minimal()
gamo_table <- table(gamo$pcr_sex, gamo$strain) %>%
  as.data.frame.matrix %>%
  select(-'mixed infection')
chisq.test(gamo_table)
#p-value 0.6252

fortis <- data %>% filter(species == "FOR") %>% filter(pcr_sex != "FAILED")
fortis %>%
  ggplot(aes(x = pcr_sex, fill = strain)) +
  geom_bar() +
  labs(
    title = "Count of Sex by Strain",
    x = "Sex",
    y = "Count"
  ) +
  theme_minimal()
fortis_table <- table(fortis$pcr_sex, fortis$strain) %>%
  as.data.frame.matrix %>%
  select(-'mixed infection')
chisq.test(fortis_table)
#p-value 0.4647

sca <- data %>% filter(species == "SCA") %>% filter(pcr_sex != "FAILED")
sca %>%
  ggplot(aes(x = pcr_sex, fill = strain)) +
  geom_bar() +
  labs(
    title = "Count of Sex by Strain",
    x = "Sex",
    y = "Count"
  ) +
  theme_minimal()
sca_table <- table(sca$pcr_sex, sca$strain) %>%
  as.data.frame.matrix %>%
  select(-'mixed infection')
chisq.test(sca_table)
#p-value 0.6665

#seasonality--------------------------------------------------------

#lets start basic and just look at the month vs strain
data %>%
  ggplot(aes(x = month, fill = strain)) + geom_bar() 
data %>%
  ggplot(aes(x = year_cir, fill = strain)) + geom_bar() 
# ok fun, lets now do the same thing but looking at the season
data %>%
  ggplot(aes(x = trad_season, fill = strain)) + geom_bar() 
#kinda confusing, will wait to figure this out until there is reason to do so 