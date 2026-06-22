rm(list = ls())

#===============================================================================#
# Friday 19 June 2026
# Summer School
# Health Inequalities: from Sources to Statistical Models.
# Barcelona, 8-19 June 2026
# Final Assessment Project
# Sex Disparities in Mortality and Causes of Death in the Netherlands: 
  # An Exploration of Historical Amsterdam Data, 1856-1875

#  Facilitator:Joana Maria Pujadas | Email: jpujades@ced.uab.es
#  Student: Gilbert Habaasa       | Email: gilbert.habaasa@lshtm.ac.uk
#===============================================================================#

# 0. Packages ---------------------------------------------------------------------------

packages <- c(

  "forcats",         # orders and recodes categorical variables/factors
  "broom",           # turns model results into tidy data frames
  "tidyverse",       # data cleaning, reshaping and ggplot graphs
  "readxl",          # imports Excel files such as ACD_1856_1875.xlsx
  "janitor",         # cleans messy variable names into easier R names 
  "nnet",            # estimates multinomial logit models with multinom()
    "modelsummary",    # creates regression tables, similar to Stata esttab  
   "marginaleffects" # computes predictions, marginal effects and contrasts
)

missing_packages <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(tidyverse)
library(readxl)
library(janitor)
library(nnet)
library(modelsummary)
library(marginaleffects)
library(broom)


# 1. Import and prepare the analysis dataset --------------------------------------------

amsterdam_db <- read_excel("data/ACD_1856_1875.xlsx")

amsterdam_db <- amsterdam_db |>
  clean_names() |>
  rename(
    person_id = tbl_persoon_id,
    age_year = leeftijd_jaar,
    Gender = geslacht,
    rent = huur,
    year = jaar,
    month = maandnr,
    neighbourhood = buurt_thuis
  ) 

amsterdam_db 

## Variables
 # mortality[infant, child,adult,old age]-Dependent Variable
 
 # Gender-Indepedent variable
 # Year of death-Indepedent variable.
 # Season-Indepedent variable
 #Cause of death-Indepedent variable

#1.mortality

amsterdam_db1 <- amsterdam_db |> 
  filter(!is.na(age_year)) |> 
  mutate(mortality=case_when(age_year>=0.0 & age_year<=1     ~"Infant",
                                age_year>1 & age_year<15     ~"Child",
                                age_year>=15 & age_year<=60  ~"Adult",
                                age_year>60                  ~"Old Age",
  ), 
         mortality= factor(mortality,
                              levels=c("Infant", "Child", "Adult", "Old Age")))
  

#2. Gender

  ##filter non-male or non-female
amsterdam_db1 <- amsterdam_db1 |> 
  filter(!Gender=="onbekend") |> 
  #Rename Gender category
  mutate(Gender=str_replace_all(Gender, pattern="vrouw",
                         replacement = "female"),
Gender=str_replace_all(Gender, pattern="man",
                       replacement = "male"),

  # Create female dummy variable
female=case_when(Gender=="female"~1,
                 Gender=="male"  ~0,
))
amsterdam_db1

# 3. Year of death
# year  year of death
amsterdam_db2 <- amsterdam_db1 |> 
  mutate(year=as.numeric(year))
str(amsterdam_db1$year) # year is now numeric

# Center year so that 1856 = 0. This makes the intercept less awkward.
# year_c = year - 1856,

#4.Season: Season is useful because nineteenth-century mortality often had seasonal patterns.

amsterdam_db2 <- amsterdam_db2 |> 
mutate (month=as.numeric(month),
        season = case_when(
          month %in% c(12, 1, 2) ~ "Winter", 
          month %in% c(3, 4, 5)  ~ "Spring", 
          month %in% c(6, 7, 8)  ~ "Summer", 
                          TRUE   ~ "Autumn"), 
season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn")))

amsterdam_db2

#5. Cause of death
amsterdam_db2 <- amsterdam_db2 |> 
  mutate(
    cause_of_death = case_when(
      icd10h1 == "A00.900"          ~ "Cholera", 
      icd10h1 == "J18.900"          ~ "Pneumonia",  
      icd10h1 == "A16.200"          ~ "Tuberculosis", 
      icd10h1 == "B05.900"          ~ "Measles", 
      TRUE                          ~ "Others"
      ), 
    cause_of_death = factor(
      cause_of_death, 
      levels = c("Cholera","Pneumonia","Tuberculosis","Measles","Others")
    )
  ) |> 
  select(year,
         age_year,
         month,
         Gender,
         female,
         season,
         mortality,
         icd10h1,
         cause_of_death) 

Cause_of_death_summary <- amsterdam_db2 |> 
group_by(Gender, cause_of_death) |> 
  summarise(deaths=n(), .groups = "drop")
Cause_of_death_summary


write_csv(amsterdam_db2,"output/amsterdam_db_pr.csv")


 ## Descriptive Statistics
# Death counts summary by year and age brackets
   ##make a crosstabulation grid table for death counts
heatmap_info <- amsterdam_db2 |> 
    group_by(year,Gender) |> 
  summarise(death_counts=n(), .groups = "drop")
head(heatmap_info)

# Draw a heatmap
ggplot(heatmap_info, aes(x=year, y=Gender, fill=death_counts))+
  geom_tile()+
  scale_fill_gradient(low = "blue", high = "yellow")+
  scale_x_continuous(breaks = seq(from=1856, to=1876,by=4))+
  labs(
    title = "Death records in Amsterdam (1856-1875)", 
    x="Year",
    y="Gender"
  )

ggsave("output/heatmap.png")

# Death counts summary by cause of death and age brackets
##make a crosstabulation grid table for death counts
heatmap_info <- amsterdam_db2 |> 
  group_by(year,cause_of_death) |> 
  summarise(death_counts=n(), .groups = "drop")
head(heatmap_info)

# Draw a heatmap
ggplot(heatmap_info, aes(x=year, y=cause_of_death, fill=death_counts))+
  geom_tile()+
  scale_fill_gradient(low = "green", high = "red")+
  scale_x_continuous(breaks = seq(from=1856, to=1876,by=4))+
  labs(
    title = "Cause of Death in Amsterdam", 
    x="Year",
    y="Distribution of Cause of Death"
  )

ggsave("output/heatmap2_cause_of_death.png")



# 5. Multinomial logit ------------------------------------------------------------------

# "Adult" is selected as the reference category
amsterdam_db2 <- amsterdam_db2 |>
  mutate(mortality = fct_relevel(mortality, "Adult"))

# Estimate models
multi_1 <- multinom(mortality ~ female, data = amsterdam_db2, trace = FALSE)
multi_2 <- multinom(mortality ~ female + season, data = amsterdam_db2, trace = FALSE)
multi_3 <- multinom(mortality ~ female + season + cause_of_death, data = amsterdam_db2, trace = FALSE)
multi_4 <- multinom(mortality ~ female + season * female + cause_of_death, data = amsterdam_db2, trace = FALSE)

# Export to Microsoft Word

modelsummary(
  list("M1" = multi_1,
       "M2" = multi_2,
       "M3" = multi_3,
       "M4" = multi_4),
  
  title = "Multinomial Logistic Regression: Gender Disparities in Mortality",
  
  output = "output/Multinomial Logistic regression.docx",
  
  stars = TRUE,
  gof_map = c("nobs", "aic", "bic"),
  shape = term + response + statistic ~ model
)

# Generate results for the first model only
# Estimate model
multi_1 <- multinom(mortality ~ female, data = amsterdam_db2, trace = FALSE)

# Export to Microsoft Word
modelsummary(
  list("M1" = multi_1),
  
  title = "Multinomial Logistic Regression: Gender Disparities in Mortality",
  
  output = "output/Multinomial Logistic regression mortalityby gender.docx",
  
  stars = TRUE,
  gof_map = c("nobs", "aic", "bic"),
  
  shape = term + response + statistic ~ model
)

# Graphical presentation of results
# Extract tidy results
tidy_m1 <- tidy(multi_1, conf.int = TRUE)

# Keep only female effects
tidy_m1 <- tidy_m1 %>%
  filter(term == "female") %>%
  mutate(
    estimate = exp(estimate),
    conf.low = exp(conf.low),
    conf.high = exp(conf.high)
  )

# Add reference category manually
ref_row <- data.frame(
  term = "female",
  y.level = "Adult",   # reference outcome
  estimate = 1,
  conf.low = 1,
  conf.high = 1
)

# Combine with original results
plot_data <- bind_rows(tidy_m1, ref_row)

# Order factor so Adult appears clearly
plot_data$y.level <- factor(
  plot_data$y.level,
  levels = c("Infant", "Child", "Adult", "Old Age")
)

# Plot
ggplot(plot_data, aes(x = estimate, y = y.level)) +
  
  geom_point(size = 3) +
  
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2) +
  
  # Reference line for MALE
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  
  # Highlight Adult reference category
  geom_point(
    data = subset(plot_data, y.level == "Adult"),
    color = "blue",
    size = 4
  ) +
  
  annotate("text", x = 1.2, y = 3,
           label = "Male reference (RRR = 1)",
           color = "red", hjust = 0) +
  
  annotate("text", x = 1.2, y = 1,
           label = "Outcome reference: Adult",
           color = "blue", hjust = 0) +
  
  labs(
    x = "Relative Risk Ratio (Female vs Male)",
    y = "Mortality Category",
    title = "Gender differences on Mortality Composition",
    caption = "95% Confidence Interval"
  ) +
  
  theme_minimal()
ggsave("output/multinomial logistic regression output.png")




rm(list = ls())

# Objective 2: Decomposition analysis  ------------------------------------

# Load packages
pacman::p_load(tidyverse, readxl, patchwork, ggplot2)

# -------------------------------
# 1. LOAD AND PREPARE DEATH DATA
# -------------------------------

amsterdam_db <- read_excel("data/ACD_1856_1875.xlsx")

amsterdam_db_age_group <- amsterdam_db |>
  rename(
    person_id = tblPersoonID,
    age_year  = LeeftijdJaar,
    Gender    = Geslacht,
    rent      = huur,
    year      = jaar,
    month     = maandnr
  ) |>
  mutate(
    # 5-year age groups
    age_5yr = cut(
      age_year,
      breaks = seq(0, 100, 5),
      right = FALSE,
      labels = paste(seq(0, 95, 5), seq(4, 99, 5), sep = "-")
    ),
    
    # Period definition
    period = case_when(
      year >= 1856 & year <= 1865 ~ "1856-1865",
      year >= 1866 & year <= 1875 ~ "1866-1875",
      TRUE ~ NA_character_
    )
  )

# Summarise deaths
deaths_summary <- amsterdam_db_age_group |>
  filter(Gender != "onbekend", !is.na(period)) |>
  mutate(
    Gender = case_when(
      Gender == "man"   ~ "male",
      Gender == "vrouw" ~ "female"
    )
  ) |>
  group_by(age_5yr, period, Gender) |>
  summarise(deaths = n(), .groups = "drop")


# -------------------------------
# 2. LOAD AND PREPARE POPULATION
# -------------------------------

pop_raw <- read_excel(
  "data/Amsterdam_population.xlsx",
  skip = 1
)

pop <- bind_rows(
  
  # 1859 → 1856-1865
  pop_raw |>
    select(
      age = `age group...1`,
      male = `men...2`,
      female = `women...3`
    ) |>
    mutate(period = "1856-1865"),
  
  # 1869 → 1866-1875
  pop_raw |>
    select(
      age = `age group...5`,
      male = `men...6`,
      female = `women...7`
    ) |>
    mutate(period = "1866-1875")
  
) |>
  filter(
    !is.na(age),
    !age %in% c("Total", "total")
  ) |>
  mutate(
    age = gsub(" to ", "-", age)
  ) |>
  pivot_longer(
    cols = c(male, female),
    names_to = "sex",
    values_to = "pop"
  ) |>
  pivot_wider(
    id_cols = age,
    names_from = c(period, sex),
    values_from = pop,
    names_glue = "pop_{period}_{sex}"
  )


# -------------------------------
# 3. MERGE DEATHS + POPULATION
# -------------------------------

deaths_wide <- deaths_summary |>
  rename(age = age_5yr) |>
  pivot_wider(
    id_cols = age,
    names_from = c(period, Gender),
    values_from = deaths,
    names_glue = "d_{period}_{Gender}"
  )

dt_kit <- deaths_wide |>
  inner_join(pop, by = "age") |>
  mutate(
    # VERY IMPORTANT FIX
    across(starts_with("d_"), ~replace_na(., 0))
  )


# -------------------------------
# 4. KITAGAWA (SEX GAP) – 1856-1865
# -------------------------------

kit_1865 <- dt_kit |>
  mutate(
    mx_f = `d_1856-1865_female` / `pop_1856-1865_female`,
    mx_m = `d_1856-1865_male`   / `pop_1856-1865_male`,
    
    cx_f = `pop_1856-1865_female` / sum(`pop_1856-1865_female`),
    cx_m = `pop_1856-1865_male`   / sum(`pop_1856-1865_male`),
    
    diff_mx = mx_m - mx_f,
    diff_cx = cx_m - cx_f,
    
    avg_mx = (mx_m + mx_f) / 2,
    avg_cx = (cx_m + cx_f) / 2,
    
    rate_component = diff_mx * avg_cx,
    structure_component = diff_cx * avg_mx
  )


results_1865 <- tibble(
  cdr_m = sum(dt_kit$`d_1856-1865_male`) /
    sum(dt_kit$`pop_1856-1865_male`),
  
  cdr_f = sum(dt_kit$`d_1856-1865_female`) /
    sum(dt_kit$`pop_1856-1865_female`),
  
  sex_gap = cdr_m - cdr_f,
  
  rate_effect = sum(kit_1865$rate_component, na.rm = TRUE),
  structure_effect = sum(kit_1865$structure_component, na.rm = TRUE),
  
  check = rate_effect + structure_effect
)


# -------------------------------
# 5. KITAGAWA – 1866-1875
# -------------------------------

kit_1875 <- dt_kit |>
  mutate(
    mx_f = `d_1866-1875_female` / `pop_1866-1875_female`,
    mx_m = `d_1866-1875_male`   / `pop_1866-1875_male`,
    
    cx_f = `pop_1866-1875_female` / sum(`pop_1866-1875_female`),
    cx_m = `pop_1866-1875_male`   / sum(`pop_1866-1875_male`),
    
    diff_mx = mx_m - mx_f,
    diff_cx = cx_m - cx_f,
    
    avg_mx = (mx_m + mx_f) / 2,
    avg_cx = (cx_m + cx_f) / 2,
    
    rate_component = diff_mx * avg_cx,
    structure_component = diff_cx * avg_mx
  )

results_1875 <- tibble(
  cdr_m = sum(dt_kit$`d_1866-1875_male`) /
    sum(dt_kit$`pop_1866-1875_male`),
  
  cdr_f = sum(dt_kit$`d_1866-1875_female`) /
    sum(dt_kit$`pop_1866-1875_female`),
  
  sex_gap = cdr_m - cdr_f,
  
  rate_effect = sum(kit_1875$rate_component, na.rm = TRUE),
  structure_effect = sum(kit_1875$structure_component, na.rm = TRUE),
  
  check = rate_effect + structure_effect
)

# -------------------------------
# 6. VALIDATION CHECK
# -------------------------------

bind_rows(
  results_1865 |> mutate(period = "1856-1865"),
  results_1875 |> mutate(period = "1866-1875")
)


# -------------------------------
# 7. PLOTTING
# -------------------------------

p1 <- kit_1865 |>
  select(age, rate_component, structure_component) |>
  pivot_longer(-age, names_to = "component", values_to = "effect") |>
  ggplot(aes(age, effect, fill = component)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Sex decomposition (1856–1865)")

p2 <- kit_1875 |>
  select(age, rate_component, structure_component) |>
  pivot_longer(-age, names_to = "component", values_to = "effect") |>
  ggplot(aes(age, effect, fill = component)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Sex decomposition (1866–1875)")

p1 + p2



# Combine both decompositions
kit_all <- bind_rows(
  kit_1865 |> mutate(period = "1856-1865"),
  kit_1875 |> mutate(period = "1866-1875")
)

# Reshape for plotting
kit_plot <- kit_all |>
  select(age, period, rate_component, structure_component) |>
  pivot_longer(
    cols = c(rate_component, structure_component),
    names_to = "component",
    values_to = "effect"
  )

# Plot
ggplot(kit_plot, aes(x = age, y = effect, fill = component)) +
  geom_col(position = "stack", width = 0.8) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "black") +
  
  facet_wrap(~period, ncol = 1) +
  
  scale_fill_manual(
    values = c("rate_component" = "#D55E00",
               "structure_component" = "#0072B2"),
    labels = c("Rate effect", "Structure effect")
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  
  labs(
    title = "Kitagawa Decomposition of CDRs",
    subtitle = "Amsterdam, 1856–1875",
    x = "Age group",
    y = "Contribution to male–female mortality gap",
    caption = "CDRs=Crude Death Rates",
    fill = "Component"
  )

ggsave("output/Kitagawa Decomposition.png")
