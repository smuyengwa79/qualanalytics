library(tidyverse)
library(janitor)
library(bbplot)
library(gt)
library(readxl)
library(questionr)

## Recoding all_sec$sector into all_sec$sector_rec
clean_data <- read_excel("Donors.xlsx",
                        col_types = c("text", "text", "text", 
                                      "text", "text", "numeric",
                                      "text", "text")) %>% 
  clean_names()


## Recoding clean_data$development into clean_data$development_rec
clean_data$development <- fct_recode(clean_data$development,
  "EU" = "EU/EC",
  "UK" = "United Kingdom"
)



data <-
  clean_data %>% 
  group_by(development, date) %>% 
  tally() %>% 
  arrange(desc(n)) %>%
  pivot_wider(names_from = date, values_from = n)
 


# ggplot(pr_data, aes(development_partner, n, fill = project_status)) + geom_col()


# Total value of aid

tv <- clean_data %>% 
  select(date, development, project_budget) %>% 
  group_by(date, development) %>% 
  summarise_all(funs(sum)) %>% 
  arrange(desc(project_budget)) %>% 
  pivot_wider(names_from = date, values_from = project_budget)

# All sectors
all_sec <-
  clean_data %>%
  filter(!is.na(sector)) %>% 
  group_by(sector) %>% 
  tally() 

all_sec$sector <- fct_recode(all_sec$sector,
                           "Agric & Climate" = "Agriculture and Climate Change",
                           "Education" = "Education Sector",
                           "Gender" = "Gender Issues",
                           "Gov, HR, D & ROL" = "Governance, Human Rights, Democracy and Rule of Law",
                           "Health" = "Health Sector",
                           "NRM" = "Natural Resource Management",
                           "Pvt Sector & Trade" = "Private Sector and Trade",
                           "Resilience" = "Resilience, Humanitarian and Development",
                           "SMEs" = "SMEs and Employment Creation",
                           "Social Protection" = "Social Protection"
)

ggplot(all_sec, aes(
  x = reorder(sector, n),
  y = n,
  fill = sector
)) +
  geom_col(width = 0.5) +
  labs(title = "Number of projects funded by sector (2018-2020",
       y = "Number of projects",
       x = "") +
  coord_flip() +
  theme_minimal() +
  labs(
    x = "",
    y = "",
    title = "Projects by sector (2018-2020)",
    caption = "© Consultant, 2020
       Source: 'Data from the development funding portal"
  ) +
  theme(legend.position = "none")

 ggsave("by_projects.png", width = 20, height = 20, units = "cm")

 
 resilence <- clean_data %>% 
  filter(sector == "Resilience Humanitarian and Development")

a <- resilence %>% 
  group_by(development, project_status) %>% 
  tally() %>% 
  pivot_wider(names_from = project_status, values_from = n, values_fill = 0)

summ <- clean_data %>% 
  group_by(development) %>% 
  summarise(budget = sum(project_budget))

summ1 <- clean_data %>% 
  filter(!is.na(sector)) %>% 
  group_by(sector) %>% 
  summarise(budget1 = sum(project_budget))
