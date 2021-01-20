# library(qcoder)
library(tidyverse)
library(janitor)
library(bbplot)
library(gt)
library(readxl)
# create_qcoder_project("my_qcoder_project", sample = TRUE)
# qcode()
library(questionr)

## Recoding all_sec$nexus into all_sec$nexus_rec
clean_data <- read_excel("NGO EXCEL.xlsx",
                        col_types = c("text", "text", "text", 
                                      "text", "text", "numeric")) %>% 
  clean_names()

clean_data$date <- fct_recode(clean_data$date,
                                  "2018" = "2018.00",
                                  "2019" = "2019.00",
                                  "2020" = "2020.00"
)
data <-
  clean_data %>% 
  group_by(development, date) %>% 
  tally() %>% 
  arrange(desc(n)) %>%
  pivot_wider(names_from = date, values_from = n)
 


# ggplot(pr_data, aes(development_partner, n, fill = project_status)) +
  geom_col()


# Total value of aid

clean_data %>% 
  select(development, project_budget) %>% 
  group_by(year, development) %>% 
  summarise_all(funs(sum)) %>% 
  arrange(desc(project_budget))

# All sectors
all_sec <-
  clean_data %>% 
  group_by(nexus) %>% 
  tally() 


all_sec$nexus <- fct_recode(all_sec$nexus,
                            "Agric & Climate" = "Agriculture and climate change",
                            "Education" = "Education sector",
                            "Gender" = "Gender issues",
                            "Gov, HR, D & ROL" = "GOVernance, human rights, democracy and rule of law",
                            "Health" = "Health sector",
                            "NRM" = "Natural resources management",
                            "Pvt Sector & Trade" = "Private sector and trade",
                            "Resilience" = "Resilience Humanitarian and Development",
                            "SMEs" = "SMEs and employment creation",
                            "Social Protection" = "Social protection"
)

ggplot(all_sec, aes(
  x = reorder(nexus, n),
  y = n,
  fill = nexus
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
  filter(nexus == "Resilience Humanitarian and Development")

a <- resilence %>% 
  group_by(development, project_status) %>% 
  tally() %>% 
  pivot_wider(names_from = project_status, values_from = n, values_fill = 0)

summ <- clean_data %>% 
  group_by(date, development) %>% 
  summarise(budget = sum(project_budget))
