library(ggplot2)
library(ggimage)
library(hrbrthemes)
library(here)

dt <- data.frame(img = list.files(here::here('Flag_Wave'), full.names = TRUE),
                 country = c('EU', 'Finland', 'Germany',
                             'Ireland', 'Netherlands', 'Spain', 'Sweden', 
                             'Switzerland', 'UK'),
                 rank = c(3, 8, 2, 7, 5, 9, 1, 6, 4),
                 projects = c(79, 11, 154, 32, 50, 4, 195, 44, 69)
)

colors <- c("#ED3B43", "#B48EAD", "#A3BE8C", "#EBCB8B", "#D08770", "#ffff00",
            "#BF616A", "#81A1C1", "#B48EAD")

ggplot(dt, aes(reorder(country, rank), projects)) +
  geom_col(
    aes(fill = as.factor(rank),
        color = as.factor(rank)
    ), 
    show.legend = FALSE) +
  geom_image(y = -10,
             aes(image = img), 
             size = rep(0.085, 9),  # Play with this a bit
             by = "width") +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  scale_y_continuous(labels = scales::comma) +
  expand_limits(y = -15) +
  labs(x = "",  y= "No. of funded projects",
       title = 'Projects funded across all sectors (2018-2020) ',
       caption = "Consultant, 2020
       Source: 'Data from the development funding portal") +
  theme_ipsum()

ggsave("project_count.png", width = 25, height = 20, units = "cm")
# By value of aid 

dt2 <- data.frame(img = list.files(here::here('Flag_Wave'), full.names = TRUE),
                 country = c('EU', 'Finland', 'Germany',
                             'Ireland', 'Netherlands', 'Spain', 'Sweden', 
                             'Switzerland', 'UK'),
                 rank = c(3, 8, 2, 7, 5, 9, 4, 6, 1),
                 value = c(summ$budget)
)

colors <- c("#ED3B43", "#B48EAD", "#A3BE8C", "#EBCB8B", "#D08770", "#ffff00",
            "#BF616A", "#81A1C1", "#B48EAD")

ggplot(dt2, aes(reorder(country, rank), value)) +
  geom_col(
    aes(fill = as.factor(rank),
        color = as.factor(rank)
    ), 
    show.legend = FALSE) +
  geom_image(y = -10000000,
             aes(image = img), 
             size = rep(0.065, 9),  # Play with this a bit
             by = "width") +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  scale_y_continuous(labels = scales::comma) +
  expand_limits(y = -100000) +
  labs(x = "",  y= "US$",
       title = 'Value of projects (all sectors)(2018-2020) ',
       caption = "© Consultant, 2020
       Source: 'Data from the development funding portal") +
  theme_ipsum()
ggsave("project_value.png", width = 25, height = 20, units = "cm")


# Social Protection by Development Partner --------------------------------

sp_v <- clean_data %>% 
  filter(sector == "Social Protection")
## Reordering sp_v$development
sp_v$development <- fct_relevel(
  sp_v$development,
  "UK", "Sweden", "Germany", "Ireland", "Netherlands"
)
ggplot(sp_v, aes(development, project_budget/1000, fill = date)) + 
  geom_col(position = "dodge", width = 0.5) + 
  theme_ipsum()+ 
  labs(x = "", 
       y = "$M(000)", 
       title = "SP funds per development partner (2018-2020)", 
       caption = "© Consultant, 2020
       Source: 'Data from the development funding portal") + 
  theme(legend.position = "bottom")
ggsave("sp_value.png", width = 20, height = 20, units = "cm")


# Resilience: Ongoing and Completed Projects  -----------------------------
rr_v <- clean_data %>% 
  filter(sector == "Resilience Humanitarian and Development")
ggplot(rr_v, aes(development, project_budget/1000, fill = date)) + 
  geom_col(position = "dodge") + 
  theme_ipsum()+ 
  labs(x = "", 
       y = "$M(000)", 
       title = "Resilience funding per development \n partner (2018-2020)", 
       caption = "© Consultant, 2020
       Source: 'Data from the development funding portal") + 
  theme(legend.position = "bottom")
ggsave("rr_value.png", width = 20, height = 20, units = "cm")

# Governance active and completed programs (2018-2020)  -------------------

gvv_v <- clean_data %>% 
  filter(sector == "GOVernance, human rights, democracy and rule of law")
ggplot(gvv_v, aes(development, project_budget/1000, fill = date)) + 
  geom_col(position = "dodge") + 
  theme_ipsum()+ 
  labs(x = "", 
       y = "$M(000)", 
       title = "Governance, human rights, democracy \n and rule of law", 
       caption = "© Consultant, 2020
       Source: 'Data from the development funding portal") + 
  theme(legend.position = "bottom")
ggsave("gvv_value.png", width = 20, height = 20, units = "cm")



# Agriculture and climate change ------------------------------------------


agric <- clean_data %>%
  filter(sector == "Agriculture and climate change")
ggplot(agric, aes(development, project_budget/1000, fill = date)) + 
  geom_col(position = "dodge") + 
  theme_ipsum()+ 
  labs(x = "", 
       y = "$M(000)", 
       title = "Agriculture and climate change",  
       caption = "© Consultant, 2020
       Source: 'Data from the development funding portal") + 
  theme(legend.position = "bottom")
ggsave("AGRIC_value.png", width = 20, height = 20, units = "cm")


# Natural resources management --------------------------------------------

nrm <- clean_data %>%
  filter(sector == "Natural resources management")
ggplot(nrm, aes(development, project_budget/1000, fill = date)) + 
  geom_col(position = "dodge") + 
  theme_ipsum()+ 
  labs(x = "", 
       y = "$M(000)", 
       title = "Natural resources management",  
       caption = "© Consultant, 2020
       Source: 'Data from the development funding portal") + 
  theme(legend.position = "bottom")
ggsave("nrm_value.png", width = 20, height = 20, units = "cm")


# Education sector --------------------------------------------------------

edu <- clean_data %>%
  filter(sector == "Education sector")
ggplot(edu, aes(development, project_budget/1000, fill = date)) + 
  geom_col(position = "dodge") + 
  theme_ipsum()+ 
  labs(x = "", 
       y = "$M(000)", 
       title = "Education sector",  
       caption = "© Consultant, 2020
       Source: 'Data from the development funding portal") + 
  theme(legend.position = "bottom")
ggsave("education_value.png", width = 20, height = 20, units = "cm")


# Gender sector --------------------------------------------------------

gend <- clean_data %>%
  filter(sector == "Gender issues")
ggplot(gend, aes(development, project_budget/1000, fill = date)) + 
  geom_col(position = "dodge") + 
  theme_ipsum()+ 
  labs(x = "", 
       y = "$M(000)", 
       title = "Gender issues",  
       caption = "© Consultant, 2020
       Source: 'Data from the development funding portal") + 
  theme(legend.position = "bottom")
ggsave("gend_value.png", width = 20, height = 20, units = "cm")

# Health sector --------------------------------------------------------

health <- clean_data %>%
  filter(sector == "Health sector")
ggplot(health, aes(development, project_budget/1000, fill = date)) + 
  geom_col(position = "dodge") + 
  theme_ipsum()+ 
  labs(x = "", 
       y = "$M(000)", 
       title = "Health sector",  
       caption = "© Consultant, 2020
       Source: 'Data from the development funding portal") + 
  theme(legend.position = "bottom")
ggsave("health_value.png", width = 20, height = 20, units = "cm")


# Private sector and trade --------------------------------------------------------

pvt <- clean_data %>%
  filter(sector == "Private sector and trade")
ggplot(pvt, aes(development, project_budget/1000, fill = date)) + 
  geom_col(position = "dodge") + 
  theme_minimal()+ 
  labs(x = "", 
       y = "$M(000)", 
       title = "Private sector and trade",  
       caption = "© Consultant, 2020
       Source: 'Data from the development funding portal") + 
  theme(legend.position = "bottom")
ggsave("pvt_value.png", width = 20, height = 20, units = "cm")


# SMEs and employment creation --------------------------------------------------------

sme <- clean_data %>%
  filter(sector == "SMEs and employment creation")
ggplot(sme, aes(development, project_budget/1000, fill = date)) + 
  geom_col(position = "dodge", width = 0.5) + 
  theme_minimal()+ 
  labs(x = "", 
       y = "$M(000)", 
       title = "SMEs and employment creation",  
       caption = "© Consultant, 2020
       Source: 'Data from the development funding portal") + 
  theme(legend.position = "bottom")
ggsave("smes_value.png", width = 20, height = 20, units = "cm")

# All by sector -----------------------------------------------------------

summ1$sector <- fct_recode(summ1$sector,
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
ggplot(summ1, aes(reorder(sector, budget1), budget1, fill = sector))+ 
  geom_col(width = 0.5)  + 
  scale_y_continuous(labels = scales::dollar_format()) + 
  coord_flip() + 
  theme_minimal()+ 
  labs(x = "", 
       y = "", 
       title = "Funding by sector (2018-2020)",  
       caption = "© Consultant, 2020
       Source: 'Data from the development funding portal") + 
  theme(legend.position = "none")
ggsave("bysector_value.png", width = 20, height = 20, units = "cm")
