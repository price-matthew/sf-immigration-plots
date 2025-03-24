# Loading data and libraries ----------------------------------------------

library(tidyverse)  # always
library(showtext)   # for custom font
library(patchwork)  # for combining multiple ggplot objects into one

font_add_google("Open Sans", family = "os") # Adding a font I like and making it the default
showtext_auto()
showtext_opts(dpi = 300)

# Unfortunately, the data I used for this analysis is not in the public domain. 
# If you would like to replicate this analysis, you can ask the Electoral Commission for access to the raw data

# Here is a some dummy data for illustrative purposes.

set.seed(123)

df <- data.frame(
  
  vote1Prev = as.numeric(sample(1:8, 1000, replace = TRUE)),
  
  immigJobs = as.numeric(sample(1:5, 1000, replace = TRUE)),
  immigEcon = as.numeric(sample(1:5, 1000, replace = TRUE)),
  culturalDiversityIE = as.numeric(sample(1:5, 1000, replace = TRUE)),
  asylumLocImpact = as.numeric(sample(1:5, 1000, replace = TRUE))
  
)


# Prepping data -----------------------------------------------------------

df_usable <- df |> 
  
  mutate(
    ge20_recode = case_when(
      
      vote1Prev == 1 ~ "FG",
      vote1Prev == 2 ~ "FF",
      vote1Prev == 3 ~ "SF",
      vote1Prev == 5 ~ "GP",
      vote1Prev == 6 ~ "Lab",
      vote1Prev == 7 ~ "SD",
      vote1Prev %in% c(8:50)  ~ "Ind/oth",
      TRUE ~ NA),
    
    immig_jobs_recode = case_when( # "Immigrants take jobs away from Irish citizens"
      immigJobs == 1 ~ 5,
      immigJobs == 2 ~ 4,
      immigJobs == 3 ~ 3,
      immigJobs == 4 ~ 2,
      immigJobs == 5 ~ 1,
      TRUE ~ NA
    ),
    
    immig_econ_recode = case_when( # "Immigrants are generally good for Ireland's economy"
      immigEcon == 7 ~ NA,
      immigEcon == 8 ~ NA,
      immigEcon == 9 ~ NA,
      TRUE ~ immigEcon
    ),
    
    immig_culture_recode = case_when( # "The cultural diversity brought by immigrants enriches Irish society"
      culturalDiversityIE == 8 ~ NA,
      culturalDiversityIE == 9 ~ NA,
      TRUE ~ culturalDiversityIE
    ),
    
    immig_local_recode = case_when( # "My community would suffer if it were to house international protection applicants"
      asylumLocImpact == 1 ~ 5,
      asylumLocImpact == 2 ~ 4,
      asylumLocImpact == 3 ~ 3,
      asylumLocImpact == 4 ~ 2,
      asylumLocImpact == 5 ~ 1,
      TRUE ~ NA
    ),
    
    immig_soc_mean = rowMeans(across(c(immig_culture_recode, immig_local_recode)), na.rm = TRUE),
    immig_econ_mean = rowMeans(across(c(immig_jobs_recode, immig_econ_recode)), na.rm = TRUE)
  )


sf_voters <- df_usable |> 
  filter(ge20_recode == "SF") |> 
  select(immig_econ_mean, immig_soc_mean) |> 
  drop_na()

fg_voters <- df_usable |> 
  filter(ge20_recode == "FG") |> 
  select(immig_econ_mean, immig_soc_mean) |> 
  drop_na()

ff_voters <- df_usable |> 
  filter(ge20_recode == "FF") |> 
  select(immig_econ_mean, immig_soc_mean) |> 
  drop_na()

gp_voters <- df_usable |> 
  filter(ge20_recode == "GP") |> 
  select(immig_econ_mean, immig_soc_mean) |> 
  drop_na()


# Creating the Sinn Féin voters chart -------------------------------------

sf_dens <-   ggplot(sf_voters, aes(immig_econ_mean, immig_soc_mean)) +
  
  geom_hline(yintercept = seq(0, 6, 3), colour = "grey85") +
  
  geom_vline(xintercept = seq(0, 6, 3), colour = "grey85") +
  
  stat_density_2d(geom = "polygon", contour = TRUE,
                  aes(fill = after_stat(level)),
                  bins = 9) +
  
  scale_fill_gradient(high = "#006d00", low = "#FFFFFF00") +
  
  scale_x_continuous(limits = c(0, 6.1), breaks = c(0, 6), 
                     labels = c("Negative", "Positive")) +
  
  scale_y_continuous(limits = c(0, 6), breaks = c(0, 6), 
                     labels = c("Negative", "Positive")) +
  
  labs(
    title = "Immigration attitudes of 2020 General Election voters",
    subtitle = "Sinn Féin",
    x = "Perceived economic effects",
    y = "Perceived social effects",
    caption = "Data: NEDS, An Coimisiún Toghcháin, June 2024"
  ) +
  
  theme_bw() +
  
  theme(panel.grid = element_blank(),
        panel.border = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title.x = element_text(family = "os", colour = "grey30", margin = margin(t = 10, b = 10), size = 11),
        axis.title.y = element_text(family = "os", colour = "grey30", margin = margin(l = 10, r = -10), size = 11),
        legend.position="none",
        axis.text = element_text(size = 11, family = "os"),
        plot.title = element_text(family = "os", colour = "grey30", margin = margin(t=10, b=10), face = "bold"),
        plot.caption = element_text(family = "os", size = 11, hjust = 0, colour = "grey30", face = "italic"),
        plot.subtitle = element_text(family = "os", colour = "grey30", face = "bold.italic")) 

sf_dens 

ggsave("density_sf.png", sf_dens, width = 7, height = 7)


# Creating the four-party comparison chart --------------------------------

create_density_plot <- function(data, title, colour, show_x, show_y) {
  
  ggplot(data, aes(immig_econ_mean, immig_soc_mean)) +
    
    geom_hline(yintercept = seq(0, 6, 3), colour = "grey85") +
    geom_vline(xintercept = seq(0, 6, 3), colour = "grey85") +
    
    stat_density_2d(geom = "polygon", contour = TRUE,
                    aes(fill = after_stat(level)),
                    bins = 9) +
    
    scale_fill_gradient(low = "#FFFFFF00", high = colour) +
    
    
    labs(subtitle = title,
         x = "Perceived economic effects",
         y = "Perceived social effects") +
    
    
    scale_x_continuous(limits = c(0, 6), breaks = c(0, 6), 
                       labels = c("Negative", "Positive")) +
    
    scale_y_continuous(limits = c(0, 6), breaks = c(0, 6), 
                       labels = c("Negative", "Positive")) +
    
    theme_bw() +
    
    theme(
      panel.border = element_blank(), 
      axis.ticks = element_blank(), 
      panel.grid = element_blank(),
      plot.subtitle = element_text(family = "os", colour = "grey30", face = "bold.italic", margin = margin(l = 20)),
      axis.title.x = element_text(family = "os", colour = "grey30", margin = margin(t = 15, b = 5), size = 11),
      axis.title.y = element_text(family = "os", colour = "grey30", margin = margin(l = 5, r = 15), size = 11),
      legend.position = "none",
      axis.text.x = if (show_x == "show") element_text(size = 9, family = "os") else element_blank(),
      axis.text.y = if (show_y == "show") element_text(size = 9, family = "os") else element_blank()
    )
}


panel_sf <- create_density_plot(sf_voters, "Sinn Féin", "#006d00", show_y = "show", show_x = "hide")
panel_fg <- create_density_plot(fg_voters, "Fine Gael", "#6699FF", show_y = "hide", show_x = "hide")
panel_ff <- create_density_plot(ff_voters, "Fianna Fáil", "#66BB66", show_y = "show", show_x = "show")
panel_gp <- create_density_plot(gp_voters, "Green Party", "#99CC33", show_y = "hide", show_x = "show")


combined_plot <- (panel_sf + plot_spacer() + panel_fg + panel_ff + plot_spacer() + panel_gp) +
  
  plot_layout(axis_titles = "collect",
              nrow = 2,
              ncol = 3,
              widths = c(1, 0.025, 1, 1, 0.025, 1)) +
  
  plot_annotation(
    title = "Immigration attitudes of 2020 General Election voters",
    caption = "Data: NEDS, An Coimisiún Toghcháin, June 2024"
  ) &
  
  theme(
    plot.title = element_text(family = "os", colour = "grey30", margin = margin(t = 10, b = 10, l = 10), face = "bold"),
    plot.caption = element_text(family = "os", size = 11, hjust = 0, colour = "grey30", face = "italic")
  ) 

combined_plot

ggsave("density_all.png", combined_plot, width = 7, height = 7)
