## Install packages and set-up ----

library(tidyverse)
library(viridis)
library(showtext)
library(ggpattern)
library(ggplot2)
library(forcats)

# Allow figures to use the font "Montserrat"

showtext_auto()
font_add_google("Montserrat", "Montserrat")





## Building figure 1 ----

# Read in the survey response data

survey_results_mass_email <- read.csv("data/COMPADRE_protocol_paper_survey_mass_email_20220806.csv")
survey_results_individual_email <- read.csv("data/COMPADRE_protocol_paper_survey_individual_email_20220806.csv")


# Perform some data cleaning on survey response data 

survey_results <- rbind(survey_results_mass_email, survey_results_individual_email)


# Firstly, let's change the first column to be a participant ID

survey_results[,1] <- seq(1:nrow(survey_results))


# Create a vector containing the desired new question names

column_key <- c("participant_id",
                "gen_info_years",
                "gen_info_comfort",
                "methods",
                "trait_names",
                "census_duration",
                "projection_interval",
                "vital_rate_formulas",
                "life_cycle_graph",
                "population_vector",
                "MPM",
                "standardized_method")


# Change the column names to those in the column_key

names(survey_results) <- column_key


# Convert all numeric data to factors

survey_results[sapply(survey_results, is.numeric)] <- lapply(survey_results[sapply(survey_results, is.numeric)], as.factor)


# Restructure the data to be in long form where each row is a single response by a participant

survey_results <- survey_results |> 
  pivot_longer(!participant_id)


# Change the column names names to question and reponse

names(survey_results) <- c("participant_id", "statement", "response")

summary_survey_results <- survey_results |> 
  drop_na() |> 
  filter(statement != "gen_info_years") |> 
  filter(statement != "gen_info_comfort") |> 
  group_by(statement, response) |> 
  dplyr::summarize(total_response = n())


# Building figure 1 plot

figure_1_plot <- summary_survey_results |> 
  ggplot(aes(x = response , y = statement)) +
  geom_vline(aes(xintercept =  stage(statement, after_scale = 3)),
             colour = "black",
             size = 1) +
  geom_point(aes(size = total_response,
                 fill = response),
             shape = 21,
             stroke = 1.2) +
  geom_hline(aes(yintercept =  stage(statement, after_scale = 1.5)),
             colour = "dark grey",
             linetype = "dashed",
             linewidth = 1.5) +
  geom_vline(aes(xintercept =  stage(statement, after_scale = 5.5)),
             colour = "dark grey",
             linewidth = 1.5) +
  scale_x_discrete(expand = expansion(mult = c(0.08, 0.4)),
                   limits = c("1","2","3","4","5"),
                   labels = c("Strongly disagree",
                              "Disagree",
                              "Neutral",
                              "Agree",
                              "Strongly agree")) +
  labs(x = "Response",
       y = "Statement",
       size = "Response number") +
  scale_fill_manual(values = c("orange", "orange", "light grey", "light blue", "light blue"),
                    guide = "none") +
  scale_y_discrete(limits = rev(c("trait_names",
                "census_duration",
                "projection_interval",
                "MPM",
                "methods",
                "life_cycle_graph",
                "population_vector",
                "vital_rate_formulas",
                "standardized_method")),
                labels = rev(c("Trait names",
                "Census duration",
                "Projection interval",
                "MPM",
                "Methods",
                "Life cycle graph",
                "Population vector",
                "Vital rate formulas",
                "Standardized method"))) + 
  theme_minimal() +
  theme(text = element_text(size = 16, family = "Montserrat"),
        axis.text.x = element_text(angle=45, hjust = 1),
        plot.margin = margin(2,2,1,1, "cm"),
        legend.position = "top",
        #legend.position=c(0, 1.08), 
        #legend.box = "horizontal",
        legend.spacing.y = unit(0, 'cm'),
        legend.direction = "horizontal",
        panel.border = element_rect(color = "black",
                                    fill = NA,
                                    linewidth = 1)) +
  guides(size = guide_legend(label.position = "bottom"))
figure_1_plot # 650x700 (dimensions the plot was printed with)




## Building figure 2 ----


# Import data from the screen of papers from COMADRE and COMPADRE

paper_screen_data <- read.csv("data/Merged_paper_screen.csv")

# Summarize the data

summarized_data <- paper_screen_data |> 
  group_by(database) |> 
  summarise(census_date = mean(census_date, na.rm = TRUE),
            projection_interval = mean(projection_interval, na.rm = TRUE),
            life_cycle_diagram = mean(life_cycle_diagram, na.rm = TRUE),
            matrix_presence = mean(matrix_presence, na.rm = TRUE),
            vital_rate_calculations = mean(vital_rate_calculations, na.rm = TRUE),
            latitude_longitude = mean(latitude_longitude, na.rm = TRUE),
            location = mean(location, na.rm = TRUE),
            population_vectors = mean(population_vectors, na.rm = TRUE)) |> 
  pivot_longer(cols = c(census_date, 
                        projection_interval,
                        life_cycle_diagram,
                        matrix_presence,
                        vital_rate_calculations,
                        latitude_longitude,
                        location,
                        population_vectors),
               names_to = "attribute") |> 
  rename(proportion = value) |> 
  group_by(database) |> 
  arrange(desc(proportion))
summarized_data


# Building figure 2 plot

screen_bar_graph <- summarized_data |> 
  ggplot(aes(x = fct_inorder(attribute), y = proportion*100, fill = fct_inorder(database))) +
  geom_bar_pattern(aes(pattern = fct_inorder(database)), 
                   position="dodge", 
                   stat="identity", 
                   colour = "black", 
                   fill = "white",
                   size = 1.2, 
                   alpha = 1) +
  geom_bar_pattern(aes(pattern = fct_inorder(database)), 
                   position="dodge", 
                   stat="identity", 
                   colour = "black", 
                   size = 1.2, 
                   alpha = 0.6) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_pattern_discrete(choices = c("none", "circle"), 
                         labels = c("COMPADRE", "COMADRE")) +
  scale_x_discrete(labels = c("Location",
                              "MPM",
                              "Census duration",
                              "Vital rate formulas",
                              "Projection interval",
                              "Latitude-longitude",
                              "Life cycle graph",
                              "Population vector")
  )+
  scale_fill_manual(values = c("#165734", "#0D2E51" ),
                    labels = c("COMPADRE", "COMADRE")) +
  labs(x = "Component of MPM communication",
       y = "Percentage of publications",
       fill = "Database",
       pattern = "Database") +
  theme_minimal() +
  theme(text = element_text(size = 16, family = "Montserrat"),
        axis.text.x = element_text(angle=35, hjust = 1),
        legend.position=c(0.8, 0.875), 
        legend.spacing.y = unit(0.15, 'cm'),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "dark grey",
                                             size = 1),
        legend.title.align=0.5,
        panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 1))
screen_bar_graph #650x650 (dimensions the plot was printed with)



## Building figure 3 ----

# The types that each publication can fall into are:
# "inadequate", "A", "A+VR", "A+VR+Pop", "A+VR+Eco","A+VR+Pop+Eco"
# inadequate means no MPM
# A means MPM and projection interval
# A+VR means A and vital rate calculations
# A+VR+Pop means A+VR and population vector
# A+VR+Eco means A+VR and lat-long and census date
# A+VR+Pop+Eco means A+VR and population vector and lat-long and census date

# Let's start with COMPADRE

COMPADRE_inadequate <- paper_screen_data |> 
  filter(database == "Compadre") |> 
  filter(projection_interval == 0 | matrix_presence == 0) |> 
  nrow()
COMPADRE_inadequate

COMPADRE_A <- paper_screen_data |> 
  filter(database == "Compadre") |> 
  filter(projection_interval == 1 & matrix_presence == 1) |> 
  nrow()
COMPADRE_A

COMPADRE_A_VR <- paper_screen_data |> 
  filter(database == "Compadre") |> 
  filter(projection_interval == 1 & matrix_presence == 1) |> 
  filter(vital_rate_calculations == 1) |> 
  nrow()
COMPADRE_A_VR

COMPADRE_A_VR_Pop <- paper_screen_data |> 
  filter(database == "Compadre") |> 
  filter(projection_interval == 1 & matrix_presence == 1) |> 
  filter(vital_rate_calculations == 1) |> 
  filter(population_vectors == 1) |> 
  nrow()
COMPADRE_A_VR_Pop

COMPADRE_A_VR_Eco <- paper_screen_data |> 
  filter(database == "Compadre") |> 
  filter(projection_interval == 1 & matrix_presence == 1) |> 
  filter(vital_rate_calculations == 1) |> 
  filter(latitude_longitude == 1 & census_date == 1) |> 
  nrow()
COMPADRE_A_VR_Eco

COMPADRE_A_VR_Pop_Eco <- paper_screen_data |> 
  filter(database == "Compadre") |> 
  filter(projection_interval == 1 & matrix_presence == 1) |> 
  filter(vital_rate_calculations == 1) |> 
  filter(population_vectors == 1 & latitude_longitude == 1 & census_date == 1) |> 
  nrow()
COMPADRE_A_VR_Pop_Eco

# Now let's correct for double counting

COMPADRE_A_VR_Eco <- COMPADRE_A_VR_Eco - COMPADRE_A_VR_Pop_Eco
COMPADRE_A_VR_Pop <- COMPADRE_A_VR_Pop - COMPADRE_A_VR_Pop_Eco

COMPADRE_A_VR <- COMPADRE_A_VR - (COMPADRE_A_VR_Eco + COMPADRE_A_VR_Pop)

COMPADRE_A <- COMPADRE_A - (COMPADRE_A_VR + COMPADRE_A_VR_Eco + COMPADRE_A_VR_Pop)


COMPADRE_inadequate + COMPADRE_A + COMPADRE_A_VR + COMPADRE_A_VR_Pop + COMPADRE_A_VR_Eco + COMPADRE_A_VR_Pop_Eco

paper_screen_data |> 
   filter(database == "Compadre") |> 
   nrow()

# Create test data.
data_matrix_type <- data.frame(
  category=c("inadequate", "A", "A+VR", "A+VR+Pop", "A+VR+Eco","A+VR+Pop+Eco"),
  count=c(COMPADRE_inadequate, 
          COMPADRE_A, 
          COMPADRE_A_VR, 
          COMPADRE_A_VR_Pop, 
          COMPADRE_A_VR_Eco, 
          COMPADRE_A_VR_Pop_Eco)
)

# Compute percentages
data_matrix_type$fraction <- data_matrix_type$count / sum(data_matrix_type$count)

# Compute the cumulative percentages (top of each rectangle)
data_matrix_type$ymax <- cumsum(data_matrix_type$fraction)

# Compute the bottom of each rectangle
data_matrix_type$ymin <- c(0, head(data_matrix_type$ymax, n=-1))

# Compute label position
data_matrix_type$labelPosition <- (data_matrix_type$ymax + data_matrix_type$ymin) / 2

# Compute a good label
data_matrix_type$label <- paste0(data_matrix_type$category, ": ", data_matrix_type$count)

# Make the plot

matrix_type_plot <- ggplot(data_matrix_type, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect(colour = "black",
            size = 1.25) +
  scale_fill_viridis_d(alpha = 0.5) +
  coord_polar(theta="y") +
  xlim(c(-6, 4)) +
  theme_void() 
matrix_type_plot # 700 x 700 (dimensions the plot was printed with)

# Now for the COMADRE figure

COMADRE_inadequate <- paper_screen_data |> 
  filter(database == "Comadre") |> 
  filter(projection_interval == 0 | matrix_presence == 0) |> 
  nrow()
COMADRE_inadequate

COMADRE_A <- paper_screen_data |> 
  filter(database == "Comadre") |> 
  filter(projection_interval == 1 & matrix_presence == 1) |> 
  nrow()
COMADRE_A

COMADRE_A_VR <- paper_screen_data |> 
  filter(database == "Comadre") |> 
  filter(projection_interval == 1 & matrix_presence == 1) |> 
  filter(vital_rate_calculations == 1) |> 
  nrow()
COMADRE_A_VR

COMADRE_A_VR_Pop <- paper_screen_data |> 
  filter(database == "Comadre") |> 
  filter(projection_interval == 1 & matrix_presence == 1) |> 
  filter(vital_rate_calculations == 1) |> 
  filter(population_vectors == 1) |> 
  nrow()
COMADRE_A_VR_Pop

COMADRE_A_VR_Eco <- paper_screen_data |> 
  filter(database == "Comadre") |> 
  filter(projection_interval == 1 & matrix_presence == 1) |> 
  filter(vital_rate_calculations == 1) |> 
  filter(latitude_longitude == 1 & census_date == 1) |> 
  nrow()
COMADRE_A_VR_Eco

COMADRE_A_VR_Pop_Eco <- paper_screen_data |> 
  filter(database == "Comadre") |> 
  filter(projection_interval == 1 & matrix_presence == 1) |> 
  filter(vital_rate_calculations == 1) |> 
  filter(population_vectors == 1 & latitude_longitude == 1 & census_date == 1) |> 
  nrow()
COMADRE_A_VR_Pop_Eco

# Now let's correct for double counting

COMADRE_A_VR_Eco <- COMADRE_A_VR_Eco - COMADRE_A_VR_Pop_Eco
COMADRE_A_VR_Pop <- COMADRE_A_VR_Pop - COMADRE_A_VR_Pop_Eco

COMADRE_A_VR <- COMADRE_A_VR - (COMADRE_A_VR_Eco + COMADRE_A_VR_Pop)

COMADRE_A <- COMADRE_A - (COMADRE_A_VR + COMADRE_A_VR_Eco + COMADRE_A_VR_Pop)


COMADRE_inadequate + COMADRE_A + COMADRE_A_VR + COMADRE_A_VR_Pop + COMADRE_A_VR_Eco + COMADRE_A_VR_Pop_Eco

paper_screen_data |> 
  filter(database == "COMADRE") |> 
  nrow()

# Create test data.
data_matrix_type <- data.frame(
  category=c("inadequate", "A", "A+VR", "A+VR+Pop", "A+VR+Eco","A+VR+Pop+Eco"),
  count=c(COMADRE_inadequate, 
          COMADRE_A, 
          COMADRE_A_VR, 
          COMADRE_A_VR_Pop, 
          COMADRE_A_VR_Eco, 
          COMADRE_A_VR_Pop_Eco)
)

# Compute percentages
data_matrix_type$fraction <- data_matrix_type$count / sum(data_matrix_type$count)

# Compute the cumulative percentages (top of each rectangle)
data_matrix_type$ymax <- cumsum(data_matrix_type$fraction)

# Compute the bottom of each rectangle
data_matrix_type$ymin <- c(0, head(data_matrix_type$ymax, n=-1))

# Compute label position
data_matrix_type$labelPosition <- (data_matrix_type$ymax + data_matrix_type$ymin) / 2

# Compute a good label
data_matrix_type$label <- paste0(data_matrix_type$category, ": ", data_matrix_type$count)

# Make the plot

matrix_type_plot <- ggplot(data_matrix_type, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect(colour = "black",
            size = 1.25) +
  scale_fill_viridis_d(alpha = 0.5) +
  coord_polar(theta="y") +
  xlim(c(-6, 4)) +
  theme_void()
matrix_type_plot # 700 x 700 (dimensions the plot was printed with)