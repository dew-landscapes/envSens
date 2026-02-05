# Select only the numeric columns for distribution plotting
bb <- summary_df %>% select(contains("iqr")) %>% 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

ggplot(bb, aes(x = value, fill = variable)) +
  geom_histogram(binwidth = 0.2) + # Use a density plot or a histogram
  labs(title = "Distribution of Multiple Variables", x = "Value", y = "Frequency") +
  facet_wrap(~ variable, scales = "free") + # Creates a grid of plots, allowing scales to vary for each
  theme_minimal() +
  guides(fill = "none") # Remove the redundant legend

aa <- readxl::read_excel(path = "database/BirdLife data on Australian birds/Habitats.xlsx", 
                         +                          sheet = 1,
                         +                          col_types = "guess") %>%
       janitor::clean_names(case = "upper_camel") %>% 
       dplyr::select(ScientificName, CommonName, HabitatsLevel1, HabitatsLevel2,
                      +                   Suitability) %>% 
       dplyr::filter(Suitability == "Suitable") %>% 
       dplyr::distinct()


dd %>%
  filter(grepl("Marine", HabitatsLevel1)) %>% 
  select(HabitatsLevel1)

aa %>% 
  filter(is.na(HabitatsLevel1)) %>% 
  select(HabitatsLevel2) %>% 
  unique()

habitat_map <- c(
  "Foreslope (Outer Reef Slope)" = "Marine Neritic",
  "Back Slope" = "Marine Neritic",
  "Outer Reef Channel" = "Marine Neritic",
  "Lagoon" = "Marine Neritic",
  "Inter-Reef Soft Substrate" = "Marine Neritic",
  "Inter-Reef Rubble Substrate" = "Marine Neritic"
)

df <- aa %>%
  dplyr::mutate(
    HabitatsLevel1 = dplyr::if_else(
      is.na(HabitatsLevel1) & HabitatsLevel2 %in% names(habitat_map),
      habitat_map[HabitatsLevel2],
      HabitatsLevel1
    )
  )

dd <- df %>% 
  select(HabitatsLevel1, HabitatsLevel2) %>% 
  distinct() %>% 
  dplyr::group_by(HabitatsLevel1) %>%
  dplyr::summarise(
    n = dplyr::n(),
    .groups = "drop"
  )
