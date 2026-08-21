#libraries
library(tidyverse)
library(vegan)
#Cleaning of survey data
raw_data <- read_csv("2024/data/herbivores/ng18_herbivore_survey_data.csv")
species_names<-read_csv("2024/data/herbivores/ng18_herbivore_species_list.csv")
raw_data <- raw_data |>
  mutate(
    id = factor(id),
    day = str_extract(id, "^\\d"),
    type = case_when(
      id == "1" ~ "Mopane",
      id == "2.1" ~ "Mopane",
      id == "2.2" ~ "Floodplain",
      id == "3.1" ~ "Floodplain",
      id == "3.2" ~ "Mopane",
      id == "4.1" ~ "Floodplain",
      id == "4.2" ~ "Mopane",
      id == "5.1" ~ "Mopane",
      id == "5.2" ~ "Floodplain",
      id == "6.1" ~ "Floodplain",
      id == "6.2" ~ "Mopane",
      id == "7.1" ~ "Mopane",
      id == "7.2" ~ "Floodplain"),
    count = if_else(count == 0, 1, count),
    species = str_to_lower(species),
    year = case_when(
      id == "1" ~ "2023",
      id == "2.1" ~ "2023",
      id == "2.2" ~ "2023",
      id == "3.1" ~ "2023",
      id == "3.2" ~ "2023",
      
      id == "4.1" ~ "2023",
      id == "4.2" ~ "2023",
      id == "5.1" ~ "2024",
      id == "5.2" ~ "2024",
      id == "6.1" ~ "2024",
      id == "6.2" ~ "2024",
      id == "7.1" ~ "2024",
      id == "7.2" ~ "2024")
    
  )%>% 
  left_join(species_names, by = join_by("species"=="entered_value")) %>% 
  mutate(scientific_name = str_replace(scientific_name, "_", " "))
raw_data <- raw_data %>% 
  select(id, species, count, distance, angle, type, day, year, common_name)


#make the species accum. curves

all_habitats<-raw_data %>% 
  filter(year==2024) %>% 
  select(species, day, count) %>% 
  pivot_wider(names_from = species, values_from = count, values_fn = sum, values_fill = 0) %>% #pivot to the vegan data format 
select(-day) %>% #remove day (it just does it using the index)
  specaccum() # you can actually just plot this here using "%>% plot()" but I don't like the base R plots
data <- data.frame(Day=all_habitats$sites, Richness=all_habitats$richness, SD=all_habitats$sd, Habitat = "All") #Create a df. 
#Can finish here if not interested in seperating by habitat

#just floodplain specaccum
floodplain<-raw_data %>% 
  filter(year==2024, type == "Floodplain") %>% 
  select(species, day, count) %>% 
  pivot_wider(names_from = species, values_from = count, values_fn = sum, values_fill = 0) %>% 
  select(-day) %>% 
  specaccum()
data<-bind_rows(data, 
                data.frame(Day = floodplain$sites, 
                           Richness = floodplain$richness, 
                           SD = floodplain$sd, 
                           Habitat = "Floodplain")
)
#just mopane specaccum

mopane<-raw_data %>% 
  filter(year==2024, type == "Mopane") %>% 
  select(species, day, count) %>% 
  pivot_wider(names_from = species, values_from = count, values_fn = sum, values_fill = 0) %>% 
  select(-day) %>% 
  vegan::specaccum() 
data<-bind_rows(data, 
                data.frame(Day = mopane$sites, 
                           Richness = mopane$richness, 
                           SD = mopane$sd, 
                           Habitat = "Mopane")
)

ggplot(data %>% filter(Habitat=="All"))+ #Change this filter if you want to plot them all
  geom_point(aes(x = Day, y = Richness, colour = Habitat))+
  geom_line(aes(x = Day, y = Richness,colour = Habitat))+
  geom_ribbon(aes(x = Day, ymin = Richness-SD, ymax = Richness+SD, fill = Habitat), alpha = 0.4)+ #add SD bar
  scale_fill_paletteer_d("nationalparkcolors::Acadia", direction  = 1)+
  scale_colour_paletteer_d("nationalparkcolors::Acadia", direction  = 1)+
  scale_x_continuous(breaks = c(1, 2, 3))+
  scale_y_continuous(limits = c(0, 16))+
  theme_bw()+
  labs(x = "Survey effort (days)",
       y = "Unique species detected")+
  theme(
    legend.position = "none", #Remove this if you plot them all. 
    legend.text = element_text(colour = "black", size = 16),
    legend.title = element_text(colour = "black", size = 18),
    axis.text = element_text(colour = "black", size = 16),
    axis.title = element_text(colour = "black", size = 18)
    )





