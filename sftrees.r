library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

register_google(key = "[insert your own key here]")

# Load the data
sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')

# Select Desired columns: species, site_info, date, dbh, latitude, longitude
# Omit NAs and filter out data errors:
#   There is one point well south of san francisco that is likely an error; lat > 37.6 filters it out
my_trees <- sf_trees %>% 
  dplyr::select(species, dbh, latitude, longitude) %>% 
  na.omit() %>%
  filter(latitude > 37.6)

# Setup bounding box and google basemap
bbox <- make_bbox(lon = my_trees$longitude, lat = my_trees$latitude, f = .1)
tree_map <- get_map(location = bbox, maptype = "terrain", source = "google", zoom = 12, scale = 2)

# Generic Heat Map of the entire tree dataset
all <- ggmap(tree_map) +
  stat_density2d(data = my_trees,
                 mapping = aes(x = longitude, y = latitude, fill = ..level.., alpha = 0.25),
                 alpha = 0.15,
                 size = 0.01,
                 bins = 30,
                 geom = "polygon"
  ) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="none",
        plot.title = element_text(hjust = 0.5)
  ) +
  ggtitle("Heat Map of All Street Side \nTrees in San Francisco")

# The four most popular tree species
my_trees %>% count(species) %>% arrange(desc(n))
# 1	Platanus x hispanica :: Sycamore: London Plane	10497
# 2	Metrosideros excelsa :: New Zealand Xmas Tree	7518
# 3	Lophostemon confertus :: Brisbane Box	6721
# 4	Pittosporum undulatum :: Victorian Box	6343

# Setup up tibble of the four most popular tree species
pop_trees <- my_trees %>% 
  select(species, latitude, longitude) %>%
  filter(species %in% c("Platanus x hispanica :: Sycamore: London Plane",
                        "Metrosideros excelsa :: New Zealand Xmas Tree",
                        "Lophostemon confertus :: Brisbane Box",
                        "Pittosporum undulatum :: Victorian Box")) %>%
  # string replace to get the facet labels to show common name + frequency
  mutate_all(funs(str_replace(., "Platanus x hispanica :: Sycamore: London Plane", "1. London Plane: 10,497"))) %>%
  mutate_all(funs(str_replace(., "Metrosideros excelsa :: New Zealand Xmas Tree", "2. New Zealand Christmas Tree: 7,518"))) %>%
  mutate_all(funs(str_replace(., "Lophostemon confertus :: Brisbane Box", "3. Brisbane Box: 6,721"))) %>%
  mutate_all(funs(str_replace(., "Pittosporum undulatum :: Victorian Box", "4. Victorian Box, 6,343"))) %>%
  transform(latitude = as.double(latitude), longitude = as.double(longitude))
                             


# Facet wrap showing the distributions of the four most popular trees
heat <- ggmap(tree_map) +
  stat_density2d(data = pop_trees,
                 mapping = aes(x = longitude, y = latitude, fill = ..level.., alpha = 0.25),
                 alpha = 0.15,
                 size = 0.01,
                 bins = 30,
                 geom = "polygon"
  ) +
  facet_wrap(~ species, nrow = 2) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="none",
        plot.title = element_text(hjust = 0.5)
        ) +
  ggtitle("Most Popular Species")

grid.arrange(all, heat, nrow = 1)
