library(tidyverse)
library(tidytuesdayR)
library(png)
library(plotly)
library(grid)
library(jpeg)



tuesdata <- tidytuesdayR::tt_load('2024-01-30')

groundhogs <- tuesdata$groundhogs
predictions <- tuesdata$predictions

df <- inner_join(groundhogs,predictions, by = "id")

# so the data is organized by groundhogs
# each groudhog is unique
# the prediction count is number of predictions since tradition started
# not number of predictions from other people
# the second groundhog appeared in 1926... maybe just focus on punx? he has the most data

x <- df %>%
  filter(id == "1") %>%
  filter(!is.na(shadow)) %>%
  select(year,shadow) %>%
  mutate(year = as.Date(paste(year,1,1, sep = "-"))) 

# need to make df, col names: Shadow, Frequency
# row names: T,F with respectful frequencies
# title: from 1887 - 2023 shadow predictions

phil <- x %>% group_by(shadow) %>%
  count() %>%
  mutate(type = "Shadow")


# ok... what if we make a geom_area...
# aggregate predictions of spring or winter?
# x axis would be year, y axis would be count of spring or winter?

test <- x %>%
  mutate(shadow = case_when(
  (shadow == "TRUE") ~ (1),
  (shadow == "FALSE") ~ (0)
    )) %>%
  mutate(agg1 = cumsum(shadow),
         shadow = case_when(
           (shadow == 0) ~ (1),
           (shadow == 1) ~ (0)
         ),
         agg2 = cumsum(shadow)
         ) %>%
  select(-shadow)

download.file("https://www.ydr.com/gcdn/presto/2020/02/02/PPYR/58a277a3-1428-4511-9f6c-270fb071355d-AP20033569172760.jpg?crop=2999,1687,x0,y0&width=2999&height=1687&format=pjpg&auto=webp",
              "ground.jpg")
db = readJPEG("ground.jpg")
db = rasterGrob(db, interpolate=TRUE) 

test %>% ggplot(aes(x = year)) +
  annotation_custom(db, ymax = 108) +
  geom_ribbon(aes(ymin = agg1, ymax = 110),fill = "white") +
  geom_line(aes(y = agg1)) +
  geom_line(aes(y = agg2))
  

annotation_custom(db, xmin=1913, xmax=2009, ymin=0, ymax=100) +
  geom_ribbon(aes(ymin=y1, ymax=100), fill="white") 
  

# seeing shadow means -> WINTER
# not seeing shadow means -> SPRING

# Load image of dollar bill and convert to raster grob


phil %>%
  ggplot(aes(y = n, x = type, fill = shadow,group = shadow)) +
  annotation_custom(db, ymin = 0, ymax = 110) +
  geom_ribbon(aes(ymax = 110,ymin = 0)) 

df %>% ggplot(aes(x = region, y = predictions_count)) +
  geom_area()




df <- groundhogs %>%
  arrange(desc(predictions_count))

top_16 <- df$name[1:16]

groundhogs <- groundhogs %>%
  filter(name %in% top_16) # getting just the top 16 predictions

df <- inner_join(groundhogs,predictions, by = "id")

