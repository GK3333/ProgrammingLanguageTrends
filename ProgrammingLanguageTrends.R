#Load package
library(readr)
library(dplyr)

# Loading Dataset.
by_tag_year <- read_csv("/Users/gautham/Desktop/Projects/R/RFPL/by_tag_year.csv")

# Viewing data.
print(by_tag_year)

# Addding fraction column.
by_tag_year_fraction <- by_tag_year %>%
  mutate( fraction = number/year_total)

# Printing the new table.
print(by_tag_year_fraction)

# Filtering for R tags.
r_time <- by_tag_year_fraction %>%
  filter(tag == "r")

# Print the new table
print(r_time)

# Loading ggplot2.
library(ggplot2)

# Creating a line plot of fraction over time.
ggplot(r_time,(aes(year,fraction))) + geom_line()

# A vector of selected tags.
selected_tags <- c("r", "dplyr", "ggplot2")

# Filter for those tags
selected_tags_over_time <- by_tag_year_fraction %>%
  filter(tag %in% selected_tags)

# Plotting tags over time on a line plot using color to represent tag.
ggplot(selected_tags_over_time, aes(x = year,
                                    y = fraction,
                                    color = tag)) +
  geom_line()

# Find total number of questions for each tag.
sorted_tags <- by_tag_year %>%
  group_by(tag) %>%
  summarize(tag_total = sum(number)) %>%
  arrange(desc(tag_total))

# Printing the new table.
print(sorted_tags)

# Getting the six largest tags.
highest_tags <- head(sorted_tags$tag)

# Filtering for the six largest tags.
by_tag_subset <- by_tag_year_fraction %>%
  filter(tag %in% highest_tags)

# Plotting tags over time on a line plot using color to represent tag.
ggplot(by_tag_subset, aes(x = year,
                          y = fraction,
                          color = tag)) +
  geom_line()

# Getting tags of interest.
my_tags <- c("android", "ios", "windows-phone")

# Filtering for those tag.s
by_tag_subset <- by_tag_year_fraction %>%
  filter(tag %in% my_tags)

# Plotting tags over time on a line plot using color to represent tag.
ggplot(by_tag_subset, aes(x = year,
                          y = fraction,
                          color = tag)) +
  geom_line()
