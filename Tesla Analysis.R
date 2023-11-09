library(tidyverse)
library(tslatwtr)

#add comment hello

# Identify accounts ranked from 2 to 6 based on followers count
top_accounts <- list_member |>
  arrange(desc(followers_count)) |>
  slice(2:6)

# Define keywords related to affordability
keywords <- c("affordable", "cheap", "price", "cost", "budget", "economy", "value", "low-cost", "inexpensive", "deal", "discount")
# Create a pattern string for detecting keywords
keywords_pattern <- paste(keywords, collapse = "|")

# Filter tweets for affordability-related keywords by selected users and calculate average engagement per tweet
affordability_tweets <- tweet |>
  filter(user_id %in% top_accounts$user_id) |>
  select(user_id, text, like_count, retweet_count, reply_count) |>
  mutate(
    text_lower = tolower(text),
    keyword = str_extract(text_lower, keywords_pattern)
  ) |>
  filter(!is.na(keyword)) |>
  left_join(top_accounts, by = "user_id") |>
  group_by(name, keyword) |>
  summarise(
    avg_likes = mean(like_count, na.rm = TRUE),
    avg_retweets = mean(retweet_count, na.rm = TRUE),
    avg_replies = mean(reply_count, na.rm = TRUE),
    avg_total_engagement = avg_likes + avg_retweets + avg_replies
  ) |>
  ungroup() |>
  arrange(desc(avg_total_engagement))


# Define a color palette for the companies
company_colors <- c(
  "Tesla" = "#ff9999",  # Light red for Tesla
  "MercedesBenz" = "#ccccff",  # Light purple for Mercedes-Benz
  "BMW" = "#99ccff",  # Light blue for BMW
  "Porsche" = "#99ff99",  # Light green for Porsche
  "Audi" = "#ffff99"  # Light yellow for Audi
)

# Plot with faceted bars, sorted keywords, and custom colors for each company
p <- ggplot(affordability_tweets, aes(x = reorder(keyword, avg_total_engagement), y = avg_total_engagement, fill = name)) +
  geom_col(show.legend = FALSE) +  # Hide legend to avoid repetition
  geom_text(aes(label = name), position = position_stack(vjust = 0.5), check_overlap = TRUE, size = 3) +
  facet_wrap(~ company_group, scales = "free_x") + # Facets with their own x-axis scale
  scale_fill_manual(values = company_colors, limits = names(company_colors)) +
  coord_flip() +
  labs(
    title = "Affordability-Related Twitter Engagement",
    subtitle = "Average engagement per keyword across top Twitter accounts, with Tesla shown separately",
    x = "Keyword",
    y = "Average Engagement per Tweet",
    caption = "Data sourced from tslatwtr package"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "lightblue"),
    strip.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p)

# Save the plot
ggsave("keyword_engagement_top_accounts.png", plot = p, width = 13.33, height = 7.5, units = "in", dpi = 300)


# I pledge that I neither gave nor received help while completing this individual final project.

