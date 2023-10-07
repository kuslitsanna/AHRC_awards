library(pacman)

p_load("tidyr",
       "ggplot2",
       "dplyr",
       "lubridate",
       "openxlsx")

### Sample topic proportion visualisation ##########

# Generate random sample from topic proportion matrix (theta)

random_row_indices <- sample(nrow(slda1$theta), 5)

topicProportionExamples <- slda1$theta[random_row_indices, ]

# Transform matrix to data frame for visualization

topicProportionExamples <- as.data.frame(topicProportionExamples)

# Add a column for document ids

topicProportionExamples$Document <- rownames(topicProportionExamples)

# Reshape the data using pivot_longer

topicProportionExamples <- pivot_longer(topicProportionExamples, cols = -Document, names_to = "Topic", values_to = "Proportion")

# Create the ggplot visualization

example_plot <- ggplot(data = topicProportionExamples, aes(x = Topic, 
                                           y = Proportion, 
                                           fill = Document)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip() +
  facet_wrap(~ Document, ncol = 5)

example_plot

### Topic proportions by year ####


topic_proportions <- as.data.frame(slda1$theta)

#topic_proportions$ProjectReference <- rownames(topic_proportions)

vars <- docvars %>% select(ProjectReference, StartDate, AwardPounds) %>% mutate(Year=year(StartDate),.keep="unused")



# get mean topic proportions per year
topic_proportion_per_year <- aggregate(topic_proportions, 
                                         by = list(Year = vars$Year), 
                                         mean
                                         )

# create palette for visualisation

custom_palette <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
  "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5",
  "#c49c94", "#f7b6d2", "#c7c7c7", "#dbdb8d", "#9edae5",
  "#393b79", "#e6550d", "#637939", "#d6616b", "#843c39",
  "#d8b365", "#7b4173", "#a55194", "#8ca252", "#b5cf6b",
  "#6b6ecf", "#8c564b"
)

# transform df to longer for visualisation

topic_proportion_per_year <- pivot_longer(topic_proportion_per_year, 
                              cols = -Year, 
                              names_to = "Topic", 
                              values_to = "Proportion")

# Plot topic proportions by year

topic_year_plot <- ggplot(topic_proportion_per_year, aes(x = Year, 
                                      y = Proportion, 
                                      fill = Topic)) + 
  geom_bar(stat = "identity") + ylab("Proportion") + 
  scale_fill_manual(values = custom_palette, name = "Topic") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Topic Proportions by Year")

topic_year_plot


### Topic proportions by year weighted by AwardPounds ####

weighted_topic_proportions <- sweep(topic_proportions, MARGIN = 1, STATS = vars$AwardPounds, FUN = "*")


weighted_topic_proportions_per_year <- aggregate(weighted_topic_proportions, 
                                         by = list(Year = vars$Year), 
                                         mean
)

# Normalize values by dividing by the row sums

weighted_topic_proportions_per_year[, -1] <-
  weighted_topic_proportions_per_year[, -1] / 
  rowSums(weighted_topic_proportions_per_year[, -1])

weighted_topic_proportions_per_year <- 
  pivot_longer(weighted_topic_proportions_per_year, 
                              cols = -Year, 
                              names_to = "Topic", 
                              values_to = "Proportion")

topic_proportion_per_year$WeightedProportion <- weighted_topic_proportions_per_year$Proportion

# Plot weighted topic proportions with custom colors

weighted_topic_year_plot <-
  ggplot(topic_proportion_per_year, aes(x = Year, 
                                                y = WeightedProportion, 
                                                fill = Topic)) + 
  geom_bar(stat = "identity") + ylab("Proportion") + 
  scale_fill_manual(values = custom_palette, name = "Topic") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Topic Proportions by Year Weighted by the Value of the Award")

weighted_topic_year_plot

rm("example_plot",
   "slda2",
   "slda2_topics",
   "topicProportionExamples",
   "vars",
   "json2",
   "random_row_indices",
   "weighted_topic_proportions_per_year")

### Descriptive stats of docvars #####

docvars$NumMonths <- as.period(interval(docvars$StartDate, 
                                        docvars$EndDate + 1)) %/% months(1)

boxplot(docvars$NumMonths)

summary(docvars$NumMonths)

docvars$StartYear <- year(docvars$StartDate)

CPI <- read.csv("ahrc_applications/CPI_index.csv")

docvars <- inner_join(docvars, CPI, by=c("StartYear" = "Year"))

docvars <- docvars%>% mutate(AwardPoundsAdjusted=AwardPounds*CPI.INDEX/131.3) %>%
  select(-CPI.INDEX)

range(docvars$AwardPounds)

# Calculate quantiles to divide the data into equal parts
quantiles <- quantile(docvars$AwardPounds, probs = seq(0, 1, length.out = 8 + 1))

# Use cut() to create quantile-based bands
bands <- cut(docvars$AwardPounds, breaks = quantiles, include.lowest = TRUE)

# Round the values to the nearest 50000
rounded_values <- round(docvars$AwardPoundsAdjusted / 50000) * 50000

# Create a frequency table
frequency_table <- table(rounded_values)

# Convert the frequency table to a data frame
frequency_df <- as.data.frame(frequency_table)
colnames(frequency_df) <- c("AwardPoundsAdjusted", "Frequency")

# Define your custom bands
custom_bands <- c(0, 25000, 50000, 100000, 200000, 350000, 550000, 750000, 1000000, Inf)



band_labels <- c("0-25", "26-50", "51-100", "101-200", "201-350", "351-550", "551-750", "751-1,000", "1,001+")
docvars$AwardPoundsBand <- cut(round(docvars$AwardPoundsAdjusted, digits = -3), breaks = custom_bands, labels = band_labels, include.lowest = TRUE)


print(table(docvars$AwardPoundsBand))


# get mean topic proportions per award band

topic_proportion_per_band <- aggregate(topic_proportions, 
                                       by = list(AwardPoundsBand = docvars$AwardPoundsBand), 
                                       mean
)

# transform df to longer for visualisation

topic_proportion_per_band <- pivot_longer(topic_proportion_per_band, 
                                          cols = -AwardPoundsBand, 
                                          names_to = "Topic", 
                                          values_to = "Proportion")

# Plot topic proportions by award band

topic_band_plot <- ggplot(topic_proportion_per_band, aes(x = AwardPoundsBand, 
                                                         y = Proportion, 
                                                         fill = Topic)) + 
  geom_bar(stat = "identity") + ylab("Proportion") + xlab("Value of Award (thousand pounds)") +
  scale_fill_manual(values = custom_palette, name = "Topic") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Topic Proportions by Value of the Award (Band)")

topic_band_plot

# Create master dataframe for visualisation
master_df <- read.xlsx("gtr_scraper/master_data.xlsx") %>%
  mutate(PI = paste(PIFirstName, PISurname, sep = " "), .keep = "unused") %>%
  mutate(StartDate = as.Date(StartDate, origin = "1899-12-30"), 
         EndDate = as.Date(EndDate, origin = "1899-12-30"),
         Region = as.factor(Region))%>%
  mutate(AwardPounds=format(AwardPounds, big.mark = ","))


save(master_df,
     slda1, 
     topic_proportion_per_band, 
     topic_proportion_per_year, 
     topic_proportions,
     custom_palette,
     json1,
     file = "outputs.RData")


filtered_abstarcts <- topic_proportions_long %>% 
  filter(Topic=="gender_sexuality") %>%
    arrange(desc(Proportion)) %>%
    inner_join(master_df, by = "ProjectReference")%>%
  filter(Proportion >= 0.1)
  



