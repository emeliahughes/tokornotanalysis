install.packages("tidyverse")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("powerjoin")
install.packages("irr")
install.packages("FSA")
library(tidyverse)
library(dplyr)
library(readr)
library("ggplot2")
library("ggpubr")
library(powerjoin)
library("irr")
library(FSA)
library(ggstatsplot)

# INITIALIZE DATA
pairs <- read.csv("pair-data.csv", stringsAsFactors = FALSE)
names(pairs) <- c("category", "pair_id", "pair_level", "video1", "video1_level", "video2", "video2_level")

videos <- read.csv("video-data.csv", stringsAsFactors = FALSE)
names(videos) <- c("category", "comment_count", "create_time", "id", "like_count", "share_count", "username", "video_description", "view_count")
  
quiz_data <- read.csv("quiz_results.csv", stringsAsFactors = FALSE, header = FALSE)
names(quiz_data) <- c("user_id", "pair_id", "timestamp", "video1", "video2", "preference", "popular_vid")
quiz_data <- quiz_data[quiz_data$user_id > 50, ]

survey <- read.csv("survey_results.csv", stringsAsFactors = FALSE, header = FALSE)
names(survey) <- c("user_id", "timestamp", "q1", "q2", "q3", "q4", "q5", "q6", "q7")

# MERGE TABLES FOR INFO ACCESS & CLEAN DATA
names(videos) <- c("category", "comment_count", "create_time", "video1_id", "like_count_v1", "share_count", "username", "video_description", "view_count_v1")
quiz <- power_full_join(quiz_data, videos[ , c("video1_id", "like_count_v1", "view_count_v1")], by = join_by(video1 == video1_id), conflict = coalesce_xy)
names(videos) <- c("category", "comment_count", "create_time", "video2_id", "like_count_v2", "share_count", "username", "video_description", "view_count_v2")
quiz <- power_full_join(quiz, videos[ , c("video2_id", "like_count_v2", "view_count_v2")], by = join_by(video2 == video2_id), conflict = coalesce_xy)
names(videos) <- c("category", "comment_count", "create_time", "id", "like_count", "share_count", "username", "video_description", "view_count")

quiz <- power_full_join(quiz, pairs, by = join_by(pair_id == pair_id), conflict = coalesce_xy)

quiz$pair_level <- case_when(
  quiz$pair_level == "viral-high" ~ "V-H",
  quiz$pair_level == "viral-medium" ~ "V-M",
  quiz$pair_level == "viral-low" ~ "V-L",
  quiz$pair_level == "viral-very_low" ~ "V-VL",
  quiz$pair_level == "high-medium" ~ "H-M",
  quiz$pair_level == "high-low" ~ "H-L",
  quiz$pair_level == "high-very_low" ~ "H-VL",
  quiz$pair_level == "medium-low" ~ "M-L",
  quiz$pair_level == "medium-very_low" ~ "M-VL",
  quiz$pair_level == "low-very_low" ~ "L-VL",
)

# CORRECT COUNT COLUMN
quiz <- quiz %>%
  mutate(correct=case_when(
    popular_vid=="correct" ~ 1,
    popular_vid=="wrong" ~ 0
  ))

quiz <- quiz %>%
  mutate(more_popular=case_when(
    view_count_v1 > view_count_v2 ~ video1,
    view_count_v2 > view_count_v1 ~ video2,
  )) %>%
  mutate(pref_popular=case_when(
    preference == more_popular ~ 1,
    preference != more_popular ~ 0,
  ))

quiz <- na.omit(quiz)

# GENERAL OVERVIEW
quiz_overview <- summarize(
  quiz,
  sum_correct = sum(correct), 
  sum_pref = sum(pref_popular),
  view_diff = abs(mean(view_count_v1) - mean(view_count_v2)),
  count = n(),
  correct_ratio = sum_correct/count,
  pref_ratio = sum_pref/count
)

# PREFERENCE ANALYSIS
quiz_pref <- quiz %>%
  mutate(more_popular=case_when(
    view_count_v1 > view_count_v2 ~ video1,
    view_count_v2 > view_count_v1 ~ video2,
  )) %>%
  mutate(pref_popular=case_when(
    preference == more_popular ~ "more popular",
    preference != more_popular ~ "less popular",
  ))

num_correct <- quiz_pref %>% group_by(correct) %>%
  summarise(count = n()) %>% subset(correct == 1, select = count) %>% 
  pull(count)

num_wrong <- quiz_pref %>% group_by(correct) %>%
  summarise(count = n()) %>% subset(correct == 0, select = count) %>% 
  pull(count)

quiz_pref_data <- quiz_pref %>% group_by(correct, pref_popular) %>%
  summarise(count = n()) 

quiz_pref_data$ratio <- with(quiz_pref_data, ifelse(correct == 1, count/num_correct, count/num_wrong))

# OVERVIEW BY PAIR LEVEL
quiz_by_level <- group_by(quiz, pair_level) %>%
  reframe(
    sum_correct = sum(correct), 
    sum_pref = sum(pref_popular),
    view_diff = abs(mean(view_count_v1) - mean(view_count_v2)),
    count = n(),
    correct_ratio = sum_correct/count,
    pref_ratio = sum_pref/count
  ) 

# OVERVIEW BY CATEGORY
quiz_by_category <- quiz %>% group_by(category) %>%
  reframe(
    sum_correct = sum(correct), 
    sum_pref = sum(pref_popular),
    view_diff = abs(mean(view_count_v1) - mean(view_count_v2)),
    count = n(),
    correct_ratio = sum_correct/count,
    pref_ratio = sum_pref/count
  )

# OVERVIEW BY PAIRS
# overview is filtered so that only pairs with 3 or more raters are used
quiz_by_pairs <- quiz %>% group_by(pair_level, pair_id) %>%
  reframe(
    sum_correct = sum(correct), 
    sum_pref = sum(pref_popular),
    view_diff = abs(mean(view_count_v1) - mean(view_count_v2)),
    count = n(),
    correct_ratio = sum_correct/count,
    pref_ratio = sum_pref/count
  ) %>%
  filter(count > 2)

# VIEW DIFF X CORRECT RATIO GRAPH
filter_low <- filter(quiz_by_pairs, !str_detect(pair_level, "V-"))
filter_high <- filter(quiz_by_pairs, str_detect(pair_level, "V-"))

ggplot(data = filter_low) +
  geom_point(mapping = aes(x = view_diff, y = correct_ratio)) +
  geom_smooth(mapping = aes(x = view_diff, y = correct_ratio)) +
  facet_wrap(~pair_level) +
  scale_y_continuous(limits=c(0,max(filter_low$correct_ratio)))+
  scale_x_continuous(limits=c(0,max(filter_low$view_diff)))

ggplot(data = filter_high) +
  geom_point(mapping = aes(x = view_diff, y = correct_ratio)) +
  geom_smooth(mapping = aes(x = view_diff, y = correct_ratio)) +
  facet_wrap(~pair_level)

ggplot(data = quiz_by_pairs) +
  geom_point(mapping = aes(x = view_diff, y = correct_ratio)) +
  geom_smooth(mapping = aes(x = view_diff, y = correct_ratio))

# CATEGORY PLOT
ggplot(data = quiz_by_category) +
  geom_point(mapping = aes(x = category, y = correct_ratio)) +
  geom_smooth(mapping = aes(x = category, y = correct_ratio))

# PREF SCORE VS CORRECT SCORE FOR PAIRS WITH MORE THAN 2 RATERS

ggplot(data = quiz_by_pairs) +
  geom_point(mapping = aes(x = pref_ratio, y = correct_ratio)) +
  geom_smooth(mapping = aes(x = pref_ratio, y = correct_ratio))

ggplot(data = quiz_by_category) +
  geom_point(mapping = aes(x = pref_ratio, y = correct_ratio)) +
  geom_smooth(mapping = aes(x = pref_ratio, y = correct_ratio))

ggplot(data = quiz_by_level) +
  geom_point(mapping = aes(x = pref_ratio, y = correct_ratio)) +
  geom_smooth(mapping = aes(x = pref_ratio, y = correct_ratio))

# KRUSKAL WALLIS ON LEVEL

group_by(quiz_by_pairs, pair_level) %>%
  summarise(
    count = n(),
    mean = mean(correct_ratio, na.rm = TRUE),
    sd = sd(correct_ratio, na.rm = TRUE),
    median = median(correct_ratio, na.rm = TRUE),
    IQR = IQR(correct_ratio, na.rm = TRUE)
  )

ggboxplot(quiz_by_pairs, x = "pair_level", y = "correct_ratio", 
          color = "pair_level", 
          ylab = "Ratio Correct", xlab = "Pair Level") + stat_compare_means()

ggline(quiz_by_pairs, x = "pair_level", y = "correct_ratio", 
       add = c("mean_se", "jitter"), 
       ylab = "Ratio Correct", xlab = "Pair Level") + stat_compare_means()

kruskal.test(correct_ratio ~ pair_level, data = quiz_by_pairs)

pairwise.wilcox.test(quiz_by_pairs$correct_ratio, quiz_by_pairs$pair_level,
                     p.adjust.method = "BH")

dunnTest(correct_ratio ~ pair_level, data = quiz_by_pairs, method = "holm")


ggbetweenstats(
  data = quiz_by_pairs,
  x = pair_level,
  y = correct_ratio,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)
