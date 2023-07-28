library(readr)
library(ggplot2)
library(stargazer)
library(lme4)
library(MuMIn)
library(lmerTest)
library(rms)
library(sjstats)
library(ggpubr)

# Importing dataset
data <- read_csv("C:\\Users\\Jitendra Palaparty\\projects\\Package\\Package\\Data\\time_series.csv")
data['index'] <- lapply(data['index'] , factor)


data$time <- rep((1:12)[1:12 != 0], 926)

data["month_index"] <- NA
data$month_index <- rep((-6:6)[-6:6 != 0], 926)
data$month_index <- factor(data$month_index, levels=c((-6:6)[-6:6 != 0]),ordered=TRUE)

# Fit the model including fixed and random effects

# RDD - Number of non-merged PRs
vif(lm(log(nonmerged + 1) ~ time 
         + intervention
         + time_after_intervention
         + age_at_bot
         + log(total_number_pr_authors+1)
         + log(commits+1)
         + log(opened+1)
         + log(comments_nonmerged+1)
         + log(commits_nonmerged+ 1)
          , data=data))

rdd_nonmerged = lme4::lmer(log(nonmerged + 1) ~ age_at_bot
                           + log(total_number_pr_authors+1)
                           + log(commits+1)
                           + log(opened+1)
                           + log(comments_nonmerged+ 1)
                           + log(commits_nonmerged+ 1)
                           + time 
                           + intervention
                           + time_after_intervention
                           + (1 | name)
                           + (1 | lang)
                           , data=data)

r.squaredGLMM(rdd_nonmerged)

anova(rdd_nonmerged)

stargazer(rdd_nonmerged, type="text")

pv_nonmerged <- parameters::p_value(rdd_nonmerged)
p.adjust(pv_nonmerged$p, method = "BH", n = length(pv_nonmerged$p))


# RDD - Number of merged PRs
vif(lm(log(merged + 1) ~ time 
         + intervention 
         + time_after_intervention
         + age_at_bot
         + log(total_number_pr_authors+1)
         + log(commits+1)
         + log(opened+1)
         + log(comments_merged+1)
         + log(commits_merged+1)
         , data=data))

rdd_merged = lme4::lmer(log(merged + 1) ~ age_at_bot
                        + log(total_number_pr_authors+1)
                        + log(commits+1)
                        + log(opened+1)
                        + log(comments_merged + 1)
                        + log(commits_merged + 1)
                        + time 
                        + intervention 
                        + time_after_intervention
                        + (1 | name)
                        + (1 | lang)
                        , data=data)

r.squaredGLMM(rdd_merged)

anova(rdd_merged)

stargazer(rdd_merged, type="text")

pv_merged <- parameters::p_value(rdd_merged)
p.adjust(pv_merged$p, method = "BH", n = length(pv_merged$p))

# Plot
g1 <- ggplot(data, aes(data$month_index, as.numeric(data$merged+1))) + 
  geom_boxplot() +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), 
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme(text = element_text(size = 7)) +
  labs(y="Num. merged PRs", x="Month index")

g2 <- ggplot(data, aes(data$month_index, as.numeric(data$nonmerged+1))) + 
  geom_boxplot() +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), 
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme(text = element_text(size = 7)) +
  labs(y="Num. non-merged PRs", x="Month index")

ggarrange(g1, g2, nrow = 2, ncol = 1)

ggsave("merged_nonmerged.png", width = 4, height = 4, dpi = 300)

# RDD - Number of comments nonmerged
vif(lm(log(comments_nonmerged + 1) ~ time 
         + intervention 
         + time_after_intervention
         + age_at_bot
         + log(total_number_pr_authors+1)
         + log(commits + 1)
         + log(opened+1)
         + log(close_time_nonmerged+1)
         + log(commits_nonmerged+1)
         , data=data))


rdd_comments = lme4::lmer(log(comments_nonmerged+1) ~ age_at_bot
                          + log(total_number_pr_authors+1)
                          + log(commits+1)
                          + log(opened+1)
                          + log(close_time_nonmerged+1)
                          + log(commits_nonmerged+1)
                          + time 
                          + intervention 
                          + time_after_intervention
                          + (1 | name)
                          + (1 | lang), data=data)

r.squaredGLMM(rdd_comments)

anova(rdd_comments)

stargazer(rdd_comments, type="text")

pv_comments <- parameters::p_value(rdd_comments)
p.adjust(pv_comments$p, method = "BH", n = length(pv_comments$p))

# RDD - Number of comments merged
vif(lm(log(comments_merged + 1) ~ time 
       + intervention 
       + time_after_intervention
       + age_at_bot
       + log(total_number_pr_authors+1)
       + log(commits + 1)
       + log(opened+1)
       + log(close_time_merged+1)
       , data=data))


rdd_comments = lme4::lmer(log(comments_merged+1) ~ age_at_bot
                          + log(total_number_pr_authors+1)
                          + log(commits+1)
                          + log(opened+1)
                          + log(close_time_merged+1)
                          + log(commits_merged+1)
                          + time 
                          + intervention 
                          + time_after_intervention
                          + (1 | name)
                          + (1 | lang), data=data)

r.squaredGLMM(rdd_comments)

anova(rdd_comments)

stargazer(rdd_comments, type="text")

pv_comments <- parameters::p_value(rdd_comments)
p.adjust(pv_comments$p, method = "BH", n = length(pv_comments$p))

# Plot
g1 <- ggplot(data, aes(data$month_index, as.numeric(data$comments_merged+1))) + 
  geom_boxplot() +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), 
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme(text = element_text(size = 7)) +
  labs(y="Median comments on merged PRs", x="Month index")

g2 <- ggplot(data, aes(data$month_index, as.numeric(data$comments_nonmerged+1))) + 
  geom_boxplot() +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), 
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme(text = element_text(size = 7)) +
  labs(y="Median comments on non-merged PRs", x="Month index")

ggarrange(g1, g2, nrow = 2, ncol = 1)

ggsave("comments.png", width = 4, height = 4, dpi = 300)

# RDD - Mean PR latency nonmerged
vif(lm(log(close_time_nonmerged+1 ) ~ time 
         + intervention 
         + time_after_intervention
         + age_at_bot
         + log(total_number_pr_authors+1)
         + log(commits+1)
         + log(opened+1)
         + log(comments_nonmerged + 1)
          + log(commits_nonmerged+1)
         , data=data))

rdd_latency = lme4::lmer(log(close_time_nonmerged+1) ~ age_at_bot
                         + log(total_number_pr_authors+1)
                         + log(commits+1)
                         + log(opened+1)
                         + log(comments_nonmerged+1)
                         + log(commits_nonmerged+1)
                         + time 
                         + intervention 
                         + time_after_intervention
                         + (1 | name)
                         + (1 | lang), data=data)

r.squaredGLMM(rdd_latency)

anova(rdd_latency)

stargazer(rdd_latency, type="text")

pv_latency <- parameters::p_value(rdd_latency)
p.adjust(pv_latency$p, method = "BH", n = length(pv_latency$p))

# RDD - Mean PR latency merged
vif(lm(log(close_time_merged + 1) ~ time 
       + intervention 
       + time_after_intervention
       + age_at_bot
       + log(total_number_pr_authors+1)
       + log(commits+1)
       + log(opened+1)
       + log(comments_merged + 1)
       + log(commits_merged+1)
       , data=data))

rdd_latency = lme4::lmer(log(close_time_merged+1) ~ age_at_bot
                         + log(total_number_pr_authors+1)
                         + log(commits+1)
                         + log(opened+1)
                         + log(comments_merged+1)
                         + log(commits_merged+1) 
                         + time 
                         + intervention 
                         + time_after_intervention
                         + (1 | name)
                         + (1 | lang), data=data)

r.squaredGLMM(rdd_latency)

anova(rdd_latency)

stargazer(rdd_latency, type="text")

pv_latency <- parameters::p_value(rdd_latency)
p.adjust(pv_latency$p, method = "BH", n = length(pv_latency$p))

# Plot
g1 <- ggplot(data, aes(data$month_index, as.numeric(data$close_time_merged+1))) + 
  geom_boxplot() +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), 
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme(text = element_text(size = 7)) +
  labs(y="Mean time to merged PRs", x="Month index")

g2 <- ggplot(data, aes(data$month_index, as.numeric(data$close_time_nonmerged+1))) + 
  geom_boxplot() +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), 
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme(text = element_text(size = 7)) +
  labs(y="Mean time to close non-merged PRs", x="Month index")

ggarrange(g1, g2, nrow = 2, ncol = 1)

ggsave("latency.png", width = 4, height = 4, dpi = 300)


# RDD - Mean PR commits
vif(lm(log(commits_nonmerged+1 ) ~ time 
       + intervention 
       + time_after_intervention
       + age_at_bot
       + log(total_number_pr_authors+1)
       + log(commits+1)
       + log(opened+1)
       + log(comments_nonmerged + 1)
       , data=data))

rdd_nonmerged_commits = lme4::lmer(log(commits_nonmerged+1) ~ age_at_bot
                         + log(total_number_pr_authors+1)
                         + log(commits+1)
                         + log(opened+1)
                         + log(comments_nonmerged+1)
                         + time 
                         + intervention 
                         + time_after_intervention
                         + (1 | name)
                         + (1 | lang), data=data)

r.squaredGLMM(rdd_nonmerged_commits)

anova(rdd_nonmerged_commits)

stargazer(rdd_nonmerged_commits, type="text")

pv_nonmerged_commits <- parameters::p_value(rdd_nonmerged_commits)
p.adjust(pv_nonmerged_commits$p, method = "BH", n = length(pv_nonmerged_commits$p))

# RDD - Mean PR commits
vif(lm(log(commits_merged + 1) ~ time 
       + intervention 
       + time_after_intervention
       + age_at_bot
       + log(total_number_pr_authors+1)
       + log(commits+1)
       + log(opened+1)
       + log(comments_merged + 1)
       , data=data))

rdd_merged_commits = lme4::lmer(log(commits_merged+1) ~ age_at_bot
                         + log(total_number_pr_authors+1)
                         + log(commits+1)
                         + log(opened+1)
                         + log(comments_merged+1)
                         + time 
                         + intervention 
                         + time_after_intervention
                         + (1 | name)
                         + (1 | lang), data=data)

r.squaredGLMM(rdd_merged_commits)

anova(rdd_merged_commits)

stargazer(rdd_merged_commits, type="text")

pv_merged_commits <- parameters::p_value(rdd_merged_commits)
p.adjust(pv_merged_commits$p, method = "BH", n = length(pv_merged_commits$p))

# Plot
g1 <- ggplot(data, aes(data$month_index, as.numeric(data$commits_merged+1))) + 
  geom_boxplot() +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), 
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme(text = element_text(size = 7)) +
  labs(y="Mean time to merged commits", x="Month index")

g2 <- ggplot(data, aes(data$month_index, as.numeric(data$commits_nonmerged+1))) + 
  geom_boxplot() +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), 
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme(text = element_text(size = 7)) +
  labs(y="Mean time to close non-merged commits", x="Month index")

ggarrange(g1, g2, nrow = 2, ncol = 1)

ggsave("commits.png", width = 4, height = 4, dpi = 300)

