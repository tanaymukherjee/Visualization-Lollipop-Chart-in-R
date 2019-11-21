# Tanay Mukherjee
# 21 Nov. 2019

library(dplyr)
library(tidyr)
library(ggplot2)

head(midwest)

df <- data.frame(midwest)

ohio_top25 <- midwest %>% filter(state == "OH") %>%  select(county, percollege) %>%
  arrange(desc(percollege)) %>%  top_n(25) %>%
  arrange(percollege) %>%  mutate(county = factor(county, levels = .$county))

# bar chart
ggplot(ohio_top25, aes(county, percollege)) +  geom_bar(stat = "identity") +  coord_flip()


# dot plot
ggplot(ohio_top25, aes(percollege, county)) +  geom_point()


# lollipop chart
ggplot(ohio_top25, aes(percollege, county)) +
  geom_segment(aes(x = 0, y = county, xend = percollege, yend = county), color = "grey50") +
  geom_point()

ohio <- midwest %>%  filter(state == "OH") %>%  select(county, percollege) %>%
  arrange(percollege) %>%  mutate(Avg = mean(percollege, na.rm = TRUE),
         Above = ifelse(percollege - Avg > 0, TRUE, FALSE),
         county = factor(county, levels = .$county))

head(ohio)

ggplot(ohio, aes(percollege, county, color = Above)) +
  geom_segment(aes(x = Avg, y = county, xend = percollege, yend = county), color = "grey50") +
  geom_point()

top10 <- midwest %>%  select(state, county, percollege) %>%  group_by(state) %>%  
  arrange(desc(percollege)) %>%  top_n(10) %>%  arrange(percollege) %>%
  unite(county_st, county, state, remove = FALSE) %>%
  mutate(county_st = factor(county_st, levels = .$county_st))

head(top10)

ggplot(top10, aes(percollege, county_st)) +
  geom_segment(aes(x = 0, y = county_st, xend = percollege, yend = county_st), color = "grey50") +
  geom_point() +  scale_y_discrete(labels = abbreviate) +  facet_wrap(~ state, scales = "free_y")

OH_top10 <- midwest %>%  select(state, county, percollege) %>%  filter(state == "OH") %>%  
  arrange(desc(percollege)) %>%  top_n(10) %>%  arrange(percollege) %>%
  mutate(county = factor(county, levels = .$county))

ggplot(OH_top10, aes(percollege, county, label = round(percollege, 1))) +
  geom_segment(aes(x = 0, y = county, xend = percollege, yend = county), color = "grey50") +
  geom_point() +  geom_text(nudge_x = 1.5)

ggplot(OH_top10, aes(percollege, county, label = paste0(round(percollege, 0), "%"))) +
  geom_segment(aes(x = 0, y = county, xend = percollege, yend = county), color = "grey50") +
  geom_point(size = 7) + geom_text(color = "white", size = 2)

ggplot(ohio, aes(percollege/100, county, color = Above)) +
  geom_segment(aes(x = Avg/100, y = county, xend = percollege/100, yend = county), color = "grey50") +
  geom_point() +
  annotate("text", x = .25, y = "ALLEN", label = "Above Average", color = "#00BFC4", size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = .25, y = "FULTON", label = "Below Average", color = "#F8766D", size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = .25, xend = .25 , y = "ASHLAND", yend = "DEFIANCE"),
               arrow = arrow(length = unit(0.2,"cm")), color = "#00BFC4") +
  geom_segment(aes(x = .25, xend = .25 , y = "KNOX", yend = "PUTNAM"),
               arrow = arrow(length = unit(0.2,"cm")), color = "#F8766D") +
  scale_x_continuous(labels = scales::percent, expand = c(0, 0), limits = c(.07, .33)) +
  labs(title = "College Educated Adults in Ohio Counties",
       subtitle = "The average percent of college educated adults in Ohio is 16.89%. Franklin, Greene, Geauga, and \nDelaware counties lead Ohio with over 30% of their adults being college educated while Vinton, \nAdams, Holmes, and Perry trailing with less than 10% of their adults being college educated.",
       caption = "U.S. Census Bureau: 2000 Census") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        text = element_text(family = "Georgia"),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 20, margin = margin(b = 10), hjust = 0),
        plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25, l = -25)),
        plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0))


# --
# Repeat it for the state == "IL"

IL_top25 <- midwest %>% filter(state == "IL") %>%
  select(county, percollege) %>% arrange(desc(percollege)) %>% top_n(25) %>%
  arrange(percollege) %>% mutate(county = factor(county, levels = .$county))

# bar chart
ggplot(IL_top25, aes(county, percollege)) +
  geom_bar(stat = "identity") +
  coord_flip()


# dot plot
ggplot(IL_top25, aes(percollege, county)) +  geom_point()


# lollipop chart
ggplot(IL_top25, aes(percollege, county)) + 
  geom_segment(aes(x = 0, y = county, xend = percollege, yend = county), color = "grey50") +
  geom_point()

IL <- midwest %>% filter(state == "IL") %>% select(county, percollege) %>%
  arrange(percollege) %>% mutate(Avg = mean(percollege, na.rm = TRUE),
         Above = ifelse(percollege - Avg > 0, TRUE, FALSE),
         county = factor(county, levels = .$county))

head(IL)

ggplot(IL, aes(percollege, county, color = Above)) +
  geom_segment(aes(x = Avg, y = county, xend = percollege, yend = county), color = "grey50") +
  geom_point()

top10 <- midwest %>% select(state, county, percollege) %>% group_by(state) %>%
  arrange(desc(percollege)) %>% top_n(10) %>%
  arrange(percollege) %>% unite(county_st, county, state, remove = FALSE) %>%
  mutate(county_st = factor(county_st, levels = .$county_st))

head(top10)

ggplot(top10, aes(percollege, county_st)) +
  geom_segment(aes(x = 0, y = county_st, xend = percollege, yend = county_st), color = "grey50") +
  geom_point() + scale_y_discrete(labels = abbreviate) + facet_wrap(~ state, scales = "free_y")

IL_top10 <- midwest %>% select(state, county, percollege) %>% filter(state == "IL") %>%
  arrange(desc(percollege)) %>% top_n(10) %>% arrange(percollege) %>%
  mutate(county = factor(county, levels = .$county))

ggplot(IL_top10, aes(percollege, county, label = round(percollege, 1))) +
  geom_segment(aes(x = 0, y = county, xend = percollege, yend = county), color = "grey50") +
  geom_point() +
  geom_text(nudge_x = 1.5)

ggplot(IL_top10, aes(percollege, county, label = paste0(round(percollege, 0), "%"))) +
  geom_segment(aes(x = 0, y = county, xend = percollege, yend = county), color = "grey50") +
  geom_point(size = 7) + geom_text(color = "white", size = 2)

ggplot(IL, aes(percollege/100, county, color = Above)) +
  geom_segment(aes(x = Avg/100, y = county, xend = percollege/100, yend = county), color = "grey50") +
  geom_point() +
  annotate("text", x = .25, y = "ALLEN", label = "Above Average", color = "#00BFC4", size = 3, hjust = -0.1, vjust = .75) +
  annotate("text", x = .25, y = "FULTON", label = "Below Average", color = "#F8766D", size = 3, hjust = -0.1, vjust = -.1) +
  geom_segment(aes(x = .25, xend = .25 , y = "ASHLAND", yend = "DEFIANCE"),
               arrow = arrow(length = unit(0.2,"cm")), color = "#00BFC4") +
  geom_segment(aes(x = .25, xend = .25 , y = "KNOX", yend = "PUTNAM"),
               arrow = arrow(length = unit(0.2,"cm")), color = "#F8766D") +
  scale_x_continuous(labels = scales::percent, expand = c(0, 0), limits = c(.07, .33)) +
  labs(title = "College Educated Adults in IL Counties",
       caption = "U.S. Census Bureau: 2000 Census") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        text = element_text(family = "Georgia"),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 20, margin = margin(b = 10), hjust = 0),
        plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25, l = -25)),
        plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0))
