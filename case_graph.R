library(tidyverse)

case1df <- read.csv(".//ICC-0.7_10000/case1_icc0.7_10000.csv")
case2df <- read.csv(".//ICC-0.7_10000/case2_icc0.7_10000.csv")

# took one sample data in a particular model
# (model 3 was arbitrarily chosen here)
case1ts <- case1df %>%
  filter(iter == 3) %>%
  select(group_model_3a, model_3a, mean_model_3a, clst_model_3a) %>%
  mutate(clst_model_3a = as.factor(clst_model_3a))

c1 <- case1ts %>%
  ggplot(aes(x = paste(group_model_3a, clst_model_3a, lex.order = T),
             y = model_3a, color = clst_model_3a)) +
  geom_jitter(width = 0.01, show.legend = F) +
  theme_bw() +
  coord_cartesian(xlim = c(0, 22), ylim = c(-2.5, 3),
                  expand = FALSE, clip = "off") +
  ylab("Response") +
  ggtitle("Sample Data under Case 1") +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(size = 15, face = "bold")) +
  annotate(geom = "text", x = 1:20, y = -2.7,
           label = unique(case1ts$clst_model_3a), size = 4) +
  annotate(geom = "text", x = c(5, 15), y = -3,
           label = c("Group 1", "Group 2"), size = 6, fontface = 2) +
  geom_vline(xintercept = 10.5)

ggsave("case1.jpeg", c1)
# case 2
# took one sample data in a particular model
# (model 1 was arbitrarily chosen here)
case2ts <- case2df %>%
  filter(iter == 3) %>%
  select(group_model_3b, model_3b, mean_model_3b, clst_model_3b) %>%
  mutate(clst_model_3b = as.factor(clst_model_3b))

c2 <- case2ts %>%
  ggplot(aes(x = paste(group_model_3b, clst_model_3b, lex.order = T),
             y = model_3b, color = clst_model_3b)) +
  geom_jitter(width = 0.01, show.legend = F) +
  theme_bw() +
  coord_cartesian(xlim = c(0, 41), ylim = c(-2.5, 3),
                  expand = FALSE, clip = "off") +
  ylab("Response") +
  ggtitle("Sample Data under Case 2") +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(size = 15, face = "bold")) +
  annotate(geom = "text", x = 1:40, y = -2.7,
           label = rep(unique(case1ts$clst_model_3a), 2), size = 2.5) +
  annotate(geom = "text", x = c(10, 30), y = -3,
           label = c("Group 1", "Group 2"), size = 6, fontface = 2) +
  geom_vline(xintercept = 20.5)
ggsave("case2.jpeg", c2)
