library(tidyverse)

n_c1_2s <- c(.26, .381, .46, .5018, .528, .546)

n_c1_1s <- c(.168, .237, .274, .282, .295, .302)

n_c2_2s <- c(.021, .01, .001, .0015, 0, 0)

n_c2_1s <- c(.027, .015, .004, .0038, .001, 0)

s_c1_2s <- c(.196, .352, .444, .5035, .544, .59)

s_c1_1s <- c(.153, .234, .285, .2877, .332, .338)

s_c2_2s <- c(.039, .027, .012, .0038, .002, 0)

s_c2_1s <- c(.034, .023, .015, .0067, .004, .001)

ICC_val <- c(0.2, 0.4, 0.6, 0.7, 0.8, 0.9)

type1_ICC <- data.frame(ICC_val, n_c1_2s, n_c1_1s, n_c2_2s, n_c2_1s,
                                 s_c1_2s, s_c1_1s, s_c2_2s, s_c2_1s)
                                
graph <- ggplot(type1_ICC, aes(x=ICC_val)) +
  geom_line(aes(y=n_c1_1s), color = 'black', linetype='twodash') +
  geom_line(aes(y=n_c1_2s), color = 'black') +
  geom_line(aes(y=n_c2_1s), color = 'red', linetype='twodash') +
  geom_line(aes(y=n_c2_2s), color = 'red') +
  xlab("ICC") +
  ylab("Actual Type I Error") +
  ggtitle("Type I Error vs ICC Value") +
  theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"))
  


# ggplot(type1_ICC, aes(x=ICC_val)) +
#   geom_line(aes(y=s_c1_1s), color = s_c1_1s, linetype='twodash') +
#   geom_line(aes(y=s_c1_2s), color = 'blue') +
#   geom_line(aes(y=s_c2_1s), color = 'green', linetype='twodash') +
#   geom_line(aes(y=s_c2_2s), color = 'green') +
#   xlab("ICC") +
#   ylab("Actual Type 1 Error")

ggsave("type1_ICC.jpeg", graph)
