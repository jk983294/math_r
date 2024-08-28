x <- exp(rnorm(100))
d <- tibble(x=x)
ggplot(data=d, mapping=aes(x=1, y=x)) +
  geom_boxplot() +
  scale_x_continuous(breaks=NULL) +
  labs(x=NULL, y=NULL)
