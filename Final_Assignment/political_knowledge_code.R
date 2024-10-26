

gles_mod <- gles_mod %>%
  mutate(pol_knowledge = ifelse(q5 < 0, NA, q5),
         pol_knowledge1 = case_when(pol_knowledge == 1 | pol_knowledge == 3 ~ "Wrong", 
                                    pol_knowledge == 2 ~ "Right",
                                    TRUE ~ NA))


gles_mod %>%
  filter(q5 %in% c(1, 2, 3), btw21_turnout1 == 1) %>%
  ggplot(aes(x = pol_knowledge1)) +
  geom_bar(aes(y = after_stat(prop), group = 1), width = 0.8) +
  annotate(geom = "text", x = 1.5, y = 0.75, label = "Germany", size = 7) +
  scale_y_continuous("Share of respondents", 
                     labels = scales::label_percent(scale = 100)) +
  expand_limits(y = 0.8) +
  labs(x = "In the federal elections you have two votes, the first vote and the\nsecond vote. Which vote decides how many seats each party will have in parliament?",
       caption = "Data source: GLES, post-election study, September/October 2022") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12))
ggsave("pol_knowledge.png", width = 9, height = 6, units = "in")
