

# modify data
gles_mod <- gles_mod %>%
  mutate(spd_identification = ifelse(q75a == 4, 1, 0),
         fdp_identification = ifelse(q75a == 5, 1, 0),
         green_identification = ifelse(q75a == 6, 1, 0),
         afd_identification = ifelse(q75a == 322, 1, 0),
         union_identification = ifelse(q75a == 1, 1, 0))


gles_mod_partisan <- gles_mod %>%
  filter(!(q75a < 0 | q75a == 808))

## plots 
gles_mod_partisan %>%
  ggplot(aes(x = household_income, fill = factor(spd_identification))) +
  geom_histogram(aes(y = after_stat(density)), position = "dodge") +
  scale_fill_manual("", 
                    labels = c("0" = "Does not identify with SPD", 
                               "1" = "Identifies with SPD"),
                    values = c("red1", "darkred")) +
  scale_x_continuous("Net Monthly Household Income",
                      breaks = seq(1, 13, 1),
                      labels = c("<500€", "[500-750€)",
                                 "[750-1000€)", "[1000-1250€)", 
                                 "[1250-1500€)", "[1500-2000€)",
                                 "[2000-2500€)", "[2500-3000€)",
                                 "[3000-4000€)", "[4000-5000€)",
                                 "[5000-7000€)", "[7000-10000€)",
                                 ">10000€")) +
  labs(y = "Density") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.85),
        legend.position = "bottom")
ggsave("spd_income_identification.png", width = 9, height = 6, units = "in")

gles_mod_partisan %>%
  ggplot(aes(x = household_income, fill = factor(green_identification))) +
  geom_histogram(aes(y = after_stat(density)), position = "dodge") +
  scale_fill_manual("", 
                    labels = c("0" = "Does not identify with Greens", 
                               "1" = "Identifies with Greens"),
                    values = c("lightgreen", "darkgreen")) +
  scale_x_continuous("Net Monthly Household Income",
                     breaks = seq(1, 13, 1),
                     labels = c("<500€", "[500-750€)",
                                "[750-1000€)", "[1000-1250€)", 
                                "[1250-1500€)", "[1500-2000€)",
                                "[2000-2500€)", "[2500-3000€)",
                                "[3000-4000€)", "[4000-5000€)",
                                "[5000-7000€)", "[7000-10000€)",
                                ">10000€")) +
  labs(y = "Density") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.85),
        legend.position = "bottom")
ggsave("green_income_identification.png", width = 9, height = 6, units = "in")

gles_mod_partisan %>%
  ggplot(aes(x = household_income, fill = factor(fdp_identification))) +
  geom_histogram(aes(y = after_stat(density)), position = "dodge") +
  scale_fill_manual("", 
                    labels = c("0" = "Does not identify with FDP", 
                               "1" = "Identifies with FDP"),
                    values = c("darkorange", "yellow")) +
  scale_x_continuous("Net Monthly Household Income",
                     breaks = seq(1, 13, 1),
                     labels = c("<500€", "[500-750€)",
                                "[750-1000€)", "[1000-1250€)", 
                                "[1250-1500€)", "[1500-2000€)",
                                "[2000-2500€)", "[2500-3000€)",
                                "[3000-4000€)", "[4000-5000€)",
                                "[5000-7000€)", "[7000-10000€)",
                                ">10000€")) +
  labs(y = "Density") +
  expand_limits(y = 0.6) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.85),
        legend.position = "bottom")
ggsave("fdp_income_identification.png", width = 9, height = 6, units = "in")


gles_mod_partisan %>%
  ggplot(aes(x = household_income, fill = factor(afd_identification))) +
  geom_histogram(aes(y = after_stat(density)), position = "dodge") +
  scale_fill_manual("", 
                    labels = c("0" = "Does not identify with AfD", 
                               "1" = "Identifies with AfD"),
                    values = c("blue", "darkblue")) +
  scale_x_continuous("Net Monthly Household Income",
                     breaks = seq(1, 13, 1),
                     labels = c("<500€", "[500-750€)",
                                "[750-1000€)", "[1000-1250€)", 
                                "[1250-1500€)", "[1500-2000€)",
                                "[2000-2500€)", "[2500-3000€)",
                                "[3000-4000€)", "[4000-5000€)",
                                "[5000-7000€)", "[7000-10000€)",
                                ">10000€")) +
  labs(y = "Density") +
  expand_limits(y = 0.5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.85),
        legend.position = "bottom")
ggsave("afd_income_identification.png", width = 9, height = 6, units = "in")


gles_mod_partisan %>%
  ggplot(aes(x = household_income, fill = factor(union_identification))) +
  geom_histogram(aes(y = after_stat(density)), position = "dodge") +
  scale_fill_manual("", 
                    labels = c("0" = "Does not identify with CDU/CSU", 
                               "1" = "Identifies with CDU/CSU"),
                    values = c("darkgrey", "black")) +
  scale_x_continuous("Net Monthly Household Income",
                     breaks = seq(1, 13, 1),
                     labels = c("<500€", "[500-750€)",
                                "[750-1000€)", "[1000-1250€)", 
                                "[1250-1500€)", "[1500-2000€)",
                                "[2000-2500€)", "[2500-3000€)",
                                "[3000-4000€)", "[4000-5000€)",
                                "[5000-7000€)", "[7000-10000€)",
                                ">10000€")) +
  labs(y = "Density") +
  expand_limits(y = 0.5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.85),
        legend.position = "bottom")
ggsave("cdu_csu_income_identification.png", width = 9, height = 6, units = "in")