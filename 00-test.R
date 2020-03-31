library(BGGM)
library(networktools)

set.seed(69)

# DSM-5 pays more attention to the behavioral symptoms that accompany PTSD and proposes four
# distinct diagnostic clusters instead of three. They are described as re-experiencing, avoidance, negative
# cognitions and mood, and arousal.
# 
# Re-experiencing covers spontaneous memories of the traumatic event, recurrent dreams related to it,
# flashbacks or other intense or prolonged psychological distress. Avoidance refers to distressing memories, thoughts, feelings or external reminders of the event.
# Negative cognitions and mood represents myriad feelings, from a persistent and distorted sense of
# blame of self or others, to estrangement from others or markedly diminished interest in activities, to an
# inability to remember key aspects of the event.
# 
# Finally, arousal is marked by aggressive, reckless or self-destructive behavior, sleep disturbances, hypervigilance or related problems. The current manual emphasizes the "flight" aspect associated with PTSD;
# the criteria of DSM-5 also account for the "fight" reaction often seen.

# comm1: re-experiencing
# comm2: avoidance
# comm3: negative cognitions and mood
# comm4: arousal

# set column names and communities
col_names <- c(
  "intrusive_thoughts",
  "nightmares",
  "flashbacks",
  "emotional_cue_reactivity",
  "psychological_cue_reactivity",
  "thought_avoidance",
  "reminder_avoidance",
  "amnesia",
  "negative_beliefs",
  "blame",
  "negative_emotions",
  "loss_interest",
  "detachment",
  "restricted_affect",
  "irritability",
  "recklesness",
  "hypervigilance",
  "startle_response",
  "diff_concentrating",
  "sleep_disturbance"
)
colnames(ptsd) <- col_names

comms <- list("B" = 1:5, "C" = 6:7, "D" = 8:14, "E" = 15:20)


# Split data
split <- sample(nrow(ptsd), size = floor(nrow(ptsd)*.33))
ptsd_exp <- ptsd[split, ]

fit <- explore(ptsd, cores = 4)

e <- select(fit, BF_cut = 3)

summary(e)


plot_adjacency(e$Adj_10)

plot(e, 
     node_groups = c(
       rep("Re-experiencing", 5),
       rep("Avoidance", 2),
       rep("Neg. Cognition & Mood", 7),
       rep("Arousal", 6)
     ),
     node_labels_color = c(
       rep("orange", 5),
       rep("green", 2),
       rep("red", 7),
       rep("black", 6)
     ),
     txt_size = )$plt

bridge_statistics <- bridge(e$Adj_10, communities = comms)
bridge_statistics

plot(bridge_statistics) +
  ggplot2::theme_bw()


plot_adjacency(e$Adj_10, 
     node_groups = c(
       rep("Re-experiencing", 5),
       rep("Avoidance", 2),
       rep("Neg. Cognition & Mood", 7),
       rep("Arousal", 6)
     ),
     node_labels_color = c(
       rep("orange", 5),
       rep("green", 2),
       rep("red", 7),
       rep("black", 6)
     ),
     txt_size = 6)

bridge_statistics$`Bridge Strength`




# confirmatory
ptsd_conf <- ptsd[-split, ]
BGGM::explore()



#-----------
networktools::bridge(slct1$partials_positive, communities = comms)$`Bridge Strength` %>% sort(decreasing=T)

make_adj_plot(slct1$Adj_20,
              node_labels = node_names,
              communities = comms,
              bridges = c("D1", "B4"),
              fade = NULL,
              node_size = 12)  +
  ggthemes::scale_fill_few() +
  guides(fill = F) +
  theme_facet()

make_adj_plot(slct2$Adj_20,
              node_labels = node_names,
              communities = comms,
              bridges = c("D1", "B4"),
              fade = NULL,
              node_size = 12)  +
  ggthemes::scale_fill_few() +
  guides(fill = F) +
  theme_facet()