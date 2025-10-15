# Replication materials for "Subsistence fishing patterns near food deserts"

# Source: github.com/savannahswinea/SubsistenceFishing

# Savannah Swinea
# swinea.s@northeastern.edu

#____________________________________________________________________________________________________________________

library(vegan)
library(tidyverse)

current_path = rstudioapi::getActiveDocumentContext()$path # finds the file path for this R script
setwd(dirname(current_path)) # sets your working directory equal to that file path

df <- read.csv("SubsistenceFishing.csv")

#____________________________________________________________________________________________________________________
# Variable Cleaning and Re-Coding

df <- df %>%
  mutate(SubsistenceMotivation = case_when(
    is.na(SubsistenceMotivation) | SubsistenceMotivation == " " ~ 0,
    TRUE ~ 1)) %>%
  mutate(FishAvid = case_when(
    FishAvid == "Fishing is an enjoyable, but infrequent activity that is incidental to other travel and outdoor interests. I am not highly skilled in fishing, rarely read fishing articles, and do not own much fishing equipment beyond the basic necessities." ~ 1,
    FishAvid == "Fishing is an important, but not exclusive outdoor activity. I occasionally read fishing articles and purchase additional equipment to aid in fishing, my participation in fishing is inconsistent, and I am moderately skilled in fishing."  ~ 2,
    FishAvid == "Fishing is my primary outdoor activity. I purchase ever-increasing amounts of equipment to aid in fishing, go fishing every chance that I get, consider myself to be highly skilled in fishing, and frequently read fishing articles."  ~ 3,
    TRUE ~ NA))

df <- df %>%
  mutate(across(c("T_Anything", "T_AtlanticCroaker", "T_Bass", "T_BlueCrab", "T_Bream", "T_CatfishSpp",
                  "T_CatfishFresh", "T_CatfishG", "T_CatfishH", "T_CatfishSalt", "T_Crappie", 
                  "T_DrumSpp", "T_DrumB", "T_DrumP", "T_DrumR", "T_Redfish", "T_Flounder", 
                  "T_Grouper", "T_Mackerel", "T_MahiMahi", "T_MulletSpp", "T_MulletG", "T_MulletB", 
                  "T_Perch", "T_Shark", "T_Sheepshead", "T_Shrimp", "T_SnapSpp", "T_SnapMang",
                  "T_SnapRed", "T_TroutSpp", "T_TroutSpec", "T_TroutW"), 
                ~ ifelse(is.na(.) | . == " ", 0, 1))) %>%
  mutate(T_Catfish = ifelse(T_CatfishSpp == 1 | T_CatfishFresh == 1 | T_CatfishG == 1 | T_CatfishH == 1 |
                              T_CatfishSalt == 1, 1, 0)) %>%
  mutate(T_Drum = ifelse(T_DrumSpp == 1 | T_DrumB == 1 | T_DrumP == 1 | T_DrumR == 1 |
                           T_Redfish == 1, 1, 0)) %>%
  mutate(T_RedDrum = ifelse(T_DrumR == 1 | T_Redfish == 1, 1, 0)) %>% 
  mutate(T_OtherDrum = ifelse(T_DrumSpp == 1 | T_DrumB == 1 | T_DrumP == 1, 1, 0)) %>% 
  mutate(T_Mullet = ifelse(T_MulletSpp == 1 | T_MulletG == 1 | T_MulletB == 1, 1, 0)) %>%
  mutate(T_Snap = ifelse(T_SnapSpp == 1 | T_SnapMang == 1 | T_SnapRed == 1, 1, 0)) %>%
  mutate(T_Trout = ifelse(T_TroutSpp == 1 | T_TroutSpec == 1 | T_TroutW == 1, 1, 0)) %>%
  mutate(across(c("C_AtlanticCroaker_K", "C_AtlanticCroaker_R", "C_Bass_K", "C_Bass_R", 
                  "C_BlueCrab_K", "C_BlueCrab_R", "C_Bream_K", "C_Bream_R", 
                  "C_CatfishSpp_K", "C_CatfishSpp_R", "C_CatfishFresh_K", "C_CatfishFresh_R", 
                  "C_CatfishG_K", "C_CatfishG_R", "C_CatfishH_K", "C_CatfishH_R", 
                  "C_CatfishSalt_K", "C_CatfishSalt_R", "C_Crappie_K", "C_Crappie_R", 
                  "C_DrumSpp_K", "C_DrumSpp_R", "C_DrumB_K", "C_DrumB_R", "C_DrumP_K", "C_DrumP_R", 
                  "C_DrumR_K", "C_DrumR_R", "C_Flounder_K", "C_Flounder_R", 
                  "C_Grouper_K", "C_Grouper_R", "C_JackCrev_K", "C_JackCrev_R", 
                  "C_Mackerel_K", "C_Mackerel_R", "C_MahiMahi_K", "C_MahiMahi_R", 
                  "C_MulletSpp_K", "C_MulletSpp_R", "C_MulletG_K", "C_MulletG_R", 
                  "C_MulletB_K", "C_MulletB_R", "C_Perch_K", "C_Perch_R", "C_Redfish_K", "C_Redfish_R", 
                  "C_Shark_K", "C_Shark_R", "C_Sheepshead_K", "C_Sheepshead_R", 
                  "C_SnapSpp_K", "C_SnapSpp_R", 
                  "C_SnapMang_K", "C_SnapMang_R", "C_SnapRed_K", "C_SnapRed_R", 
                  "C_Stingray_K", "C_Stingray_R", "C_TroutSpp_K", "C_TroutSpp_R", 
                  "C_TroutSpec_K", "C_TroutSpec_R", "C_TroutW_K", "C_TroutW_R", 
                  "C_Whiting_K", "C_Whiting_R"), 
                ~ ifelse(is.na(.) | . == " ", 0, .))) %>%
  mutate(C_AtlanticCroaker = rowSums(dplyr::select(., C_AtlanticCroaker_K, C_AtlanticCroaker_R), na.rm = TRUE)) %>%
  mutate(C_Bass = rowSums(dplyr::select(., C_Bass_K, C_Bass_R), na.rm = TRUE)) %>%
  mutate(C_BlueCrab = rowSums(dplyr::select(., C_BlueCrab_K, C_BlueCrab_R), na.rm = TRUE)) %>%
  mutate(C_Bream = rowSums(dplyr::select(., C_Bream_K, C_Bream_R), na.rm = TRUE)) %>%
  mutate(C_Catfish_K = rowSums(dplyr::select(., C_CatfishSpp_K, C_CatfishFresh_K, C_CatfishG_K, 
                                      C_CatfishH_K, C_CatfishSalt_K), na.rm = TRUE)) %>%
  mutate(C_Catfish_R = rowSums(dplyr::select(., C_CatfishSpp_R, C_CatfishFresh_R, C_CatfishG_R, 
                                      C_CatfishH_R, C_CatfishSalt_R), na.rm = TRUE)) %>%
  mutate(C_Catfish = rowSums(dplyr::select(., C_CatfishSpp_K, C_CatfishSpp_R, C_CatfishFresh_K, 
                                    C_CatfishFresh_R, C_CatfishG_K, C_CatfishG_R, C_CatfishH_K, 
                                    C_CatfishH_R, C_CatfishSalt_K, C_CatfishSalt_R), na.rm = TRUE)) %>%
  mutate(C_Crappie = rowSums(dplyr::select(., C_Crappie_K, C_Crappie_R), na.rm = TRUE)) %>%
  mutate(C_Drum_K = rowSums(dplyr::select(., C_DrumSpp_K, C_DrumB_K, C_DrumP_K, C_DrumR_K, 
                                   C_Redfish_K), na.rm = TRUE)) %>%
  mutate(C_Drum_R = rowSums(dplyr::select(., C_DrumSpp_R, C_DrumB_R, C_DrumP_R, C_DrumR_R, 
                                   C_Redfish_R), na.rm = TRUE)) %>%
  mutate(C_Drum = rowSums(dplyr::select(., C_DrumSpp_K, C_DrumB_K, C_DrumP_K, C_DrumR_K, 
                                 C_Redfish_K, C_DrumSpp_R, C_DrumB_R, C_DrumP_R, 
                                 C_DrumR_R, C_Redfish_R), na.rm = TRUE)) %>%
  mutate(C_Flounder = rowSums(dplyr::select(., C_Flounder_K, C_Flounder_R), na.rm = TRUE)) %>%
  mutate(C_Grouper = rowSums(dplyr::select(., C_Grouper_K, C_Grouper_R), na.rm = TRUE)) %>%
  mutate(C_JackCrev = rowSums(dplyr::select(., C_JackCrev_K, C_JackCrev_R), na.rm = TRUE)) %>%
  mutate(C_Mackerel = rowSums(dplyr::select(., C_Mackerel_K, C_Mackerel_R), na.rm = TRUE)) %>%
  mutate(C_MahiMahi = rowSums(dplyr::select(., C_MahiMahi_K, C_MahiMahi_R), na.rm = TRUE)) %>%
  mutate(C_Mullet_K = rowSums(dplyr::select(., C_MulletSpp_K, C_MulletG_K, C_MulletB_K), 
                              na.rm = TRUE)) %>%
  mutate(C_Mullet_R = rowSums(dplyr::select(., C_MulletSpp_R, C_MulletG_R, C_MulletB_R), 
                              na.rm = TRUE)) %>%
  mutate(C_Mullet = rowSums(dplyr::select(., C_MulletSpp_K, C_MulletG_K, C_MulletB_K, C_MulletSpp_R, 
                                   C_MulletG_R, C_MulletB_R), na.rm = TRUE)) %>%
  mutate(C_Perch = rowSums(dplyr::select(., C_Perch_K, C_Perch_R), na.rm = TRUE)) %>%
  mutate(C_Shark = rowSums(dplyr::select(., C_Shark_K, C_Shark_R), na.rm = TRUE)) %>%
  mutate(C_Sheepshead = rowSums(dplyr::select(., C_Sheepshead_K, C_Sheepshead_R), na.rm = TRUE)) %>%
  mutate(C_Snap_K = rowSums(dplyr::select(., C_SnapSpp_K, C_SnapMang_K, C_SnapRed_K), 
                            na.rm = TRUE)) %>%
  mutate(C_Snap_R = rowSums(dplyr::select(., C_SnapSpp_R, C_SnapMang_R, C_SnapRed_R), 
                            na.rm = TRUE)) %>%
  mutate(C_Snap = rowSums(dplyr::select(., C_SnapSpp_K, C_SnapMang_K, C_SnapRed_K, C_SnapSpp_R, 
                                 C_SnapMang_R, C_SnapRed_R), na.rm = TRUE)) %>%
  mutate(C_Stingray = rowSums(dplyr::select(., C_Stingray_K, C_Stingray_R), na.rm = TRUE)) %>%
  mutate(C_Trout_K = rowSums(dplyr::select(., C_TroutSpp_K, C_TroutSpec_K, C_TroutW_K), 
                             na.rm = TRUE)) %>%
  mutate(C_Trout_R = rowSums(dplyr::select(., C_TroutSpp_R, C_TroutSpec_R, C_TroutW_R), 
                             na.rm = TRUE)) %>%
  mutate(C_Trout = rowSums(dplyr::select(., C_TroutSpp_K, C_TroutSpec_K, C_TroutW_K, C_TroutSpp_R, 
                                  C_TroutSpec_R, C_TroutW_R), na.rm = TRUE)) %>%
  mutate(C_Whiting = rowSums(dplyr::select(., C_Whiting_K, C_Whiting_R), na.rm = TRUE)) %>%
  mutate(C_Total_K = rowSums(dplyr::select(., c("C_AtlanticCroaker_K", "C_Bass_K", "C_BlueCrab_K", "C_Bream_K", "C_Catfish_K",
                                         "C_Crappie_K", "C_Drum_K", "C_Flounder_K", "C_Grouper_K", "C_Whiting_K",
                                         "C_JackCrev_K", "C_Mackerel_K", "C_MahiMahi_K", "C_Mullet_K", "C_Perch_K",
                                         "C_Shark_K", "C_Sheepshead_K", "C_Snap_K", "C_Stingray_K", 
                                         "C_Trout_K")), na.rm = TRUE)) %>%
  mutate(CPUE_K = C_Total_K / TimeFished)



#____________________________________________________________________________________________________________________
# Dummy Variable: Subsistence Fishermen

df <- df %>%
  filter(!is.na(BroughtHomeFish) & !is.na(PercLocalCatch)) %>%
  mutate(Subsistence = if_else(BroughtHomeFish == "Yes" & SubsistenceMotivation == 1 & PercLocalCatch > 0, 1, 0))

#____________________________________________________________________________________________________________________
# Differences in fishing motivations for subsistence vs. non-subsistence fishermen

df <- df %>%
  mutate_at(vars("AccessLV", "AestheticsLV", "CommunityLV", "ConvenienceLV", "CultureLV",
                 "FishLV", "HappyLV", "HealthyLV", "HistoricLV", "InfrastructureLV", "ParkingLV", 
                 "PastLV", "SafetyLV", "SpiritualLV", "TherapeuticLV", "WildernessLV"), 
            ~ case_when(
              is.na(.) | . == " " ~ 0,
              TRUE ~ 1)) %>%
  mutate(
    totalMotivations = rowSums(
      across(c(AccessLV, AestheticsLV, CommunityLV, ConvenienceLV, CultureLV,
               FishLV, HappyLV, HealthyLV, HistoricLV, InfrastructureLV, ParkingLV, 
               PastLV, SafetyLV, SpiritualLV, TherapeuticLV, WildernessLV)),
      na.rm = TRUE
    )
  )

# Average number of motivations held by subsistence vs. non-subsistence fishermen
df %>%
  group_by(Subsistence) %>%
  summarise(avg_sum = mean(totalMotivations, na.rm = TRUE))

df %>%
  dplyr::select(Subsistence, SubsistenceMotivation, ends_with("LV")) %>%
  pivot_longer(
    cols = c(ends_with("LV"), SubsistenceMotivation),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  group_by(Subsistence, Variable) %>%
  summarise(prop_1 = mean(Value == 1, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Subsistence, values_from = prop_1, names_prefix = "SubFish_") %>%
  mutate(diff = SubFish_1 - SubFish_0) %>%
  arrange(desc(diff)) %>%
  mutate(Variable = factor(Variable, levels = Variable)) %>%
  ggplot(aes(x = Variable, y = diff, fill = diff > 0)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Motivation",
    y = "Difference in proportion of subsistence vs. non-subsistence fishermen reporting motivations"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato"),
                    guide = "none") +
  theme_minimal()

#____________________________________________________________________________________________________________________
# Re-code variables for who respondents are fishing with and how often

df <- df %>%
  mutate(across(c(FishAloneFreq, FishFamFreq, FishFriendsFreq), ~ case_when(
                    . == "Never" ~ 1,
                    . == "Rarely" ~ 2,
                    . == "Sometimes" ~ 3,
                    . == "Often" ~ 4,
                    . == "Always" ~ 5,
                    TRUE ~ NA )))

wilcox.test(FishAloneFreq ~ Subsistence, data = df)
wilcox.test(FishFamFreq ~ Subsistence, data = df)
wilcox.test(FishFriendsFreq ~ Subsistence, data = df)

#____________________________________________________________________________________________________________________
# PERMANOVA: Sharing catch with multiple social groups for subsistence vs. non-subsistence fishermen

df_share <- df %>%
  mutate(across(starts_with("ShareCatch"), 
                ~ if_else(is.na(.) | . == " ", 2, 1)))


dist_share <- vegdist(df_share[, c("ShareCatchHouse", "ShareCatchNonHouse", "ShareCatchFriends", 
                                           "ShareCatchNeighbors")], metric = "jaccard")

perm_share_sub <- adonis2(dist_share ~ Subsistence, data = df_share, by = "margin")
perm_share_sub

#____________________________________________________________________________________________________________________
# PERMANOVA: Target fish groups for subsistence vs. non-subsistence fishermen

df_fishtarget_sub <- df %>%
  filter(rowSums(dplyr::select(., 
                        "T_Anything", "T_AtlanticCroaker", "T_Bass", "T_BlueCrab", "T_Bream", "T_Catfish",
                        "T_Crappie", "T_RedDrum", "T_OtherDrum", "T_Flounder", "T_Grouper", "T_Mackerel", "T_MahiMahi", 
                        "T_Mullet", "T_Perch", "T_Shark", "T_Sheepshead", "T_Shrimp", "T_Snap", "T_Trout")) > 0)

fishtarget_sub <- df_fishtarget_sub %>%
  dplyr::select("T_Anything", "T_AtlanticCroaker", "T_Bass", "T_BlueCrab", "T_Bream", "T_Catfish",
         "T_Crappie", "T_RedDrum", "T_OtherDrum", "T_Flounder", "T_Grouper", "T_Mackerel", "T_MahiMahi", 
         "T_Mullet", "T_Perch", "T_Shark", "T_Sheepshead", "T_Shrimp", "T_Snap", "T_Trout") %>%
  mutate_all(~ ifelse(. > 0, 1, 0))

dist_fishtarget_sub <- vegdist(fishtarget_sub, metric = "jaccard")

perm_fishcatch_t_sub <- adonis2(dist_fishtarget_sub ~ Subsistence, data = df_fishtarget_sub, by = "margin")
perm_fishcatch_t_sub

disp <- betadisper(dist_fishtarget_sub, df_fishtarget_sub$Subsistence)

anova(disp)

#____________________________________________________________________________________________________________________
# Finding: subsistence fishermen catch and keep more fish, despite similar levels of fishing skill between subsistence and non-subsistence fishermen

# Removing an outlier
df_catch <- df %>%
  filter(CPUE_K < 4)

# Catch per unit effort for fish kept is significantly different for subsistence and non-subsistence fishermen
wilcox.test(CPUE_K ~ Subsistence, data = df_catch)

# Subsistence fishermen catch and keep more than twice as many fish as non-subsistence fishermen
df_catch %>%
  group_by(Subsistence) %>%
  summarise(mean_CPUE_K = mean(CPUE_K, na.rm = TRUE)) %>%
  mutate(mean_CPUE_K = mean_CPUE_K * 60)

# Fishing avidity is similar for subsistence and non-subsistence fishermen
wilcox.test(FishAvid ~ Subsistence, data = df)

# The number of years fished is similar for subsistence and non-subsistence fishermen
wilcox.test(YearsFishing ~ Subsistence, data = df)

# Checking that selling and trading do not differ across subsistence and non-subsistence fishermen
df_st <- df %>%
  mutate(SellTrade = case_when(
    SellTrade == "Never" ~ 1,
    SellTrade == "Rarely"  ~ 2,
    SellTrade == "Sometimes"  ~ 3,
    SellTrade == "Often" ~ 4,
    SellTrade == "Always" ~ 5,
    TRUE ~ NA))

wilcox.test(SellTrade ~ Subsistence, data = df_st)

#____________________________________________________________________________________________________________________
# Target Fish Groups for Subsistence vs. Non-Subsistence Fishermen (Fig. 2a)
df %>%
  group_by(Subsistence) %>%
  summarise(across(starts_with("T_"), ~ mean(. == 1), .names = "prop_{col}")) %>%
  pivot_longer(cols = starts_with("prop_T_"), 
               names_to = "Species", 
               values_to = "Proportion") %>%
  mutate(Species = gsub("prop_", "", Species)) %>%
  filter(Species %in% c("T_Anything", "T_AtlanticCroaker", "T_Bass", "T_BlueCrab",
                        "T_RedDrum", "T_Flounder", "T_OtherDrum", "T_Trout")) %>%
  mutate(Species = factor(Species, levels = c("T_Anything",  "T_BlueCrab", "T_RedDrum", "T_Trout",
                                              "T_Flounder", "T_OtherDrum", "T_AtlanticCroaker", "T_Bass")),
         Subsistence = factor(Subsistence, labels = c("Non-Subsistence", "Subsistence")),) %>%
  ggplot(aes(x = Proportion*100, y = Subsistence, fill = Species)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ Species, scales = "fixed", nrow = 2,
             labeller = labeller(Species = c("T_Anything" = "Anything", "T_BlueCrab" = "Blue Crab", "T_RedDrum" = "Red Drum", 
                                             "T_Trout" = "Trout spp.", "T_Flounder" = "Flounder spp.", "T_OtherDrum" = "Drum spp.\nexcl. Red Drum", 
                                             "T_AtlanticCroaker" = "Atlantic\nCroaker", "T_Bass" = "Bass spp."))) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_fill_brewer(palette = "RdYlBu", guide = guide_legend(reverse = FALSE)) +
  theme_bw(base_size = 14) +
  labs(x = "Proportion of Respondents With Fish Target",
       y = "")
#____________________________________________________________________________________________________________________
# Community Fish Sharing for Subsistence vs. Non-Subsistence Fishermen (Fig. 2b)

df %>%
  mutate_at(vars(starts_with("ShareCatch")), 
          ~ ifelse(is.na(.) | . == " ", 2, 1)) %>%
  dplyr::select(Subsistence, ShareCatchHouse, ShareCatchNonHouse, ShareCatchNeighbors, ShareCatchFriends) %>%
  group_by(Subsistence) %>%
  summarise(across(starts_with("ShareCatch"), ~ mean(. == 1), .names = "prop_{col}")) %>%
  pivot_longer(cols = starts_with("prop_ShareCatch"), 
               names_to = "ShareCatch", 
               values_to = "Proportion") %>%
  mutate(
    ShareCatch = gsub("prop_", "", ShareCatch),
    Subsistence = factor(Subsistence, levels = c(0, 1), labels = c("Non-Subsistence", "Subsistence")),
    ShareCatch = factor(ShareCatch, levels = c("ShareCatchNeighbors", "ShareCatchNonHouse", "ShareCatchFriends", "ShareCatchHouse"),
                        labels = c("Neighbors", "Non-Household\nFamily", "Friends", "Household"))) %>%
  ggplot(aes(x = Proportion*100, y = ShareCatch, color = Subsistence)) +
  geom_line(aes(group = ShareCatch), color = "black", size = 1) +
  geom_point(size = 5) +
  scale_x_continuous(
    limits = c(0, 100), 
    breaks = seq(0, 100, 20), 
    labels = scales::percent_format(scale = 1)
  ) +
  scale_color_manual(values = c("Non-Subsistence" = "#D1D1D1", "Subsistence" = "#156082")) +
  labs(
    x = "Proportion of Fishers Who Shared\nWith Community",
    y = "",
    color = "") +
  theme_bw(base_size = 14) +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),  
    legend.text = element_text(size = 16),  
    legend.position = "bottom"
  )
