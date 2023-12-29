#Figure 3D

#Load libraries
library(tidyverse)
library(DescTools)
library(broom)
library(ggthemes)
library(ggsignif)

# specify colorblind palette
cols <- colorblind_pal()(8)

#anova time-course function
anova_timecourse_function <- function(d){
  aov(value~time, data = data_wrangled |>
        filter(dosage == d) |>
        mutate(time = as.character(time))) |>
    tidy() |>
    mutate(significance = if_else(p.value <0.0001, true = "****",
                                  false = if_else(p.value < 0.001, true = "***",
                                                  false = if_else(p.value < 0.01, true = "**",
                                                                  false = if_else(p.value < 0.05, true = "*", 
                                                                                  false = "ns"))))) |>
    mutate(dosage = d, .before = term)  
}

#Dunnett timecourse function
dunnett_timecourse_function <- function(d){
  dunnett_test <- DunnettTest(value~time, data = data_wrangled |>
                                filter(dosage == d), control = "1")
  
  dunnett_test[["1"]] |>
    as_tibble(rownames = "comparison") |>
    mutate(significance = if_else(pval <0.0001, true = "****",
                                  false = if_else(pval < 0.001, true = "***",
                                                  false = if_else(pval < 0.01, true = "**",
                                                                  false = if_else(pval < 0.05, true = "*", 
                                                                                  false = "ns"))))) |>
    mutate(dosage = d, .before = comparison)
}

#read raw data
raw_data <- read_csv(file = "3D.csv", col_select = -ends_with("...2")) |>
  rename("time" = "...1")

#Data wrangling
data_wrangled <- raw_data |>
  pivot_longer(cols = -time, names_to = "dosage") |>
  mutate(dosage = str_remove(dosage, "...[0-9]+$"))

# plot the timecourse graph
data_plot <- data_wrangled |>
  drop_na() |>
  group_by(time, dosage) |>
  summarise(mean = mean(value, na.rm = T), sd = sd(value, na.rm = T)) |>
  mutate(upper = mean + sd, lower = mean - sd)

#Perform one-way anova on the data for each dosage
data_anova <- data_wrangled$dosage |>
  unique() |>
  map(anova_timecourse_function) |>
  list_rbind()

#write anova analysis file
write_csv(x = data_anova, file = "3d_anova.csv")

#Dunnett post-hoc analyses
data_dunnett <- data_wrangled$dosage |>
  unique() |>
  map(dunnett_timecourse_function) |>
  list_rbind()

write_csv(x = data_dunnett, file = "3d_dunnett.csv")

data_dunnett_plot <- data_dunnett |>
  separate_wider_delim(cols = comparison, delim = "-", names = c("x1", "x2")) |>
  mutate(x1 = as.double(x1), x2 = as.double(x2)) |>
  filter(significance != "ns") |>
  mutate(y = if_else(dosage == "RBD4059-1 mg/kg", true = 160,
                     false = if_else(dosage == "RBD4059-3 mg/kg", true = 150, false = 140))) |>
  mutate(x1 = if_else(x1 == 8, true = 6.8, false = x1))

#plot the data
plot_timecourse <-  data_plot |>
  ggplot(aes(x = time, y = mean, color = dosage)) +
  geom_line(linewidth = 0.5) +
  geom_errorbar(aes(ymin = mean, ymax = upper), width = 1.5, linewidth = 0.3) +
  geom_point(aes(shape = dosage, fill = dosage), color = "black", size = 1, stroke = 0.3) +
  scale_y_continuous(limits = c(0, 200), breaks = c(50, 100, 150, 200)) +
  theme_classic() +
  labs(x = "Time (Days)", y = "FXI activity (%)\n(Normalized to pre-dose)") +
  theme(text = element_text(size = 8),
        title = element_text(size = 8),
        legend.title = element_blank(),
        legend.box.spacing = unit(0.2, "cm")) +
  scale_color_manual(values = cols[c(2, 3, 4, 6, 7, 8)]) +
  scale_fill_manual(values = cols[c(2, 3, 4, 6, 7, 8)]) +
  scale_shape_manual(values = 21:25) +
  scale_x_continuous(breaks = c(1, 2, 8, 15, 29, 43, 57, 71, 85, 113),
                     guide = guide_axis(n.dodge = 2)) +
  geom_text(data = data_dunnett_plot,
            mapping = aes(x = x1, y = y, label = significance), show.legend = F)

#save the plot
ggsave("3d_plot_timecourse.pdf", plot = plot_timecourse, scale = 1, width = 15,
       height = 6, units = "cm", dpi = 300)

#calculate area under the curve
#data_auc <- 
data_auc <- raw_data |>
  filter(time != 113) |> #removed day 113 data points
  select_if(~ !any(is.na(.))) #removes columns with any NA values (removed monkey #4501 that died)
  
auc <- data_auc |>
  select(-time) |>
  map(.f = ~AUC(x = data_auc$time, y = .x, method = "trapezoid", na.rm = T)) |>
  as_tibble() |>
  pivot_longer(cols = everything(), values_to = "auc_value", names_to = "sample") |>
  separate_wider_delim(cols = sample, delim = "...", names = c("dosage", "animal_number"))

#Statistics: Kruskal-Wallis test for non-parametric alternative to one-way ANOVA
# Assumption: the sample data does not come from a normal distribution
#kruskal_test_auc <- 
kruskal_test_auc <-  auc |>
  kruskal.test(formula = auc_value ~ dosage, data = auc) |>
  tidy() |>
  mutate(significance = if_else(p.value <0.0001, true = "****",
                                false = if_else(p.value < 0.001, true = "***",
                                                false = if_else(p.value < 0.01, true = "**",
                                                                false = if_else(p.value < 0.05, true = "*", 
                                                                                false = "ns")))))

write_csv(x = kruskal_test_auc, file = "3d_kruskal_auc.csv")

#Since p-value from Kruskal-Wallis test is <0.05, it means atleast one pairwaise
# group comparison should be positive. For pairwaise comparison we do Dunn's multiple
# comparison test

dunn_test <- DunnTest(auc_value ~ dosage, data = auc, method = "holm", alternative = "two.sided")
dunn_test_data <- dunn_test[[1]] |>
  as_tibble(rownames = "comparison") |>
  mutate(significance = if_else(pval <0.0001, true = "****",
                                false = if_else(pval < 0.001, true = "***",
                                                false = if_else(pval < 0.01, true = "**",
                                                                false = if_else(pval < 0.05, true = "*", 
                                                                                false = "ns")))))

write_csv(x = dunn_test_data, file = "3d_dunn_auc.csv")

auc_significances <- dunn_test_data |>
  select(significance)

#plot AUC graph and put significance levels
plot_auc <- auc |>
  ggplot(aes(x = dosage, y = auc_value)) +
  geom_boxplot(aes(fill = dosage), width = 0.5, linewidth = 0.15, outlier.size = 0.05) +
  theme_classic() +
  labs(x = NULL, y = "AUC\nNormalized to baseline") +
  theme(text = element_text(size = 8),
        title = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        legend.position = "none") +
  scale_fill_manual(values = cols[c(2, 3, 4, 6, 7, 8)]) +
  geom_signif(
    y_position = c(7000, 7800, 6200), xmin = c(1, 1, 2), xmax = c(2, 3, 3),
    annotations = auc_significances$significance, textsize = 2.8, size = 0.3) +
  scale_y_continuous(limits = c(2000, 8000), breaks = c(2000, 4000, 6000, 8000))

#Save the auc plot
ggsave("3d_plot_auc.pdf", plot = plot_auc, scale = 1, width = 5,
       height = 6, units = "cm", dpi = 300)
#------------end of code---------------------------------------------------------


