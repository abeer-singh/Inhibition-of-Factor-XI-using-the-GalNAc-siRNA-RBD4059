# *Code for figure 4B and 4C for 25min*

#Load libraries
library(tidyverse)
library(DescTools)
library(broom)
library(ggthemes)
library(ggsignif)
library(xlsx)

#functions
dunnett_test_function <- function(time_number = 10, c){
  data_timecourse_filtered_dunnett <- raw_data_timecourse |>
    filter(time == time_number)
  dunnett_analysis <- DunnettTest(blood_flow_velocity ~ sample,
                                  data = data_timecourse_filtered_dunnett,
                                  control = c, conf.level = 0.95)
  dunnett_analysis[[c]] |>
    as_tibble(rownames = "comparison") |>
    mutate(day = time_number, .after = comparison)
}

# specify colorblind palette
cols <- colorblind_pal()(8)

#Read the data
raw_data <- read_csv(file = "4B (25 min).csv", col_select = `Time（min）` : "RBD4059 9 mg/kg...51") |>
  rename (time = `Time（min）`)

#***4c  
#calculate area under curve (auc)
auc_data <- raw_data |> select(-time) |>
  map(.f = ~AUC(x = raw_data$time, y = .x, method = "trapezoid", na.rm = T)) |>
  as_tibble() |>
  pivot_longer(cols = everything(), values_to = "auc", names_to = "sample") |>
  mutate(sample = str_remove(sample,"...[0-9]+$")) |>
  mutate(sample = str_remove(sample, " 4 mg/kg"))

#Statistics: Kruskal-Wallis test for non-parametric alternative to one-way ANOVA
# Assumption: the sample data does not come from a normal distribution
kruskal_test <- kruskal.test(auc~sample, data = auc_data) |>
  tidy() |>
  mutate(significance = if_else(p.value <0.0001, true = "****",
                                false = if_else(p.value < 0.001, true = "***",
                                false = if_else(p.value < 0.01, true = "**",
                                false = if_else(p.value < 0.05, true = "*", 
                                                false = "ns")))))

#Write kruskal test data as .csv file
write_csv(x = kruskal_test, file = "4c_25_kruskal_test.csv")

#Since p-value from Kruskal-Wallis test is <0.05, it means atleast one pairwaise
# group comparison should be positive. For pairwaise comparison we do Dunn's multiple
# comparison test
dunn_test <- DunnTest(auc~sample, data = auc_data, method = "holm", alternative = "two.sided")
dunn_test_data <- dunn_test[[1]] |>
  as_tibble(rownames = "comparison") |>
  mutate(significance = if_else(pval <0.0001, true = "****",
                                false = if_else(pval < 0.001, true = "***",
                                                false = if_else(pval < 0.01, true = "**",
                                                                false = if_else(pval < 0.05, true = "*", 
                                                                                false = "ns")))))
significance_levels <- dunn_test_data |>
  filter(significance != "ns") |>
  separate_wider_delim(cols = comparison, names = c("group1", "group2"), delim = "-") |>
  mutate(x_min = as.double(NA), x_max = as.double(NA), y = as.double(NA)) |>
  rows_update(tibble(group1 = c("RBD4059 3 mg/kg", "RBD4059 9 mg/kg"),
                     group2 = c("PBS", "PBS"),
                     x_min = c(1, 1), x_max = c(3, 4),
                     y = c(40, 45)))
#Write Dunn test data as .csv file
write_csv(x = dunn_test_data, file = "4c_25_dunn_test.csv")

#Change factor levels of "auc_data" for plotting preference
auc_data$sample <- factor(auc_data$sample, levels = c("PBS", "RBD4059 1 mg/kg",
                                                      "RBD4059 3 mg/kg",
                                                      "RBD4059 9 mg/kg",
                                                      "Enoxaparine"))

#Plot the auc graph
plot_auc <- auc_data |>
  ggplot(aes(x = sample, y = auc)) +
  geom_boxplot(aes(fill = sample), width = 0.5, linewidth = 0.15,
               outlier.size = 0.2, show.legend = F) +
  theme_classic() +
  labs(x = NULL, y = "AUC\nNormalized to baseline") +
  theme(text = element_text(size = 8),
        title = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        legend.title = element_blank()) +
  scale_fill_manual(values = cols[c(2, 3, 4, 6, 7, 8)]) +
  geom_signif(y_position = significance_levels$y, xmin = significance_levels$x_min,
              xmax = significance_levels$x_max,
              annotations = significance_levels$significance,
              textsize = 2.8, size = 0.3) +
  scale_y_continuous(limits = c(0, 50), breaks = c(0, 10, 20, 30, 40, 50))

#Save the auc plot
ggsave("4c_plot_auc.pdf", plot = plot_auc, scale = 1, width = 6,
       height = 6, units = "cm", dpi = 300)
#***4b
#data wrangling for time course analysis
raw_data_timecourse <- raw_data |>
  pivot_longer(cols = -time, names_to = "sample", values_to = "blood_flow_velocity") |>
  mutate(sample = str_remove(sample, "...[0-9]+$")) |>
  mutate(sample = str_remove(sample, " 4 mg/kg"))

data_timecourse_plotting <- raw_data_timecourse |>
  group_by(time, sample) |>
  summarise(mean = mean(blood_flow_velocity), sd = sd(blood_flow_velocity)) |>
  mutate(upper = mean + sd,
         lower = mean - sd)

#Change factor levels of sample for timecourse plotting preference
data_timecourse_plotting$sample <- factor(data_timecourse_plotting$sample, levels = c("PBS", "RBD4059 1 mg/kg",
                                                      "RBD4059 3 mg/kg",
                                                      "RBD4059 9 mg/kg",
                                                      "Enoxaparine"))

#Plot timecourse data
plot_timecourse <- data_timecourse_plotting |>
  ggplot(aes(x = time, y = mean, color = sample)) +
  geom_line(aes(group = sample), linewidth = 0.5) +
  geom_errorbar(aes(ymin = mean, ymax = upper), width = 0.2, linewidth = 0.3, alpha = 0.3) +
  geom_point(aes(shape = sample, fill = sample), color = "black", size = 1, stroke = 0.3) +
  theme_classic() +
  labs(x = "Time (min)", y = "Blood flow velocity\nNormalized to baseline") +
  theme(text = element_text(size = 8),
        title = element_text(size = 8),
        legend.title = element_blank(),
        legend.box.spacing = unit(0.2, "cm")) +
  scale_color_manual(values = cols[c(2, 3, 4, 6, 7, 8)]) +
  scale_fill_manual(values = cols[c(2, 3, 4, 6, 7, 8)]) +
  scale_shape_manual(values = 21:25)

#Save the timecourse plot
ggsave("4b_25_plot.pdf", plot = plot_timecourse, scale = 1, width = 15,
       height = 6, units = "cm", dpi = 300)

#Two-way anova
raw_data_timecourse_anova <- raw_data_timecourse |>
  mutate(time = as.character(time))
anova_timecourse <- aov(blood_flow_velocity~sample * time, data = raw_data_timecourse_anova) |>
  tidy() |>
  mutate(significance = if_else(p.value <0.0001, true = "****",
                                false = if_else(p.value < 0.001, true = "***",
                                                false = if_else(p.value < 0.01, true = "**",
                                                                false = if_else(p.value < 0.05, true = "*", 
                                                                                false = "ns")))))

#write down anova test as .csv file
write_csv(x = anova_timecourse, file = "4b_25_anova_timecourse.csv")

#Since the two-way anova test showed a p-value <0.05 for the interaction model, 
#we will perform Dunnett multiple comparison test for post-hoc analysis. control = PBS
time_levels <- raw_data_timecourse$time |>
  unique()

#dunnett post-hoc analysis, control = "PBS"
dunnett_timecourse_data_pbs <- map(time_levels[time_levels != 1],
                               ~dunnett_test_function (time_number = .x, c = "PBS")) |>
  list_rbind() |>
  mutate(significance = if_else(pval <0.0001, true = "****",
                                false = if_else(pval < 0.001, true = "***",
                                                false = if_else(pval < 0.01, true = "**",
                                                                false = if_else(pval < 0.05, true = "*", 
                                                                                false = "ns")))))
#dunnett post-hoc analysis, control = "Enoxaparine"
dunnett_timecourse_data_enoxaparine <- map(time_levels[time_levels != 1],
                                   ~dunnett_test_function (time_number = .x, c = "Enoxaparine")) |>
  list_rbind() |>
  mutate(significance = if_else(pval <0.0001, true = "****",
                                false = if_else(pval < 0.001, true = "***",
                                                false = if_else(pval < 0.01, true = "**",
                                                                false = if_else(pval < 0.05, true = "*", 
                                                                                false = "ns")))))

#write dunnett timecourse data analysis 
#write_csv(x = dunnett_timecourse_data, file = "4b_25_dunnett_timecourse.csv")
write.xlsx(x = dunnett_timecourse_data_pbs, file = "4b_dunnett.xlsx",
           sheetName = "control = PBS")
write.xlsx(x = dunnett_timecourse_data_enoxaparine, file = "4b_dunnett.xlsx",
           sheetName = "control = Enoxaparine", append = T)

#-----End of code---------------------------------------------------------------
