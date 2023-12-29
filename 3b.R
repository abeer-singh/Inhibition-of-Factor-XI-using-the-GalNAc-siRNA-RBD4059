#Figure 3B

#Load libraries
library(tidyverse)
library(DescTools)
library(broom)
library(ggthemes)

# specify colorblind palette
cols <- colorblind_pal()(8)

#Functions
anova_function <- function(s){
  aov(value~time, data = data_wrangled_anova |> filter(sample == s)) |>
    tidy() |>
    mutate(sample = s) |>
    mutate(significance = if_else(p.value <0.0001, true = "****",
                                  false = if_else(p.value < 0.001, true = "***",
                                                  false = if_else(p.value < 0.01, true = "**",
                                                                  false = if_else(p.value < 0.05, true = "*", 
                                                                                  false = "ns")))))
}

dunnett_test_function <- function(s){
  dunnett_test <- DunnettTest(value~time, data = data_wrangled |> filter(sample == s), control = "1")
  dunnett_test[["1"]] |>
    as_tibble(rownames = "comparison") |>
    mutate(sample = s) |>
    mutate(significance = if_else(pval <0.0001, true = "****",
                                  false = if_else(pval < 0.001, true = "***",
                                                  false = if_else(pval < 0.01, true = "**",
                                                                  false = if_else(pval < 0.05, true = "*", 
                                                                                  false = "ns")))))
}

#Read the data
raw_data <- read_csv(file = "3B.csv", col_select = ends_with(c("...1", "...3", "...4","...5",
                                                   "...6","...7","...8","...15",
                                                   "...16","...17","...18","...19",
                                                   "...20","...27","...28","...29",
                                                   "...30","...31","...32"))) |>
  rename("time" = "...1")

#Data wrangling
data_wrangled <- raw_data |>
  pivot_longer(cols = -time, names_to = "sample") |>
  mutate(sample = str_remove(sample, "...[0-9]+$"),
         sample = str_to_lower(sample),
         sample = str_replace(sample, " ", "_"))

#Perform one-way anova on FXI mrna, FXI activity and APTT
sample_unique <- data_wrangled |>
  select(sample) |>
  unique()

data_wrangled_anova <- data_wrangled |>
  mutate(time = as.character(time))

anova_results <- sample_unique$sample |>
  map(anova_function) |>
  bind_rows()
write_csv(x = anova_results, file = "3b_anova.csv")

#Perform Dunnett multiple comparison test for each group
dunnett_results <- sample_unique$sample |>
  map(dunnett_test_function) |>
  bind_rows()
write_csv(x = dunnett_results, file = "3b_dunnett.csv")

significance_labels <- dunnett_results |>
  separate_wider_delim(cols = comparison, names = c("x", NA), delim = "-") |>
  mutate(x = as.double(x)) |>
  mutate(y = as.double(NA), .after = x) |>
  rows_update(tibble(sample = c("fxi_activity", "aptt", "mrna"), y = c(140, 150, 130))) |>
  mutate(sample = str_replace(sample, "fxi_activity", "FXI Activity"),
         sample = str_replace(sample, "aptt", "APTT"),
         sample = str_replace(sample, "mrna", "FXI mRNA")) |>
  filter(significance != "ns")

# plot the timecourse graph
data_plot <- data_wrangled |>
  mutate(value = if_else(sample == "aptt", true = value * 60, false = value)) |>
  group_by(time, sample) |>
  summarise(mean = mean(value, na.rm = T), sd = sd(value, na.rm = T)) |>
  mutate(upper = mean + sd, lower = mean - sd,
         sample = str_replace(sample, "aptt", "APTT"),
         sample = str_replace(sample, "fxi_activity", "FXI Activity"),
         sample = str_replace(sample, "mrna", "FXI mRNA"))

#plot the data
plot_timecourse <- data_plot |>
  ggplot(aes(x = time, y = mean, color = sample)) +
  geom_line(linewidth = 0.5) +
  geom_errorbar(aes(ymin = mean, ymax = upper), width = 1, linewidth = 0.3) +
  geom_point(aes(shape = sample, fill = sample), color = "black", size = 1, stroke = 0.3) +
  scale_y_continuous(limits = c(0, 150), breaks = c(50, 100, 150),
                                                sec.axis = sec_axis(~./60,
                                                                    name = "APTT ratio\n")) +
  theme_classic() +
  labs(x = "Time (Days)", y = "FXI activity/\nFXI mRNA\nNormalized to PBS %") +
  theme(text = element_text(size = 8),
        title = element_text(size = 8),
        legend.title = element_blank(),
        legend.box.spacing = unit(0.2, "cm")) +
  scale_color_manual(values = cols[c(2, 3, 4, 6, 7, 8)]) +
  scale_fill_manual(values = cols[c(2, 3, 4, 6, 7, 8)]) +
  scale_shape_manual(values = 21:25) +
  scale_x_continuous(breaks = c(1, 2, 8, 15, 29, 43, 64, 85),
                     guide = guide_axis(n.dodge = 2)) +
  geom_text(data = significance_labels,
            mapping = aes(x = x, y = y,label = significance, color = sample),
            show.legend = F) #map significance labels

ggsave("3b_plot.pdf", plot = plot_timecourse, scale = 1, width = 15,
       height = 6, units = "cm", dpi = 300) 

#---------End of code-----------------------------------------------------------


