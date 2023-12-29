#Code for figure 3A

#Load libraries
library(tidyverse)
library(DescTools)
library(broom)
library(ggthemes)

# specify colorblind palette
cols <- colorblind_pal()(8)

anova_function <- function(s){
  aov(value~sample, data = data_wrangled |> filter(readout == s)) |>
    tidy() |>
    mutate(readout = s) |>
    mutate(significance = if_else(p.value <0.0001, true = "****",
                                  false = if_else(p.value < 0.001, true = "***",
                                                  false = if_else(p.value < 0.01, true = "**",
                                                                  false = if_else(p.value < 0.05, true = "*", 
                                                                                  false = "ns")))))
}

dunnett_function <- function(s){
  d <- DunnettTest(value~sample, data = data_wrangled |> filter(readout == s),
                   control = "PBS")
  d[["PBS"]] |>
    as_tibble(rownames = "comparison") |>
    mutate(readout = s) |>
    mutate(significance = if_else(pval <0.0001, true = "****",
                                  false = if_else(pval < 0.001, true = "***",
                                                  false = if_else(pval < 0.01, true = "**",
                                                                  false = if_else(pval < 0.05, true = "*", 
                                                                                  false = "ns"))))) 
}

#read raw data
raw_data <- read_csv(file = "3A.csv", col_select = -"...2") |>
  rename(sample = "...1")

#Data wrangled
data_wrangled <- raw_data |>
  pivot_longer(cols = -sample, names_to = "readout") |>
  drop_na() |>
  mutate(readout = str_remove(readout, "...[0-9]+$")) |>
  mutate(readout = str_replace(readout, "mRNA", "FXI mRNA"))

#ANOVA test
readout_unique <- data_wrangled |>
  select(readout) |>
  unique()

anova_data <- readout_unique$readout |>
  map(anova_function) |>
  bind_rows()
#Write ANOVA data file
write_csv(x = anova_data, file = "3A_anova.csv")

#Dunnett multiple comparison test
dunnett_data <- readout_unique$readout |>
  map(dunnett_function) |>
  bind_rows()
write_csv(x = dunnett_data, file = "3A_dunnett.csv")

significance_labels <- dunnett_data |>
  select(comparison, readout, significance) |>
  mutate(x = as.double(NA), y = as.double(NA)) |>
  rows_update(tibble(readout = c("APTT", "FXI Activity", "FXI mRNA"),
                     y = c(160, 150, 140))) |>
  rows_update(tibble(comparison = c("1 mg/kg-PBS", "3 mg/kg-PBS", "6 mg/kg-PBS", "9 mg/kg-PBS"),
                     x = c(2, 3, 4, 5))) |>
  filter(significance != "ns")
 
#create boxplot
data_plot <- data_wrangled |>
  mutate(value = if_else(readout == "APTT", true = value * 93.75, false = value))

data_plot$sample <- factor(x = data_plot$sample,
                           levels = c("PBS", "1 mg/kg", "3 mg/kg", "6 mg/kg", "9 mg/kg"))

p <- data_plot |>
  ggplot(aes(x = sample, y = value, fill = readout)) +
  geom_boxplot(width = 0.5, linewidth = 0.15, outlier.size = 0.2) +
  theme_classic() +
  labs(x = NULL, y = "FXI Activity/\n FXI mRNA\n Normalized to PBS (%)") +
  scale_y_continuous(sec.axis = sec_axis(~./93.75, name = "APTT ratio\n")) +
  theme(text = element_text(size = 8),
        title = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank()) +
  scale_fill_manual(values = cols[c(2, 3, 4, 6, 7, 8)]) +
  scale_color_manual(values = cols[c(2, 3, 4, 6, 7, 8)]) +
  geom_text(data = significance_labels,
            aes(x = x, y = y,label = significance,color = readout),
            show.legend = F) #map significance labels

#Save the plot
ggsave("3A_plot.pdf", plot = p, scale = 1, width = 15, height = 6, units = "cm",
       dpi = 300)



#--------End of code-------------------------------------------------------------



