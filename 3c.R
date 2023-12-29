# code for figure 3C

#Load libraries
library(tidyverse)
library(DescTools)
library(broom)
library(ggthemes)

# specify colorblind palette
cols <- colorblind_pal()(8)

#read raw data
raw_data <- read_csv(file = "3C.csv", col_select = -"...2") |>
  rename(time = "...1")

#Data wrangled
data_wrangled <- raw_data |>
  pivot_longer(cols = -time, names_to = "readout") |>
  drop_na() |>
  mutate(readout = str_remove(readout, "...[0-9]+$"))

data_plot <- data_wrangled |>
  mutate(value = if_else(readout == "RBD4059_AS Conc.", true = value / 666.67,
                         false = value)) |>
  group_by(time, readout) |>
  summarise(mean = mean(value, na.rm = T), sd = sd(value, na.rm = T)) |>
  mutate(upper = mean + sd, lower = mean - sd)
  
#plot data 
p <- data_plot |>
  mutate(readout = str_replace(readout, "RBD4059_AS Conc.", "Antisense\nconcentration\nin liver")) |>
  ggplot(aes(x = time, y = mean, color = readout)) +
  geom_line(linewidth = 0.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 1.5, linewidth = 0.3) +
  geom_point(aes(shape = readout, fill = readout), color = "black", size = 1,
             stroke = 0.3) +
  scale_y_continuous(limits = c(0, 150), breaks = c(50, 100, 150),
                     sec.axis = sec_axis(~.*666.67,
                                         name = "Liver concentration\n(ng/g)\n")) +
  theme_classic() +
  labs(x = "Time (Days)", y = "FXI activity (%)\nNormalized to PBS %") +
  theme(text = element_text(size = 8),
        title = element_text(size = 8),
        legend.title = element_blank(),
        legend.box.spacing = unit(0.2, "cm")) +
  scale_color_manual(values = cols[c(2, 3, 4, 6, 7, 8)]) +
  scale_fill_manual(values = cols[c(2, 3, 4, 6, 7, 8)]) +
  scale_shape_manual(values = c(21, 22)) +
  scale_x_continuous(breaks = c(1, 2, 8, 15, 29, 43, 64, 85),
                     guide = guide_axis(n.dodge = 2))

#save plot
ggsave(filename = "3C.pdf", plot = p, scale = 1, width = 12,
       height = 6, units = "cm", dpi = 300)

