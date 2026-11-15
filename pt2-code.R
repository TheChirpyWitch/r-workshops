# R & RStudio for Researchers: Workshop Pt. 2
# All the code from the slides, in order.
# Run a line with Ctrl+Enter (Windows) or Cmd+Enter (Mac).
# Exercise solutions are marked: try the exercise yourself first!



# ------------------------------------------------------------
# Start-up
# ------------------------------------------------------------
library(tidyverse)
library(palmerpenguins)
library(here)
library(broom)     # new today: tidy model results
library(emmeans)   # new today: group comparisons


# ------------------------------------------------------------
# A data frame is a table of vectors
# ------------------------------------------------------------
penguins$body_mass_g[1:6]

mean(penguins$body_mass_g, na.rm = TRUE)


# ------------------------------------------------------------
# filter(): keep rows
# ------------------------------------------------------------
penguins |>
  filter(species == "Gentoo", body_mass_g > 5800)


# ------------------------------------------------------------
# select() and arrange(): choose columns, sort rows
# ------------------------------------------------------------
penguins |>
  select(species, island, body_mass_g) |>
  arrange(desc(body_mass_g))       # desc() = largest first


# ------------------------------------------------------------
# mutate(): add a column
# ------------------------------------------------------------
penguins |>
  mutate(mass_kg = body_mass_g / 1000) |>
  select(species, body_mass_g, mass_kg)


# ------------------------------------------------------------
# group_by() + summarise(): the pivot table
# ------------------------------------------------------------
penguins |>
  group_by(species) |>
  summarise(n = n(),
            mean_g = mean(body_mass_g, na.rm = TRUE),
            sd_g = sd(body_mass_g, na.rm = TRUE))


# ------------------------------------------------------------
# Exercise: wrangle
# ------------------------------------------------------------
# Solution (try it yourself first!)
penguins |>
  filter(island == "Dream") |>
  mutate(bill_ratio =
    bill_length_mm / bill_depth_mm) |>
  group_by(species) |>
  summarise(mean_ratio =
    mean(bill_ratio, na.rm = TRUE)) |>
  arrange(desc(mean_ratio))


# ------------------------------------------------------------
# Reshape: wide to long
# ------------------------------------------------------------
scores <- tibble(id = 1:2, week1 = c(5.1, 4.8), week2 = c(5.6, 5.0))
scores

scores |>
  pivot_longer(c(week1, week2),
               names_to = "week",
               values_to = "score")


# ------------------------------------------------------------
# Cleaning messy column names
# ------------------------------------------------------------
names(penguins_raw)[10:13]

clean <- janitor::clean_names(penguins_raw)
names(clean)[10:13]


# ------------------------------------------------------------
# Build it up: 1. data and axes
# ------------------------------------------------------------
ggplot(penguins,
       aes(x = flipper_length_mm,
           y = body_mass_g))


# ------------------------------------------------------------
# Build it up: 2. add points
# ------------------------------------------------------------
ggplot(penguins,
       aes(x = flipper_length_mm,
           y = body_mass_g)) +
  geom_point()


# ------------------------------------------------------------
# Build it up: 3. colour and labels
# ------------------------------------------------------------
ggplot(penguins,
       aes(x = flipper_length_mm,
           y = body_mass_g,
           colour = species)) +
  geom_point(alpha = 0.7) +
  labs(x = "Flipper length (mm)",
       y = "Body mass (g)",
       colour = NULL) +
  theme(legend.position = "top")


# ------------------------------------------------------------
# Show the data, not just the average
# ------------------------------------------------------------
ggplot(penguins,
       aes(x = species,
           y = body_mass_g)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15,
              alpha = 0.4) +
  labs(x = NULL,
       y = "Body mass (g)")


# ------------------------------------------------------------
# Small multiples: one panel per group
# ------------------------------------------------------------
ggplot(penguins,
       aes(x = flipper_length_mm,
           y = body_mass_g,
           colour = species)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ island) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1))


# ------------------------------------------------------------
# Save figures from code
# ------------------------------------------------------------
dir.create(here("figures"), showWarnings = FALSE)   # make the folder once

my_plot <- ggplot(penguins, aes(flipper_length_mm, body_mass_g)) +
  geom_point()

ggsave(here("figures", "fig1.pdf"), my_plot,
       width = 85, height = 70, units = "mm")       # vector: sharp at any size

ggsave(here("figures", "fig1.png"), my_plot,
       width = 85, height = 70, units = "mm", dpi = 600)


# ------------------------------------------------------------
# Exercise: plot
# ------------------------------------------------------------
# Solution (try it yourself first!)
penguins |>
  filter(!is.na(sex)) |>
  ggplot(aes(x = species,
             y = bill_depth_mm)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.4) +
  facet_wrap(~ sex) +
  labs(x = NULL, y = "Bill depth (mm)")


# ------------------------------------------------------------
# t-test: do male and female Adelie differ in mass?
# ------------------------------------------------------------
adelie <- penguins |> filter(species == "Adelie", !is.na(sex))

t.test(body_mass_g ~ sex, data = adelie)


# ------------------------------------------------------------
# ANOVA: three or more groups
# ------------------------------------------------------------
fit_aov <- aov(body_mass_g ~ species, data = penguins)
summary(fit_aov)


# ------------------------------------------------------------
# Which groups differ?
# ------------------------------------------------------------
emmeans(fit_aov, pairwise ~ species)$contrasts


# ------------------------------------------------------------
# Linear regression
# ------------------------------------------------------------
fit_lm <- lm(body_mass_g ~ flipper_length_mm + species, data = penguins)

tidy(fit_lm, conf.int = TRUE) |>
  select(term, estimate, conf.low, conf.high, p.value)


# ------------------------------------------------------------
# Check the assumptions
# ------------------------------------------------------------
par(mfrow = c(1, 2))
plot(fit_lm, which = 1:2)


# ------------------------------------------------------------
# Exercise: statistics
# ------------------------------------------------------------
# Solution (try it yourself first!)
gentoo <- penguins |>
  filter(species == "Gentoo", !is.na(sex))

t.test(flipper_length_mm ~ sex, data = gentoo)
