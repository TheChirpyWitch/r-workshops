# R & RStudio Basics: Workshop Pt. 1
# All the code from the slides, in order.
# Run a line with Ctrl+Enter (Windows) or Cmd+Enter (Mac).
# Exercise solutions are marked: try the exercise yourself first!

# ------------------------------------------------------------
# What is R?
# ------------------------------------------------------------
mean(c(12, 15, 9))   # "give me the average of 12, 15 and 9"


# ------------------------------------------------------------
# Install the packages
# ------------------------------------------------------------
install.packages(c("tidyverse", "palmerpenguins", "here",
                   "broom", "car", "emmeans", "janitor", "Hmisc",
                   "patchwork", "quarto", "renv"))


# ------------------------------------------------------------
# here(): paths from the project folder
# ------------------------------------------------------------
library(here)
here("data", "raw", "penguins_raw.csv")


# ------------------------------------------------------------
# Objects and assignment
# ------------------------------------------------------------
mass_g <- 3750
species <- "Adelie"
mass_kg <- mass_g / 1000
mass_kg


# ------------------------------------------------------------
# Vectors: R's basic unit
# ------------------------------------------------------------
masses <- c(3750, 3800, 3250, NA, 3450)
masses / 1000
masses[2]               # the 2nd value: counting starts at 1
masses[c(1, 3)]         # the 1st and 3rd values


# ------------------------------------------------------------
# Factors: categories with an order
# ------------------------------------------------------------
dose <- factor(c("high", "low", "medium", "low"))
levels(dose)            # alphabetical by default

dose <- factor(dose, levels = c("low", "medium", "high"))
levels(dose)            # now in a meaningful order


# ------------------------------------------------------------
# Missing values: NA
# ------------------------------------------------------------
mean(masses)
mean(masses, na.rm = TRUE)
sum(is.na(masses))


# ------------------------------------------------------------
# Conditions: yes-or-no questions
# ------------------------------------------------------------
temps <- c(18, 25, 31, 22, 29)   # daily highs, °C
temps > 24


# ------------------------------------------------------------
# Using conditions
# ------------------------------------------------------------
temps[temps > 24]                # keep the values where the answer is TRUE
sum(temps > 24)                  # TRUE counts as 1: how many warm days?
mean(temps > 24)                 # share of days that were warm

masses > 3500
sum(masses > 3500, na.rm = TRUE)


# ------------------------------------------------------------
# Functions and packages
# ------------------------------------------------------------
round(3.14159, digits = 2)

# install.packages("palmerpenguins")   # once per computer (already done above)
library(palmerpenguins)              # once per session


# ------------------------------------------------------------
# Getting help
# ------------------------------------------------------------
?mean


# ------------------------------------------------------------
# Exercise: vectors
# ------------------------------------------------------------
# Solution (try it yourself first!)
bills <- c(39.1, 39.5, NA, 46.5, 50.0)
mean(bills, na.rm = TRUE)
sum(bills > 40, na.rm = TRUE)


# ------------------------------------------------------------
# Read a CSV
# ------------------------------------------------------------
library(tidyverse)
library(palmerpenguins)
library(here)

dir.create(here("data", "raw"), recursive = TRUE, showWarnings = FALSE)
write_csv(penguins_raw, here("data", "raw", "penguins_raw.csv"))

penguins_raw <- read_csv(here("data", "raw", "penguins_raw.csv"))


# ------------------------------------------------------------
# First look: glimpse()
# ------------------------------------------------------------
glimpse(penguins, width = 70)


# ------------------------------------------------------------
# First look: summary() and count()
# ------------------------------------------------------------
summary(penguins$body_mass_g)

count(penguins, island)


# ------------------------------------------------------------
# Exercise: inspect the penguins
# ------------------------------------------------------------
# Solution (try it yourself first!)
sum(is.na(penguins$sex))
count(penguins, island, sort = TRUE)
class(penguins$year)


# ------------------------------------------------------------
# 1. Who lives where?
# ------------------------------------------------------------
# Solution (try it yourself first!)
count(penguins, island, species)


# ------------------------------------------------------------
# 2. Averages per group
# ------------------------------------------------------------
penguins |>
  group_by(species) |>
  summarise(mean_mass_g = mean(body_mass_g, na.rm = TRUE))

# Solution (try it yourself first!)
penguins |>
  group_by(species) |>
  summarise(mean_flipper_mm = mean(flipper_length_mm, na.rm = TRUE))


# ------------------------------------------------------------
# 3. Your first plot
# ------------------------------------------------------------
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, colour = species)) +
  geom_point()

# Solution (try it yourself first!)
ggplot(penguins,
       aes(x = bill_length_mm,
           y = body_mass_g,
           colour = species)) +
  geom_point()
