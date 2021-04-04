#
# Data visualisations on Minecraft Millport
# Susan Johnston
#
#

#~~ Load the libraries needed for this script.

library(ggplot2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 0. Demonstrating Anscombe's quartet                      #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

anscombe

summary(lm(y1 ~ x1, data = anscombe))
summary(lm(y2 ~ x2, data = anscombe))
summary(lm(y3 ~ x3, data = anscombe))
summary(lm(y4 ~ x4, data = anscombe))

#~~ Plotting anscombe (basic)

plot(y1 ~ x1, data = anscombe)
plot(y2 ~ x2, data = anscombe)
plot(y3 ~ x3, data = anscombe)
plot(y4 ~ x4, data = anscombe)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 1. Are dark ladybirds more likely to live in industrial (dark) backgrounds?  #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Create the data frame

ladybirds <- data.frame(Habitat = c("Industrial", "Industrial", "Rural", "Rural"),
                        Colour = c("black", "red", "black", "red"),
                        Count = c(115, 85, 30, 70))
ladybirds

# Make a bar plot

ggplot(ladybirds, aes(x = Habitat, y = Count, fill = Colour)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_identity(guide = "legend")

# Do a chi-square test

ladybirds2 <- matrix(c(115, 85, 30, 70), nrow = 2)

chisq.test(ladybirds2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 2. Does body weight differ significantly between Adelie and Gentoo penguins?   #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Load the dataset

#install.packages("palmerspenguins")   # run this line once!
library(palmerpenguins)
data("penguins")

# Remove chinstrap penguins just now :)

penguins <- subset(penguins, species != "Chinstrap")

# Preliminary visualisation using geom_dotplot()

ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 100) +
  labs(x = "Species", y = "Body Mass (g)")

# Proper visualisation using geom_boxplot()

ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_boxplot() +
  labs(x = "Species", y = "Body Mass (g)")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 3. Does flipper length vary relative to body weight in penguins?               #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Basic plot

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point() +
  labs(x = "Flipper Length (mm)", y = "Body Mass (g)")

fit1 <- lm(body_mass_g ~ flipper_length_mm, data = penguins)
summary(fit1)

# Add a linear regression line

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point() +
  labs(x = "Flipper Length (mm)", y = "Body Mass (g)") +
  stat_smooth(method = "lm")

# Could this be stratified by species?

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, colour = species)) +
  geom_point() +
  labs(x = "Flipper Length (mm)", y = "Body Mass (g)") +
  stat_smooth(method = "lm")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 4. Customising a plot                  #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Make the background white

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, colour = species)) +
  geom_point() +
  labs(x = "Flipper Length (mm)", y = "Body Mass (g)") +
  stat_smooth(method = "lm") +
  theme_bw()
  
# Create separate graphs with different variables with facet_wrap

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, colour = species)) +
  geom_point() +
  labs(x = "Flipper Length (mm)", y = "Body Mass (g)") +
  stat_smooth(method = "lm") +
  facet_wrap(~species)


# Move legend position to "top", "right", "left", "bottom", or "none"

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, colour = species)) +
  geom_point() +
  labs(x = "Flipper Length (mm)", y = "Body Mass (g)") +
  stat_smooth(method = "lm") +
  theme(legend.position = "top")

#~ Change the font sizes of labels

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, colour = species)) +
  geom_point() +
  labs(x = "Flipper Length (mm)", y = "Body Mass (g)") +
  stat_smooth(method = "lm") +
  theme(legend.position = "top",
        axis.text = element_text(size = 12),    # tick label size
        axis.title = element_text(size = 14),   # axis label size
        legend.text = element_text(size = 12),  # legend text size
        legend.title = element_text(size = 14)) # legend title size


