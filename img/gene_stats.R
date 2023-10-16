library(ggplot2)

setwd("../Desktop/github_perso/machine_learning_courses/img/")

### NORMAL ###

# Parameters for the normal distribution
mus <- c(0, 0, 2, 2)
sigmas <- c(1, 2, 1, 2)
linetypes <- c("solid", "dashed")
colors <- c("blue", "red")

x = seq(-5, 5, by = 0.01)

data <- data.frame(
  x = c(x,x,x,x),
  mu = factor(rep(mus, each = length(x))),
  sigma = factor(rep(sigmas, each = length(x))))

data$y <- with(data, dnorm(x, mean = as.numeric(as.character(mu)), sd = as.numeric(as.character(sigma))))

g = ggplot(data, aes(x=x, y=y, linetype=mu, color=sigma)) +
  geom_line() +
  scale_linetype_manual(values = linetypes, name = expression(mu)) +
  scale_color_manual(values = colors, name = expression(sigma)) +
  labs(title="Normal Distribution", x="x", y="Density") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("law_normal.svg",g, height = 4, width = 4)

### GAMMA ###

# Parameters for the normal distribution
shapes <- c(0.5, 0.5, 1, 1, 3, 3)
rates <- c(1, 2, 1, 2, 1, 2)
linetypes <- c("solid", "dashed", "dotted")
colors <- c("blue", "red")

x = seq(0.01, 2.5, by = 0.01)

data <- data.frame(
  x = c(x,x,x,x,x,x),
  shape = factor(rep(shapes, each = length(x))),
  rate = factor(rep(rates, each = length(x))))

data$y <- with(data, dgamma(x, shape = as.numeric(as.character(shape)), rate = as.numeric(as.character(rate))))

g = ggplot(data, aes(x=x, y=y, linetype=shape, color=rate)) +
  geom_line() +
  scale_linetype_manual(values = linetypes, name = expression(alpha)) +
  scale_color_manual(values = colors, name = expression(lambda)) +
  labs(title="Normal Distribution", x="x", y="Density") +
  theme_classic() +
  ylim(0,1.2) +
  theme(plot.title = element_text(hjust = 0.5))

plot(g)

ggsave("law_gamma.svg",g, height = 4, width = 4)

### BETA ###

# Parameters for the normal distribution
shapes <- c(0.5, 0.5, 1, 1, 3, 3)
rates <- c(1, 2, 1, 2, 1, 2)
linetypes <- c("solid", "dashed", "dotted")
colors <- c("blue", "red")

x = seq(0.01, 2.5, by = 0.01)

data <- data.frame(
  x = c(x,x,x,x,x,x),
  shape = factor(rep(shapes, each = length(x))),
  rate = factor(rep(rates, each = length(x))))

data$y <- with(data, dgamma(x, shape = as.numeric(as.character(shape)), rate = as.numeric(as.character(rate))))

g = ggplot(data, aes(x=x, y=y, linetype=shape, color=rate)) +
  geom_line() +
  scale_linetype_manual(values = linetypes, name = expression(alpha)) +
  scale_color_manual(values = colors, name = expression(lambda)) +
  labs(title="Normal Distribution", x="x", y="Density") +
  theme_classic() +
  ylim(0,1.2) +
  theme(plot.title = element_text(hjust = 0.5))

plot(g)

ggsave("law_gamma.svg",g, height = 4, width = 4)

