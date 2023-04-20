getwd()

# set working directory
setwd("/Users/tanjona/Library/Mobile Documents/com~apple~CloudDocs/misc/service_friend/ESSA_demo")

# import data about airlines
demo_dat <- read.csv("data/airlines.csv", row.names = 1)

dat  <- read.csv("data/BD_Carbone_2.csv")

View(dat)

# load dyplr
library(dplyr)

# count how many site per soil occupation
n_site <- dat |>
    count(OccSol) |>
    select(n)

# calculate site, divide by 3 because 3 depth
n_site_agr <- n_site[1,] / 3
n_site_cua <- n_site[2,] / 3

# create a vector for site ID
v1  <- rep(1:n_site_agr, each = 3)
v2 <- rep(1:n_site_cua, each = 3)
site_id <- c(v1, v2)

length(site_id)
dim(dat) # need dim because length gives the number of column for 2 dim data

# add site ID vector to the data, make site_Id factor
dat$site_id  <- as.factor(site_id)

View(dat)

# overview of the structure of the data
str(dat)

# export data
write.csv(dat, "data/BD_carb_clean.csv")

# calculate the total and mean SOC for Soil utilization and depth
sum_SOC <- dat |>
    group_by(OccSol, Prof) |>
    summarise(totSOC = sum(SOC), meanSOC = mean(SOC))

# Make a bar plot with
    # the total SOC on y
    # soil utilization and depth on x axis
    # use different color for depth
library(ggplot2)
ggplot(sum_SOC, aes(x = OccSol, y = meanSOC, fill = Prof)) +
    geom_col(position = "dodge")

# Make 2 x 3 panel histograms with SOC on x axis
    # rows represent soil utilization
    # columns represent depth
ggplot(dat, aes(x = SOC)) +
    geom_histogram(binwidth = 5) +
    facet_grid(OccSol ~ Prof)

# Plot a box plot, x axis = depth, y axis = total SOC
    # put Soil utilization on different panel
ggplot(dat, aes(x = Prof, y = SOC, fill = Prof)) +
    geom_boxplot() +
    facet_grid(~OccSol)


# if we want to export/save the figure, the best practice is to name the figure
fig_soc <- ggplot(dat, aes(x = Prof, y = SOC, fill = Prof)) +
    geom_boxplot() +
    facet_grid(~OccSol)

ggsave("fig/fig_boxplot.pdf", fig_soc, width = 140, unit = "mm")

# Use Kruskal Wallis test to see if
    # the median of the total SOC differs among depths
    # maybe anova is better if we test for normality first
dat_agro <- dat |> filter(OccSol == "Agroforesterie")
kruskal.test(SOC ~ Prof, data = dat_agro)

# if there is a difference among depths, use post hoc to see which depths differ
    # using Dunn test here, for pairwise comparison
install.packages("FSA")
library(FSA)

dunnTest(SOC ~ Prof,
              data = dat_agro,
              method = "bh")
