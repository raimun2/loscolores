# Limpieza de entorno
rm(list = ls())

# Dependencias
library(farver)
library(ggplot2)
library(patchwork)
library(dplyr)
library(tidyr)

# Source del motor (Orden estricto)
source("R/utils_color.R")
source("R/engine_core.R")
source("R/engine_expansion.R")
source("R/constructor.R")
source("R/viz.R")
source("R/data_handlers.R")

input_test <- "#4fb3aaff"
plot(loscolores_system(input_test))

input_test <- "#FFFFFF"
plot(loscolores_system(input_test))

input_test <- c("#F5F5F5", "#F0F0F0", "#FAF9F6")
plot(loscolores_system(input_test))

input_test <- c("#00FF00", "#FF00FF", "#00FFFF")
plot(loscolores_system(input_test))

input_test <- c("#4B3621", "#3B2F2F", "#555555")
plot(loscolores_system(input_test))

input_test <- c("#003B00", "#008F11", "#00FF41")
plot(loscolores_system(input_test))

input_test <- apply(matrix(sample(0:255, 768, replace=T), ncol=3), 1, 
                    function(x) rgb(x[1], x[2], x[3], maxColorValue=255))
plot(loscolores_system(input_test, n_cat = 25))

input_test <- c("#FF4500", "#1E90FF")
plot(loscolores_system(input_test))

input_test <- sample(c("#D2B48C", "#C19A6B", "#A0522D", "#8B4513"), 100, replace=TRUE)
plot(loscolores_system(input_test))

input_test <- "#0047AB"
plot(loscolores_system(input_test))

input_test <- c("#FF0000", "#0000FF", "#FFFF00", "#00FF00")
plot(loscolores_system(input_test))

input_test <- c("#6B8E23", "#A0522D", "#2E8B57")
plot(loscolores_system(input_test))

input_test <- c("#000000", "#FFFFFF")
plot(loscolores_system(input_test))

input_test <- c("#333333", "#666666", "#999999", "#CCCCCC")
plot(loscolores_system(input_test))

input_test <- c("#000020", "#000040", "#FFFFFF")
plot(loscolores_system(input_test))