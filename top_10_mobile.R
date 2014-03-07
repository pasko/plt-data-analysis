# Copyright (c) 2014 The Chromium Authors. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS-IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

require(data.table)

all_graphs <- function(data) {
  org <- data[data$name == "original",]
  cores1 <- data[data$name == "cores1",]
  cores2 <- data[data$name == "cores2",]
  cores4 <- data[data$name == "cores4",]
  
  # Display traditionally-looking timeline graphs.
  plot_site_timeline <- function(frame, site_name, col="blue") {
    slice <- frame[frame$site == site_name,]
    lines(slice$index, slice$warm_time, col=col, lwd=3)
  }
  plot_site_timeline_temperature <- function(frame, site_name) {
    slice <- frame[frame$site == site_name,]
    lines(slice$index, slice$temperature, lty=2)
  }
  plot_all_with_temperature <- function(frame, temp_from, title) {
    plot(1, type="n", xlim=c(0, 29), ylim=c(800, 3000), main=title, ylab="Warm time (ms)", xlab="# of measurement")
    plot_site_timeline(frame, "wikipedia", col="red")
    plot_site_timeline(frame, "google", col="green")
    plot_site_timeline(frame, "facebook", col="blue")
    legend("topleft", c("wikipedia", "google", "facebook", "temperature"), lty=c(1, 1, 1, 2), lwd=c(3, 3, 3, 1), col=c("red", "green", "blue", "black"))
    temp_to = 70
    plot.window(xlim=c(0, 29), ylim=c(temp_from, temp_to))
    axis(4, at=seq(from=temp_from, to=temp_to, by=2))
    mtext("Temperature (Degrees Celsius)", side=4, line=3)
    plot_site_timeline_temperature(frame, "wikipedia")
    plot_site_timeline_temperature(frame, "google")
    plot_site_timeline_temperature(frame, "facebook")
  }
  par_save <- par(no.readonly = TRUE)
  par(mfrow = c(1, 2), mar=c(5, 5, 5, 5))
  temp_from <- min(data$temperature)
  plot_all_with_temperature(org, temp_from, "Original")
  plot_all_with_temperature(cores4, temp_from, "4 CPU cores enabled")
  par(par_save)
  
  # Dislay original PLT as a function of temperature.
  par(mfrow = c(4, 3))
  min_temp <- min(data$temperature)
  max_temp <- max(data$temperature)
  plot_original_temp_to_time <- function(data, site, min_temp, max_temp) {
    org <- data[data$name == "original" & data$site == site, ]
    z <- org$temperature
    diff_temp <- max_temp - min_temp
    c1 <- (z - min_temp) / diff_temp
    c2 <- (z - min_temp + 2) / (diff_temp + 2)
    color <- rgb(c1, 0.5*c1, 1-c1)
    plot(org$temperature, org$warm_time, pch=19, main=site, col=color, cex=2, xlab="", ylab="")
  }
  site_names <- unique(data$site)
  for (site in site_names) {
    plot_original_temp_to_time(data, site, min_temp, max_temp)
  }

  # Boxplots outline distributions for each experiment.
  boxplot_site <- function(site_name) {
    slice <- data[data$site == site_name,]
    boxplot(warm_time ~ factor(name, c("original", "cores2", "cores4")), data=slice, main=site_name)
  }
  par(mfrow = c(4, 3))
  for (site in site_names) {
    boxplot_site(site)
  }
  
  # Plot Relative StDev.
  aggregate_warm_time <- function(df, sd_name) {
    a_median <- aggregate(warm_time ~ site, data=df, FUN=median)
    setnames(a_median, "warm_time", "median")
    a_sd <- aggregate(warm_time ~ site, data=df, FUN=sd)
    setnames(a_sd, "warm_time", "sd")
    a_merged <- merge(a_median, a_sd, by="site")
    a_merged[,sd_name] <- (a_merged$sd / a_merged$median)
    return(a_merged)
  }
  org_merged <- aggregate_warm_time(org, "org_sd")
  cores2_merged <- aggregate_warm_time(cores2, "cores2_sd")
  pre_merge <- merge(org_merged, cores2_merged, by="site")
  cores4_merged <- aggregate_warm_time(cores4, "cores4_sd")
  comparison <- merge(pre_merge, cores4_merged, by="site")
  par(mfrow = c(1, 1))
  relative_std_graph <- comparison[,c("org_sd", "cores2_sd", "cores4_sd")]
  row.names(relative_std_graph) <- comparison$site
  barplot(t(relative_std_graph), legend=c("original", "2 cores", "4 cores"), main="", ylab="Relative StDev (lower is better)", beside=TRUE)
}

n4_data <- read.csv("./data/top_10_mobile30.csv")
n5_data <- read.csv("./data/top_10_mobile30_n5.csv")
all_graphs(n4_data)
all_graphs(n5_data)
