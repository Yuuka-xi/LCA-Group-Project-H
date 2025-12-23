rm (list = ls())

library(timelineS) 
library(lubridate) 
library(ggplot2) 
library(dplyr) 
library(reshape2) 
library(rPref) 
library(mco) 
library(MASS)

# --------

#This function convert a set of EventTime into actual dates
#(I think it's better using EventTime here)
# And then draw a timeline
plot.timeline <- function(lifetime, events.name, start.Date, plot.Name) {
  dataP <- data.frame(
    Events = events.name,
    Event_Dates = ymd(start.Date) + years(lifetime)
  )
  timelineS(
    dataP,
    main = plot.Name,
    labels = events.name,
    label.direction = "up",
    label.position = 3
  )
}

# This function generates the life-cycle timeline of maintenance events
# by expanding intervention intervals into a sorted sequence of event years.
dist.Events <- function(lifetime, events, start.Date, option.Name) {
  # sort the events
  events <- sort(events, decreasing = TRUE)
  # create the distribution of the events
  distribution.events <- sapply(events, seq, from = 0, to = lifetime)
  # all events unlisted
  all.events <- melt(distribution.events)
  colnames(all.events) <- c("frequency", "event")
  # sort the events ascending
  all.events <- all.events[order(all.events$frequency), ]
  # get the unique sequence of events
  unique.events <- all.events[!duplicated(all.events$frequency), ]
  unique.events$event[which(unique.events$frequency == 0)] <- "DC"
  # plot the timeline
  # plot.timeline(
  #   unique.events$frequency,
  #   unique.events$event,
  #   start.Date,
  #   option.Name
  # )
  return(unique.events)
}

#This function merges the maintenance timelines of two systems into a single "system-level" timeline 
#And calculates the downtime caused by each maintenance.
combine.lifeTimelines <- function(product.1, product.2, p1.dur, p2.dur) {
  
  # Create an output container for the combined timeline
  base <- list(Names = c(), frequency = c(), duration = c())
  
  # Collect all unique event years from both products and sort them ascending
  base$frequency <- sort(
    unique(c(product.1$frequency, product.2$frequency)),
    decreasing = FALSE
  )
  
  # Initialize start and end points with zero duration
  base$Names[1] <- "DC"
  base$duration[1] <- 0
  base$Names[length(base$frequency)] <- "END"
  base$duration[length(base$frequency)] <- 0
  
  # Loop through the timeline
  for (index in 2:(length(base$frequency) - 1)) {
    
    # Find which event happens in product 1 and product 2 at this year (if any)
    phase.1 <- product.1$event[which(product.1$frequency == base$frequency[index])]
    phase.2 <- product.2$event[which(product.2$frequency == base$frequency[index])]
    
    # Case 1: both products have an event at the same year (overlap)
    if (length(phase.1) != 0 && length(phase.2) != 0) {
      base$Names[index] <- paste(phase.1, phase.2)
      # Interruption duration is the maximum of the two durations
      base$duration[index] <- max(
        p1.dur[which(names(p1.dur) == phase.1)],
        p2.dur[which(names(p2.dur) == phase.2)]
      )
      # Case 2: only product 1 has an event at this year
    } else if (length(phase.1) != 0 && length(phase.2) == 0) {
      base$Names[index] <- phase.1
      base$duration[index] <- p1.dur[which(names(p1.dur) == phase.1)]
      # Case 3: only product 2 has an event at this year
    } else if (length(phase.1) == 0 && length(phase.2) != 0) {
      base$Names[index] <- phase.2
      base$duration[index] <- p2.dur[which(names(p2.dur) == phase.2)]
      # Case 4: no event
    } else {
      base$duration[index] <- 0
    }
  }
  return(base)
}

# --------

# par(mfrow = c(2, 1))
start.Date <- "2020-01-01"
lifetime <- 120  # years

# ---Steel Truss Bridge(.stb)---
#Maintenance Data Input
events.Op1.stb <- c(
  RM.stb = 5,  # Routine Maintenance
  MP.stb = 15,   # Member Replacement
  FR.stb = 25,   # Full Recoating
  END    = lifetime
)

duration.ev.stb <- c(
  RM.stb = 2,
  MP.stb = 7,
  FR.stb = 21
)

# Define design option names for each system
design.Options.stb <- list(
  desing.Op1.stb <- "Steel Truss Bridge 1"
)

# Generate life-cycle maintenance event distributions for each system
maintenance.stb <- dist.Events(
  lifetime,
  events.Op1.stb,
  start.Date,
  design.Options.stb
)

# Show diagram
par(cex = 0.4)
plot.timeline(maintenance.stb$frequency, maintenance.stb$event, start.Date, "Steel Truss Bridge 1")

# ---Railway Track timber sleepers(.rlwt)---
events.Op1.rlwt <- c(
  rlwt_RP.rep = 10,  # Full rail pad replacement
  rlwt_RP.loc = 5,   # Localised rail pad replacement
  END    = lifetime
)

duration.ev.rlwt <- c(
  rlwt_RP.rep = 1, 
  rlwt_RP.loc = 0
)

design.Options.rlwt <- list(
  desing.Op1.rlwt <- "Railway Track timber sleepers 1"
)

maintenance.rlwt <- dist.Events(
  lifetime,
  events.Op1.rlwt,
  start.Date,
  design.Options.rlwt
)


# ---------
# Combine two maintenance timelines into one integrated system timeline
integrated.interv <- combine.lifeTimelines(
  maintenance.stb,
  maintenance.rlwt,
  duration.ev.stb,
  duration.ev.rlwt
)

# Total downtime (days) over the whole lifetime
total.downtime.days <- sum(integrated.interv$duration)
total.downtime.days

# ---------

# Automate exploration of maintenance strategy combinations
# Modified design space exploration function.
# - Uses real intervention durations instead of simplified assumptions.
# - Computes the minimum interval only between major interventions,
#   avoiding the degenerate case where the minimum interval is always 1 year
#   due to frequent minor maintenance actions.
design.explore <- function(events1, events2, dur1, dur2, major1, major2, name1, name2) {
  results <- c()
  
  # Loop over all scenarios (rows) for system 1
  for (i in 1:nrow(events1)) {
    ev1 <- as.numeric(unlist(events1[i, ]))
    names(ev1) <- colnames(events1)
    ev1 <- c(ev1, END = lifetime)
    dist.1 <- dist.Events(lifetime, ev1, start.Date, name1)
    
    # Loop over all scenarios (rows) for system 2
    for (j in 1:nrow(events2)) {
      ev2 <- as.numeric(unlist(events2[j, ]))
      names(ev2) <- colnames(events2)
      ev2 <- c(ev2, END = lifetime)
      dist.2 <- dist.Events(lifetime, ev2, start.Date, name2)
      
      # Merge both timelines into a single system-level timeline
      combined <- combine.lifeTimelines(dist.1, dist.2, dur1, dur2)
      
      # Compute dist.inter using MAJOR interventions only
      major_patterns <- c(major1, major2)
      freq_major <- combined$frequency[
        sapply(combined$Names, function(x) any(grepl(paste(major_patterns, collapse = "|"), x)))
      ]
      
      # Minimum interval (years) between consecutive major interventions
      dist.inter <- if (length(freq_major) >= 2) min(diff(freq_major)) else NA_real_
      
      results <- rbind(results, c(ev1[1:ncol(events1)], ev2[1:ncol(events2)],
          dur = sum(combined$duration), dist.inter = dist.inter))
    }
  }
  return(as.data.frame(results))
}

# n.grid defines how many values are sampled for each intervention parameter
n.grid <- 4

# Generate intervention scenarios for steel truss bridge
events.grid.stb <- expand.grid(
  RM.stb = sample(seq(4, 8, by = 1), n.grid), #5
  MP.stb = sample(seq(10, 20, by = 1),  n.grid), #15
  FR.stb = sample(seq(20, 35, by = 1), n.grid) #25
)

# Generate intervention scenarios for Railway Track timber sleepers
events.grid.rlwt <- expand.grid(
  rlwt_RP.rep = sample(seq(5, 15, by = 1), n.grid), #10
  rlwt_RP.loc = sample(seq(4, 8, by = 1),  n.grid) #5
)

# Explore all combinations
response.space <- design.explore(events.grid.stb, 
                                 events.grid.rlwt, 
                                 dur1 = duration.ev.stb,
                                 dur2 = duration.ev.rlwt,
                                 major1 = c("MP.stb", "FR.stb"),
                                 major2 = c("rlwt_RP.rep"),
                                 name1 = "stb", name2 = "rlwt")

# Make sure numeric columns are numeric
response.space$dur <- as.numeric(response.space$dur)
response.space$dist.inter <- as.numeric(response.space$dist.inter)


# Define preference: minimize downtime, maximize minimum interval
p <- low(dur) * high(dist.inter)


# Select the best alternative according to preference p
sky <- psel(response.space, p)
sky

# Rank all alternatives
pareto2 <- psel(response.space, p, top = nrow(response.space))

# Visualize ranking levels
ggplot(response.space, aes(x = dur, y = dist.inter)) +
  geom_point(shape = 21) +
  geom_point(
    data = pareto2,
    size = 3,
    aes(color = factor(.level))
  )


# Plot Pareto frontier + highlight the best alternative under a given preference
show_front <- function(pref) {
  plot(
    response.space$dur,
    response.space$dist.inter,
    xlab = "Total downtime (dur)",
    ylab = "Min. distance between interventions (dist.inter)"
  )
  
  # Select best alternative(s) under the preference
  sky <- psel(response.space, pref)
  
  # Plot Pareto frontier (blue line)
  plot_front(response.space, pref, col = rgb(0, 0, 1))
  
  # Highlight the best alternative(s) (black points)
  points(sky$dur, sky$dist.inter, lwd = 3)
}

# Visualize Pareto frontier for preference p 
show_front(p)

# This part of the script is based on the tutorial (pages P8–P14).
# Most of the code follows the tutorial structure, with additional comments
# added to improve readability and understanding.
#
# The function design.explore was modified to better reflect our use case,

# Further work is required to complete the model, including:
# - adding the missing input data for all systems, and
# - extending the current functions so that they can integrate six systems
#   instead of only two.



