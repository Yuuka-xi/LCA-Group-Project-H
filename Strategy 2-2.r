install.packages("timelineS")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("reshape2")
install.packages("rPref")
install.packages("mco")
install.packages("MASS")

library(timelineS)
library(lubridate)
library(ggplot2)
library(dplyr)
library(reshape2)
library(rPref)
library(mco)
library(MASS)



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



dist.Events <- function (lifetime, events, start.Date, option.Name, do.plot = TRUE) {
  events <- sort(events, decreasing = TRUE)
  distribution.events <- sapply(events, seq, from = 0, to = lifetime)
  all.events <- melt(distribution.events)
  colnames(all.events) <- c("frequency", "event")
  all.events <- all.events[order(all.events$frequency),]
  unique.events <- all.events[!duplicated(all.events$frequency),]
  unique.events$event[which(unique.events$frequency == 0)] <- "DC"
  
  if(do.plot) {
    plot.timeline(unique.events$frequency, unique.events$event, start.Date, option.Name)
  }
  
  return(unique.events)
}

combine.lifeTimelines <- function(product.1, product.2, p1.dur, p2.dur) {
  
  base <- list(Names = c(), frequency = c(), duration = c())
  
  base$frequency <- sort(unique(c(product.1$frequency, product.2$frequency)))
  
  base$Names[1] <- "DC"
  base$duration[1] <- 0
  base$Names[length(base$frequency)] <- "END"
  base$duration[length(base$frequency)] <- 0
  
  for(index in 2:(length(base$frequency) - 1)) {
    
    phase.1 <- product.1$event[product.1$frequency == base$frequency[index]]
    phase.2 <- product.2$event[product.2$frequency == base$frequency[index]]
    
    if(length(phase.1) != 0 && length(phase.2) != 0) {
      base$Names[index] <- paste(phase.1, phase.2)
      base$duration[index] <- max(
        p1.dur[names(p1.dur) == phase.1],
        p2.dur[names(p2.dur) == phase.2]
      )
    } else if(length(phase.1) != 0) {
      base$Names[index] <- phase.1
      base$duration[index] <- p1.dur[names(p1.dur) == phase.1]
    } else if(length(phase.2) != 0) {
      base$Names[index] <- phase.2
      base$duration[index] <- p2.dur[names(p2.dur) == phase.2]
    } else {
      base$duration[index] <- 0
    }
  }
  
  return(base)
}

lifetime <- 120
start.Date <- "2026-01-01"

events.pcf <- c(
  SR.BRC   = 60,
  CSJR.BRC = 25,
  CT.BRC   = 20,
  SG.BRC   = 40,
  END      = lifetime
)

dur.pcf <- c(
  SR.BRC   = 21,
  CSJR.BRC = 7,
  CT.BRC   = 5,
  SG.BRC   = 41
)

events.gcw <- c(
  SSRC.RLW = 51,   
  RM.RLW   = 3,    
  FRR.RLW  = 80,   
  END      = lifetime
)

dur.gcw <- c(
  
  SSRC.RLW = 1,     
  RM.RLW   = 0,     
  FRR.RLW  = 0.25   
)

# =========================
# System names 
# =========================
design.Options.pcf <- "BRC – Building Reinforced Concrete"
design.Options.gcw <- "RLW – Railway Track (orange track)"   # <- 原本寫成 STB 是錯的（但數據不改）

# =========================
# Generate maintenance timelines 
# =========================
maintenance.pcf <- dist.Events(lifetime, events.pcf, start.Date, design.Options.pcf)
maintenance.gcw <- dist.Events(lifetime, events.gcw, start.Date, design.Options.gcw)

integrated.interv <- combine.lifeTimelines(
  maintenance.pcf,
  maintenance.gcw,
  dur.pcf,
  dur.gcw
)

total.interruption <- sum(integrated.interv$duration)
total.interruption


# =========================
# Design exploration 
# =========================
design.explore <- function(events1, events2) {
  results <- c()
  
  for(i in 1:dim(events1)[1]) {
    ev1 <- unlist(events1[i, ])
    dist.1 <- dist.Events(lifetime, ev1, start.Date, design.Options.pcf, do.plot = FALSE)
    dur.ev1 <- dur.pcf
    
    for(j in 1:dim(events2)[1]) {
      ev2 <- unlist(events2[j, ])
      dist.2 <- dist.Events(lifetime, ev2, start.Date, design.Options.gcw, do.plot = FALSE)
      dur.ev2 <- dur.gcw
      
      combined.lifetime <- combine.lifeTimelines(dist.1, dist.2, dur.ev1, dur.ev2)
      
      min.dist.int <- min(abs(
        combined.lifetime$frequency[1:(length(combined.lifetime$frequency) - 1)] -
          combined.lifetime$frequency[2:length(combined.lifetime$frequency)]
      ))
      
      results <- rbind(results, c(ev1, ev2,
                                  dur = sum(combined.lifetime$duration),
                                  dist.inter = min.dist.int
      ))
    }
  }
  
  return(as.data.frame(results))
}


# =========================
# Grid sampling 
# =========================
n.grid <- 5

events.grid.PCF <- expand.grid(
  SR.BRC   = sample(seq(50, 70, by = 1), n.grid, replace = TRUE),
  CSJR.BRC = sample(seq(20, 30, by = 1), n.grid, replace = TRUE),
  CT.BRC   = sample(seq(15, 25, by = 1), n.grid, replace = TRUE),
  SG.BRC   = sample(seq(30, 50, by = 1), n.grid, replace = TRUE)
)

events.grid.GCW <- expand.grid(
  SSRC.RLW = 51,
  RM.RLW   = sample(seq(2, 5, by = 1), n.grid, replace = TRUE),
  FRR.RLW  = 80
)

response.space <- design.explore(events.grid.PCF, events.grid.GCW)

response.space$dur <- as.numeric(as.character(response.space$dur))
response.space$dist.inter <- as.numeric(as.character(response.space$dist.inter))

p <- low(dur) * high(dist.inter)
sky <- psel(response.space, p)
sky

pareto2 <- psel(response.space, p, top = nrow(response.space))
ggplot(response.space, aes(x = dur, y = dist.inter)) +
  geom_point(shape = 21) +
  geom_point(data = pareto2, size = 3, aes(color = factor(pareto2$.level)))

show_front <- function(pref) {
  plot(response.space$dur, response.space$dist.inter)
  sky <- psel(response.space, pref)
  plot_front(response.space, pref, col = rgb(0, 0, 1))
  points(sky$dur, sky$dist.inter, lwd = 3)
}

show_front(p)

p2 <- high(dur) * low(dist.inter)
show_front(p2)
sky2 <- psel(response.space, p2)
sky2

