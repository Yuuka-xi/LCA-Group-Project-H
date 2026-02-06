rm (list = ls())

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
  par(mar = c(4, 4, 4, 2))
  timelineS(
    dataP,
    main = plot.Name,
    labels = events.name,
    label.direction = "up",
    label.position = 3,
    label.cex = 0.6  #Front size
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

events.BRC <- c(
  SR.BRC   = 60,
  CSJR.BRC = 25,
  CT.BRC   = 20,
  SG.BRC   = 40,
  END      = lifetime
)

dur.BRC <- c(
  SR.BRC   = 21,
  CSJR.BRC = 7,
  CT.BRC   = 5,
  SG.BRC   = 41
)

events.STB <- c(
  MP.STB = 15,
  FR.STB = 25,
  END    = lifetime
)

dur.STB <- c(
  MP.STB = 7,
  FR.STB = 21
)

design.Options.BRC <- "BRC – Building Reinforced Concrete"
design.Options.STB <- "STB – Steel Truss Bridge"

maintenance.BRC <- dist.Events(lifetime, events.BRC, start.Date, "BRC")
maintenance.STB <- dist.Events(lifetime, events.STB, start.Date, "STB")

maintenance.BRC
maintenance.STB

integrated.interv <- combine.lifeTimelines(
  maintenance.BRC,
  maintenance.STB,
  dur.BRC,
  dur.STB
)

integrated.interv

total.interruption <- sum(integrated.interv$duration)
total.interruption



# BRC — overlap-focused intervals
events.Op1.BRC <- c(
  SR.BRC   = 50,  # 60 -> 50
  CSJR.BRC = 25,
  CT.BRC   = 20,  
  SG.BRC   = 40, 
  END      = lifetime
)

# durations keep same (unless you also want to test duration changes)
duration.ev.BRC <- c(
  SR.BRC   = 21,
  CSJR.BRC = 7,
  CT.BRC   = 5,
  SG.BRC   = 41
)

# STB — overlap-focused intervals
events.Op1.STB <- c(
  MP.STB = 20,   # 15 -> 20 (跟 20/40/50 更對齊)
  FR.STB = 25,  
  END    = lifetime
)

duration.ev.STB <- c(
  MP.STB = 7,
  FR.STB = 21
)

# design option names (for plot titles / reporting)
design.Options.BRC <- "BRC – Building Reinforced Concrete"
design.Options.STB <- "STB – Steel Truss Bridge"

maintenance.BRC <- dist.Events(lifetime, events.Op1.BRC, start.Date, design.Options.BRC)
maintenance.STB <- dist.Events(lifetime, events.Op1.STB, start.Date, design.Options.STB)

integrated.interv <- combine.lifeTimelines(
  maintenance.BRC,
  maintenance.STB,
  duration.ev.BRC,
  duration.ev.STB
)
sum(integrated.interv$duration)

head(data.frame(
  year = integrated.interv$frequency,
  events = integrated.interv$Names,
  duration_days = integrated.interv$duration
), 20)
any(is.na(integrated.interv$duration))

design.explore <- function(events1, events2) {
  results <- c()
  
  for(i in 1:dim(events1)[1]) {
    ev1 <- unlist(events1[i, ])
    dist.1 <- dist.Events(lifetime, ev1, start.Date, design.Options.BRC, do.plot = FALSE)
    dur.ev1 <- duration.ev.BRC
    
    for(j in 1:dim(events2)[1]) {
      ev2 <- unlist(events2[j, ])
      dist.2 <- dist.Events(lifetime, ev2, start.Date, design.Options.STB, do.plot = FALSE)
      dur.ev2 <- duration.ev.STB
      
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

n.grid <- 3

# BRC scenarios
events.grid.BRC <- expand.grid(
  SR.BRC   = sample(seq(50, 70, by = 5), n.grid, replace = TRUE),
  CSJR.BRC = sample(seq(20, 30, by = 1), n.grid, replace = TRUE),
  CT.BRC   = sample(seq(15, 25, by = 1), n.grid, replace = TRUE),
  SG.BRC   = sample(seq(30, 50, by = 5), n.grid, replace = TRUE)
)

# STB
events.grid.STB <- expand.grid(
  MP.STB = sample(seq(10, 20, by = 1), n.grid, replace = TRUE),
  FR.STB = sample(seq(20, 30, by = 1), n.grid, replace = TRUE)
)

# Run exploration
response.space <- design.explore(events.grid.BRC, events.grid.STB)

response.space$dur <- as.numeric(as.character(response.space$dur))
response.space$dist.inter <- as.numeric(as.character(response.space$dist.inter))

p <- low(dur) * high(dist.inter)
sky <- psel(response.space, p)
sky
pareto2 <- psel(response.space, p, top = nrow(response.space))
ggplot(response.space, aes(x = dur, y = dist.inter)) +
  geom_point(shape = 21) +
  geom_point(data = pareto2, size = 3, aes(color = factor(pareto2$.level)))

names(response.space)
str(response.space[, c("dur", "dist.inter")])
response.space$dur <- as.numeric(as.character(response.space$dur))
response.space$dist.inter <- as.numeric(as.character(response.space$dist.inter))

show_front <- function(pref) {
  plot(response.space$dur, response.space$dist.inter)
  sky <- psel(response.space, pref)
  plot_front(response.space, pref, col = rgb(0, 0, 1))
  points(sky$dur, sky$dist.inter, lwd = 3)
}

show_front(p)

p <- high(dur) * low(dist.inter)
show_front(p)
sky <- psel(response.space, p)
sky

# =========================
# P15-P16
LCI.materials <- read.csv(file.choose(), stringsAsFactors = FALSE)
events_input <- read.csv(file.choose(), stringsAsFactors = FALSE)

# check
setdiff(events_input$material, LCI.materials$material)

# equal to the LCA.bridge function in the tutorial
LCA.byEvents <- function(events_input, timeline, LCI_materials) {
  
  # 1) Number of occurrences of each event over the life cycle
  event_freq <- as.data.frame(table(timeline$event), stringsAsFactors = FALSE)
  colnames(event_freq) <- c("event_code", "n_occurrences")
  event_freq <- subset(event_freq, !(event_code %in% c("DC", "END")))
  
  # 2) Merge event frequencies into events_input
  df <- merge(events_input, event_freq, by = "event_code", all.x = TRUE)
  df$n_occurrences[is.na(df$n_occurrences)] <- 0
  
  # 3) First try exact matching by material
  df2 <- merge(
    df,
    LCI_materials,
    by.x  = "material",
    by.y  = "material",
    all.x = TRUE
  )
  
  # 4) Identify rows not matched by material and apply scope-based fallback matching
  miss_idx <- which(is.na(df2$energy))
  if (length(miss_idx) > 0) {
    df_miss <- df2[miss_idx, ]
    keep_cols <- names(df)
    df_miss_core <- df_miss[, keep_cols, drop = FALSE]
    df_miss2 <- merge(
      df_miss_core,
      LCI_materials,
      by.x  = "material",
      by.y  = "scope",
      all.x = TRUE
    )
    lci_cols <- setdiff(names(LCI_materials), c("material", "scope"))
    for (col in lci_cols) {
      df2[miss_idx, col] <- df_miss2[[col]]
    }
  }
  
  # 5) Check if any rows are still unmatched
  if (any(is.na(df2$energy))) {
    missing <- unique(df2$material[is.na(df2$energy)])
    stop(paste(
      "These events_input.material values did not match any LCI material (or scope fallback):",
      paste(missing, collapse = ", ")
    ))
  }
  
  # 6) Life-cycle material quantities
  df2$total_material <- df2$quantities * df2$amount_per_event * df2$n_occurrences
  
  # 7) Life-cycle impacts
  df2$Energy_LC <- df2$total_material * df2$energy
  df2$CO2_LC    <- df2$total_material * df2$CO2
  df2$NOx_LC    <- df2$total_material * df2$NOx
  df2$Cost_LC   <- df2$total_material * df2$Cost
  
  LCA.results <- list(
    Energy = sum(df2$Energy_LC),
    CO2    = sum(df2$CO2_LC),
    NOx    = sum(df2$NOx_LC),
    Cost   = sum(df2$Cost_LC),
    detail = df2   # for debug
  )
  return(LCA.results)
}


# LCA data for BRC and STB
events_input.BRC  <- subset(events_input, system == "BRC")
LCI.BRC           <- subset(LCI.materials, system == "BRC")

events_input.STB <- subset(events_input, system == "STB")
LCI.STB          <- subset(LCI.materials, system == "STB")

LCA.BRC  <- LCA.byEvents(events_input.BRC,  maintenance.BRC,  LCI.BRC)
LCA.STB <- LCA.byEvents(events_input.STB, maintenance.STB, LCI.STB)

# LCA.BRC
# LCA.STB

integrated.Design <- as.data.frame(list(
  Energy = LCA.BRC$Energy + LCA.STB$Energy,
  CO2    = LCA.BRC$CO2    + LCA.STB$CO2,
  NOx    = LCA.BRC$NOx    + LCA.STB$NOx,
  Cost   = LCA.BRC$Cost   + LCA.STB$Cost
))
integrated.Design


# Convert environmental impacts into monetary costs
Energy.costs <- 0.128
CO2.unitcost <- 26 # per metric tone 
NOx.unitCost <- 42 # per metric tone

integrated.Design <- mutate(
  integrated.Design,
  TotalCosts = Energy * Energy.costs +
    (CO2/1000) * CO2.unitcost +
    (NOx/1000) * NOx.unitCost + 
    Cost
)

integrated.Design

