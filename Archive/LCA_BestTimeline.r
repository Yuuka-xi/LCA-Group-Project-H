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
  
  # Set all events occurring at year 0 as "DC" 
  all.events$event[all.events$frequency == 0] <- "DC"
  
  # Remove maintenance events occurring at the same year as END
  # while keeping the END event as the life-cycle boundary
  all.events <- subset(all.events, !(frequency == lifetime & event != "END"))
  
  # Use a de-duplicated version of the timeline only for plotting,
  # to avoid overlapping labels in the same year
  if (do.plot) {
    unique.events <- all.events[!duplicated(all.events$frequency), ]
    plot.timeline(unique.events$frequency, unique.events$event, start.Date, option.Name)
  }
  
  return(all.events)  # Return full event list
}



lifetime <- 120
start.Date <- "2026-01-01"

events.PCF <- c(
  CR.pcf   = 18,
  JM.pcf   = 18,
  PR.pcf   = 60,
  END      = lifetime
)

events.GCW <- c(
  DC.gcw   = 9,
  GR.gcw   = 9,
  IR.gcw   = 60,
  END      = lifetime
)

events.BRC <- c(
  SR.brcs   = 61,
  CSJR.brcs = 20,
  CT.brcp   = 16,
  SG.brcf   = 30,
  END      = lifetime
)

events.STB <- c(
  MP.stb = 20,
  FR.stb = 20,
  END    = lifetime
)

events.RLWC <- c(
  SSRC.rlw_slp = 51,
  RM.rlw_rls   = 5,
  FRR.rlw_rls  = 80,
  END      = lifetime
)

events.RLWT <- c(
  rlwt_TS.full = 30,
  rlwt_SB.geo   = 90,
  FRR.rlwt  = 66,
  END      = lifetime
)

maintenance.PCF <- dist.Events(lifetime, events.PCF, start.Date, "PCF")
maintenance.GCW <- dist.Events(lifetime, events.GCW, start.Date, "GCW")
maintenance.BRC <- dist.Events(lifetime, events.BRC, start.Date, "BRC")
maintenance.STB <- dist.Events(lifetime, events.STB, start.Date, "STB")
maintenance.RLWC <- dist.Events(lifetime, events.RLWC, start.Date, "RLWC")
maintenance.RLWT <- dist.Events(lifetime, events.RLWT, start.Date, "RLWT")

#Just to check the output, you can comment it
maintenance.PCF
maintenance.GCW
maintenance.BRC
maintenance.STB
maintenance.RLWC
maintenance.RLWT


# =========================
# P15-P16
LCI.materials <- read.csv(file.choose(), stringsAsFactors = FALSE)
events_input <- read.csv(file.choose(), stringsAsFactors = FALSE)

# check
# setdiff(events_input$material, LCI.materials$material)

# equal to the LCA.bridge function in the tutorial
LCA.byEvents <- function(events_input, timeline, LCI_materials) {
  
  # 1) Count the number of occurrences of each event over the life cycle
  event_freq <- as.data.frame(table(timeline$event), stringsAsFactors = FALSE)
  colnames(event_freq) <- c("event_code", "n_occurrences")
  event_freq <- subset(event_freq, !(event_code %in% c("DC", "END")))
  
  # 2) Merge event frequencies into events_input
  df <- merge(events_input, event_freq, by = "event_code", all.x = TRUE)
  df$n_occurrences[is.na(df$n_occurrences)] <- 0  # timeline里没出现的事件 → 0次
  
  # 3) Build an LCI lookup table using both material and scope as keys
  LCI_keyed <- rbind(
    transform(LCI_materials, join_key = material),
    transform(LCI_materials, join_key = scope)
  )
  LCI_keyed <- LCI_keyed[!is.na(LCI_keyed$join_key) & LCI_keyed$join_key != "", ]
  
  # 4) Match events_input materials with the LCI lookup table
  df$join_key <- df$material
  df2 <- merge(df, LCI_keyed, by = "join_key", all.x = TRUE)
  
  # # 5) Check for unmatched materials or scopes
  # if (any(is.na(df2$energy))) {
  #   missing <- unique(df2$join_key[is.na(df2$energy)])
  #   stop(paste("These event_input.material values did not match any LCI material/scope:", 
  #              paste(missing, collapse = ", ")))
  # }
  
  # 6) Compute life-cycle material quantities
  df2$total_material <- df2$quantities * df2$amount_per_event * df2$n_occurrences
  
  # 7)Compute life-cycle environmental impacts
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


# LCA data

events_input.PCF <- subset(events_input, system == "PCF")
events_input.GCW <- subset(events_input, system == "GCW")
events_input.BRC  <- subset(events_input, system == "BRC")
events_input.STB <- subset(events_input, system == "STB")
events_input.RLWC <- subset(events_input, system == "RLWC")
events_input.RLWT <- subset(events_input, system == "RLWT")

LCA.PCF  <- LCA.byEvents(events_input.PCF,  maintenance.PCF,  LCI.materials)
LCA.GCW  <- LCA.byEvents(events_input.GCW,  maintenance.GCW,  LCI.materials)
LCA.BRC  <- LCA.byEvents(events_input.BRC,  maintenance.BRC,  LCI.materials)
LCA.STB  <- LCA.byEvents(events_input.STB,  maintenance.STB,  LCI.materials)
LCA.RLWC <- LCA.byEvents(events_input.RLWC, maintenance.RLWC, LCI.materials)
LCA.RLWT <- LCA.byEvents(events_input.RLWT, maintenance.RLWT, LCI.materials)

# Quick view for debug, you can comment it
LCA.PCF
LCA.GCW
LCA.BRC
LCA.STB
LCA.RLWC
LCA.RLWT


integrated.Design <- as.data.frame(list(
  Energy = LCA.PCF$Energy  + LCA.GCW$Energy  + LCA.BRC$Energy  + LCA.STB$Energy  + LCA.RLWC$Energy  + LCA.RLWT$Energy,
  CO2    = LCA.PCF$CO2     + LCA.GCW$CO2     + LCA.BRC$CO2     + LCA.STB$CO2     + LCA.RLWC$CO2     + LCA.RLWT$CO2,
  NOx    = LCA.PCF$NOx     + LCA.GCW$NOx     + LCA.BRC$NOx     + LCA.STB$NOx     + LCA.RLWC$NOx     + LCA.RLWT$NOx,
  Cost   = LCA.PCF$Cost    + LCA.GCW$Cost    + LCA.BRC$Cost    + LCA.STB$Cost    + LCA.RLWC$Cost    + LCA.RLWT$Cost
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

