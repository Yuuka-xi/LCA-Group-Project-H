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


#---------------------------------------------------------------------------


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

# =========================
# Railway 1 (rlw_* events)
# =========================
events.rlw <- c(
  SSRC.rlw_slp = 50,
  RM.rlw_rls   = 3,
  FRR.rlw_rls  = 80,
  END = lifetime
)

dur.rlw <- c(
  SSRC.rlw_slp = 5,
  RM.rlw_rls   = 0,
  FRR.rlw_rls  = 1.25
)

# =========================
# Railway 2 (rlwt events)
# =========================
events.rlwt <- c(
  rlwt_TS.full = 20,
  rlwt_SB.geo  = 100,
  FRR.rlwt     = 80,
  END = lifetime
)

dur.rlwt <- c(
  rlwt_TS.full = 1,
  rlwt_SB.geo  = 14,
  FRR.rlwt     = 1.25
)

design.Options.rlwt <- "RLWT – Railway track (timber sleeper)"
design.Options.rlw <- "RLW – Railway track (rlw_*)"


# ====== 跑老師的步驟：dist.Events -> maintenance.* -> 印出來 ======
maintenance.rlw  <- dist.Events(lifetime, events.rlw,  start.Date, design.Options.rlw)
maintenance.rlwt <- dist.Events(lifetime, events.rlwt, start.Date, design.Options.rlwt)

maintenance.rlw
maintenance.rlwt

#------------------------
integrated.interv <- combine.lifeTimelines(
  maintenance.rlw,
  maintenance.rlwt,
  dur.rlw,
  dur.rlwt
)

integrated.interv

total.interruption <- sum(integrated.interv$duration)
total.interruption


###----
# Railway 1: RLW  (Updated to the NEW reduced event set)
# ---------------------------
events.Op1.RLW <- c(
  SSRC.rlw_slp = 49,
  RM.rlw_rls   = 3,
  FRR.rlw_rls  = 79,
  END = lifetime
)

duration.ev.RLW <- c(
  SSRC.rlw_slp = 5,
  RM.rlw_rls   = 0,
  FRR.rlw_rls  = 1.25
)

# ---------------------------
# Railway 2: RLWT (Updated to the NEW reduced event set)
# ---------------------------
events.Op1.RLWT <- c(
  rlwt_TS.full = 19,
  rlwt_SB.geo  = 97,
  FRR.rlwt     = 80,
  END = lifetime
)

duration.ev.RLWT <- c(
  rlwt_TS.full = 1,
  rlwt_SB.geo  = 14,
  FRR.rlwt     = 1.25
)

# design option names (for plot titles / reporting)
design.Options.RLW  <- "RLW – Railway Track (new set)"
design.Options.RLWT <- "RLWT – Railway Track (new set)"

# generate maintenance schedules
maintenance.RLW  <- dist.Events(lifetime, events.Op1.RLW,  start.Date, design.Options.RLW)
maintenance.RLWT <- dist.Events(lifetime, events.Op1.RLWT, start.Date, design.Options.RLWT)

# integrate two timelines
integrated.interv <- combine.lifeTimelines(
  maintenance.RLW,
  maintenance.RLWT,
  duration.ev.RLW,
  duration.ev.RLWT
)

# total interruption duration (days)
sum(integrated.interv$duration)


# =========================
# Explore ranges (same logic, fix ONLY wrong names)
# =========================
design.explore <- function(events1, events2) {
  results <- c()
  
  for(i in 1:dim(events1)[1]) {
    ev1 <- unlist(events1[i, ])
    dist.1 <- dist.Events(lifetime, ev1, start.Date, design.Options.RLW, do.plot = FALSE)
    dur.ev1 <- duration.ev.RLW
    
    for(j in 1:dim(events2)[1]) {
      ev2 <- unlist(events2[j, ])
      dist.2 <- dist.Events(lifetime, ev2, start.Date, design.Options.RLWT, do.plot = FALSE)
      dur.ev2 <- duration.ev.RLWT
      
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

#----------------------------------------------------------------------------------------


n.grid <- 5

events.grid.RLW <- expand.grid(
  SSRC.rlw_slp = sample(seq(40, 60, by = 1),  n.grid, replace = TRUE),
  RM.rlw_rls   = sample(seq(2, 5,  by = 1),  n.grid, replace = TRUE),
  FRR.rlw_rls  = sample(seq(60, 100, by = 1), n.grid, replace = TRUE)
)

events.grid.RLWT <- expand.grid(
  rlwt_TS.full = sample(seq(15, 30, by = 1),  n.grid, replace = TRUE),
  rlwt_SB.geo  = sample(seq(80, 120, by = 1), n.grid, replace = TRUE),
  FRR.rlwt     = sample(seq(60, 100, by = 1), n.grid, replace = TRUE)
)

response.space <- design.explore(events.grid.RLW, events.grid.RLWT)

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

# ===== helper：把單一策略變成兩條 timeline，並算重疊 =====
calc_overlap <- function(row) {
  ev_rlw  <- c(SSRC.rlw_slp = row[["SSRC.rlw_slp"]],
               RM.rlw_rls   = row[["RM.rlw_rls"]],
               FRR.rlw_rls  = row[["FRR.rlw_rls"]],
               END = lifetime)
  
  ev_rlwt <- c(rlwt_TS.full = row[["rlwt_TS.full"]],
               rlwt_SB.geo  = row[["rlwt_SB.geo"]],
               FRR.rlwt     = row[["FRR.rlwt"]],
               END = lifetime)
  
  m1 <- dist.Events(lifetime, ev_rlw,  start.Date, "RLW",  do.plot = FALSE)
  m2 <- dist.Events(lifetime, ev_rlwt, start.Date, "RLWT", do.plot = FALSE)
  
  # 只計算「有 duration 的事件」避免 0-day 事件干擾
  y1 <- m1$frequency[m1$event %in% names(dur.rlw)  & dur.rlw[m1$event]  > 0]
  y2 <- m2$frequency[m2$event %in% names(dur.rlwt) & dur.rlwt[m2$event] > 0]
  
  overlap_years <- intersect(y1, y2)
  overlap_count <- length(overlap_years)
  
  # gap：兩集合年份的最小差（避免空集合）
  if(length(y1)==0 || length(y2)==0) {
    min_gap <- NA
  } else {
    min_gap <- min(abs(outer(y1, y2, "-")))
  }
  
  c(overlap_count = overlap_count, min_gap = min_gap)
}

# ===== 對整個 response.space 算 overlap =====
tmp <- t(apply(response.space, 1, calc_overlap))
response.space$overlap_count <- tmp[, "overlap_count"]
response.space$min_gap <- tmp[, "min_gap"]

# ===== 挑出最錯開的策略（先 overlap=0，再 min_gap 最大，再 dur 最小）=====
candidates <- response.space[order(response.space$overlap_count,
                                   -response.space$min_gap,
                                   response.space$dur), ]

head(candidates, 10)

best <- candidates[1, ]

events.best.RLW <- c(
  SSRC.rlw_slp = best$SSRC.rlw_slp,
  RM.rlw_rls   = best$RM.rlw_rls,
  FRR.rlw_rls  = best$FRR.rlw_rls,
  END = lifetime
)

events.best.RLWT <- c(
  rlwt_TS.full = best$rlwt_TS.full,
  rlwt_SB.geo  = best$rlwt_SB.geo,
  FRR.rlwt     = best$FRR.rlwt,
  END = lifetime
)

maintenance.best.RLW  <- dist.Events(lifetime, events.best.RLW,  start.Date, "RLW BEST")
maintenance.best.RLWT <- dist.Events(lifetime, events.best.RLWT, start.Date, "RLWT BEST")

integrated.best.rail <- combine.lifeTimelines(
  maintenance.best.RLW,
  maintenance.best.RLWT,
  dur.rlw,
  dur.rlwt
)
integrated.best.rail
sum(integrated.best.rail$duration)



