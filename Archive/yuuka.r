rm (list = ls())

library(timelineS) 
library(lubridate) 
library(ggplot2) 
library(dplyr) 
library(reshape2) 
library(rPref) 
library(mco) 
library(MASS)


# =========================
# P8-P14内容
# 把可以整理两个subsystem的函数升级为可以整理很多个的
# 不过也是chatgpt老师写的，看不明白，得了一种看到嵌套循环就头疼的病
# =========================
# 前三个工具函数
# =========================
# 把一组“事件发生在第几年”转为真实日期，然后画一条时间轴
# This function convert a set of EventTime into actual dates And then draw a timeline
# (I think it's better using EventTime here)
plot.timeline <- function(EventTime, events.name, start.Date, plot.Name) {
  dataP <- data.frame(
    Events = events.name,
    Event_Dates = ymd(start.Date) + years(EventTime)
  )
  timelineS(
    dataP,
    main = plot.Name,
    labels = events.name,
    label.direction = "up",
    label.position = 3
  )
}

# 整个生命周期中，每一年到底发生什么事件
# This function generates the life-cycle timeline of maintenance events
# by expanding intervention intervals into a sorted sequence of event years.
dist.Events <- function (lifetime, events, start.Date, option.Name, do.plot = FALSE) {
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


# 将六个系统的维护时间线合并，并计算停用时长
# Combine maintenance timelines for N subsystems
combine.lifeTimelinesN <- function(timelines, durs, rule = c("max", "sum")) {
  # timelines: list of dist.Events outputs (each has $frequency and $event)
  # durs:      list of named numeric vectors, mapping event name -> duration (days)
  # rule:      how to combine simultaneous downtimes ("max" to assume bundling)
  rule <- match.arg(rule)
  
  if (!is.list(timelines) || !is.list(durs)) {
    stop("timelines and durs must both be lists.")
  }
  if (length(timelines) != length(durs)) {
    stop("timelines and durs must have the same length.")
  }
  
  n <- length(timelines) # n = 6
  
  # Collect all unique years across subsystems
  all_years <- sort(unique(unlist(lapply(timelines, function(tl) tl$frequency))), decreasing = FALSE)
  
  base <- list(
    frequency = all_years, 
    Names = rep("", length(all_years)), 
    duration = rep(0, length(all_years)) 
  )
  
  # Optional: enforce start/end labels if years include 0 and lifetime
  base$Names[base$frequency == 0] <- "DC"
  
  # Loop over each year 开始一些烧脑的循环嵌套，说实话我没看懂… 能跑就行吧
  for (k in seq_along(all_years)) {
    yr <- all_years[k]
    
    events_this_year <- c()
    durations_this_year <- c()
    
    for (s in seq_len(n)) {
      tl <- timelines[[s]]
      dur_map <- durs[[s]]
      
      ev <- tl$event[tl$frequency == yr]
      if (length(ev) == 0) next
      
      # In your dist.Events, each year is unique, so length(ev) is usually 1
      for (e in ev) {
        events_this_year <- c(events_this_year, e)
        
        # Look up duration; if missing, treat as 0 and warn once
        d <- dur_map[names(dur_map) == e]
        if (length(d) == 0) {
          d <- 0
          # You can comment this out if too noisy
          warning(sprintf("No duration found for event '%s' (subsystem %d). Using 0.", e, s))
        }
        durations_this_year <- c(durations_this_year, as.numeric(d))
      }
    }
    
    # Fill outputs for this year
    if (length(events_this_year) > 0) {
      base$Names[k] <- paste(events_this_year, collapse = " + ")
      base$duration[k] <- if (rule == "max") max(durations_this_year) else sum(durations_this_year)
    } else {
      base$Names[k] <- ""
      base$duration[k] <- 0
    }
  }
  
  # Put END label if the max year is lifetime (optional)
  base$Names[base$frequency == max(base$frequency)] <- "END"
  
  return(base)
}


# =========================
# 输入event.input的数据
# =========================
# par(mfrow = c(2, 1))
start.Date <- "2026-01-01"
lifetime <- 120  # years

# input 六个系统的输入，我把building的三个合并了
# 我们没有系统有很多方案，所以我把option相关删掉了。不过另外两条railway好像作为两个不同方案更合适。
# --- Precast Concrete Facade (.pcf) ---
events.pcf <- c(
  CR.pcf = 15,   # Coating Refresh
  JM.pcf = 20,   # Joint Maintenance
  PR.pcf = 60,   # Panel Replacement
  END = lifetime
)

duration.pcf <- c(
  CR.pcf = 3,
  JM.pcf = 5,
  PR.pcf = 10
)

maintenance.pcf <- dist.Events(
  lifetime,
  events.pcf,
  start.Date,
  option.Name = "Precast Concrete Facade (.pcf)"
)

# --- Glass Curtain Wall (.gcw) ---
events.gcw <- c(
  DC.gcw = 7,    # Deep cleaning
  GR.gcw = 10,   # Gasket replacement
  IR.gcw = 40,   # IGU replacement
  END = lifetime
)

duration.gcw <- c(
  DC.gcw = 3,
  GR.gcw = 5,
  IR.gcw = 14
)

maintenance.gcw <- dist.Events(
  lifetime,
  events.gcw,
  start.Date,
  option.Name = "GCW (.gcw)"
)

# ---Building(.brc) ---
events.brc <- c(
  SR.brcs    = 60,  # Spall Repair
  CSJR.brcs  = 25,  # Crack Sealing / Joint Refurbishment
  CT.brcp = 20,   # Carbonation Treatment
  SG.brcf = 40,   # Structural Grounding
  END = lifetime
)

duration.brc <- c(
  SR.brcs   = 21,
  CSJR.brcs = 7,
  CT.brcp = 5,
  SG.brcf = 41
)

maintenance.brc <- dist.Events(
  lifetime,
  events.brc,
  start.Date,
  option.Name = "Building(.brc)"
)

# ---Steel Truss Bridge(.stb)---
events.stb <- c(
  RM.stb = 5,     # Routine Maintenance
  MP.stb = 15,    # Member Replacement
  FR.stb = 25,    # Full Recoating
  BR.stb = 2,     # Bearing Replacement
  SM.stb = 1,     # Staircase maintenance
  END = lifetime
)

duration.stb <- c(
  RM.stb = 2,
  MP.stb = 7,
  FR.stb = 21,
  BR.stb = 1,     
  SM.stb = 0
)

maintenance.stb <- dist.Events(
  lifetime,
  events.stb,
  start.Date,
  option.Name = "Steel Truss Bridge (.stb)"
)

# ---Railway Track Concrete sleepers(.rlwc)---
events.rlwc <- c(
  SLR.rlw_slp = 1,
  SSRC.rlw_slp = 50,
  RG.rlw_rls = 1,
  RM.rlw_rls = 3,
  LRR.rlw_rls = 1,
  FRR.rlw_rls = 80,
  ROP.rlw_plt = 80,
  END = lifetime
)

duration.rlwc <- c(
  SLR.rlw_slp = 0,
  SSRC.rlw_slp = 5,
  RG.rlw_rls = 0,
  RM.rlw_rls = 0,
  LRR.rlw_rls = 0,
  FRR.rlw_rls = 1.25,
  ROP.rlw_plt = 0   
)

maintenance.rlwc <- dist.Events(
  lifetime,
  events.rlwc,
  start.Date,
  option.Name = "Railway Track Concrete sleepers(.rlwc)"
)

# ---Railway Track timber sleepers(.rlwt)---
events.rlwt <- c(
  rlwt_RP.rep = 10,
  rlwt_RP.loc = 5,
  rlwt_TS.full = 20,
  rlwt_TS.rep = 10,
  rlwt_B.clean = 12,
  rlwt_SB.geo = 100,
  END = lifetime
)

duration.rlwt <- c(
  rlwt_RP.rep = 1,
  rlwt_RP.loc = 0,
  rlwt_TS.full = 1,
  rlwt_TS.rep = 0,
  rlwt_B.clean = 0,
  rlwt_SB.geo = 14
)

maintenance.rlwt <- dist.Events(
  lifetime,
  events.rlwt,
  start.Date,
  option.Name = "Railway Track timber sleepers(.rlwt)"
)



# =========================
# 计算total.downtime
# =========================
timelines <- list(
  maintenance.pcf,
  maintenance.gcw,
  maintenance.brc,
  maintenance.stb,
  maintenance.rlwc,
  maintenance.rlwt
)

durs <- list(
  duration.pcf,
  duration.gcw,
  duration.brc,
  duration.stb,
  duration.rlwc,
  duration.rlwt
)

integrated<- combine.lifeTimelinesN(timelines, durs, rule="max")

total.downtime<-sum(integrated$duration)

total.downtime # output: 340days

# =========================
# design.exploreN函数
# =========================
design.exploreN <- function(
    events_grids,   # list of data.frames, one per subsystem
    dur_list,       # list of named numeric vectors
    major_list,     # list of character vectors (major event names)
    name_list,      # character vector (labels)
    lifetime,
    start.Date,
    rule = "max"    # passed to combine.lifeTimelinesN
) {
  # --- checks ---
  stopifnot(is.list(events_grids), is.list(dur_list), is.list(major_list))
  n <- length(events_grids)
  if (length(dur_list) != n || length(major_list) != n || length(name_list) != n) {
    stop("events_grids, dur_list, major_list, name_list must have the same length.")
  }
  
  # Number of scenarios per subsystem
  nrows <- sapply(events_grids, nrow)
  
  # Cartesian product of scenario indices: all combinations across subsystems
  idx_grid <- expand.grid(lapply(nrows, function(m) seq_len(m)))
  colnames(idx_grid) <- paste0("sys", seq_len(n))
  
  results <- vector("list", nrow(idx_grid))
  
  # Pre-build a single regex for "major events"
  major_patterns <- unique(unlist(major_list))
  major_regex <- paste(major_patterns, collapse = "|")
  
  for (r in seq_len(nrow(idx_grid))) {
    
    # --- build timelines for each subsystem under this combination ---
    timelines <- vector("list", n)
    chosen_params <- list()  # for output: record chosen event frequencies
    
    for (s in seq_len(n)) {
      grid_s <- events_grids[[s]]
      row_id <- idx_grid[r, s]
      
      ev <- as.numeric(unlist(grid_s[row_id, , drop = FALSE]))
      names(ev) <- colnames(grid_s)
      
      # Always add END
      ev <- c(ev, END = lifetime)
      
      timelines[[s]] <- dist.Events(lifetime, ev, start.Date, option.Name = name_list[s], do.plot = FALSE)
      
      chosen_params[[s]] <- ev[names(ev) != "END"]  # keep only "real" events for output
    }
    
    # --- combine to system level ---
    combined <- combine.lifeTimelinesN(timelines, dur_list, rule = rule)
    
    dur_total <- sum(combined$duration)
    
    # dist.inter: only between major interventions (system-level)
    freq_major <- combined$frequency[
      sapply(combined$Names, function(x) grepl(major_regex, x))
    ]
    dist.inter <- if (length(freq_major) >= 2) min(diff(freq_major)) else NA_real_
    
    # --- output row ---
    flat_params <- unlist(chosen_params)
    results[[r]] <- c(flat_params, dur = dur_total, dist.inter = dist.inter)
  }
  
  out <- as.data.frame(do.call(rbind, results))
  # Ensure numeric outputs
  out$dur <- as.numeric(out$dur)
  out$dist.inter <- as.numeric(out$dist.inter)
  return(out)
}

# =========================
# 输入events.grid
# =========================
# 我的电脑也跑不动太多方案，所以删了一下，只留下了major event
# 我觉得这些major event是比较重要的，可以改
n.grid <- 2
# PCF scenarios
events.grid.pcf <- expand.grid(
  PR.pcf = sample(seq(40, 60, by = 1), n.grid, replace = TRUE)
)

# GCW scenarios
events.grid.gcw <- expand.grid(
  IR.gcw = sample(seq(40, 60, by = 1), n.grid, replace = TRUE)
)

# BRC scenarios
events.grid.brc <- expand.grid(
  SR.brcs   = sample(seq(50, 70, by = 5), n.grid, replace = TRUE),
  SG.brcf   = sample(seq(30, 50, by = 5), n.grid, replace = TRUE)
)

# STB scenarios
events.grid.stb <- expand.grid(
  MP.stb = sample(seq(10, 20, by = 1),  n.grid, replace = TRUE),
  FR.stb = sample(seq(20, 35, by = 1), n.grid, replace = TRUE)
)

# RLWC scenarios
events.grid.rlwc <- expand.grid(
  SSRC.rlw_slp = sample(seq(40, 60, by = 5), n.grid, replace = TRUE),
  FRR.rlw_rls  = sample(seq(60, 100, by = 10), n.grid, replace = TRUE)
)

# RLWT scenarios
events.grid.rlwt <- expand.grid(
  rlwt_RP.rep  = sample(seq(8, 15, by = 1), n.grid, replace = TRUE),
  rlwt_TS.full = sample(seq(15, 30, by = 5), n.grid, replace = TRUE),
  rlwt_SB.geo  = sample(seq(80, 120, by = 10), n.grid, replace = TRUE)
)

# =========================
# 输出最优解
# =========================
# 打包成 design.exploreN 需要的四个 list
events_grids <- list(
  events.grid.pcf,
  events.grid.gcw,
  events.grid.brc,
  events.grid.stb,
  events.grid.rlwc,
  events.grid.rlwt
)

dur_list <- list(
  duration.pcf,
  duration.gcw,
  duration.brc,
  duration.stb,
  duration.rlwc,
  duration.rlwt
)

name_list <- c("pcf","gcw","brc","stb","rlwc","rlwt")


# 定义 major events(要不然event太多了，后面的图没有很大意义)

major_list <- list(
  c("PR.pcf"),                         # facade: panel replacement
  c("IR.gcw"),                         # curtain wall: IGU replacement
  c("SR.brcs", "SG.brcf"),             # building: spall repair + structural grounding
  c("MP.stb", "FR.stb"),     # bridge: member replacement + full recoating
  c("SSRC.rlw_slp", "FRR.rlw_rls"),  # rlwc: campaigns / full renewals
  c("rlwt_RP.rep", "rlwt_TS.full", "rlwt_SB.geo")   # rlwt: full replacements
)


response.space <- design.exploreN(
  events_grids = events_grids,
  dur_list     = dur_list,
  major_list   = major_list,
  name_list    = name_list,
  lifetime     = lifetime,
  start.Date   = start.Date,
  rule         = "max"
)

# 确保数值列是 numeric
response.space$dur <- as.numeric(response.space$dur)
response.space$dist.inter <- as.numeric(response.space$dist.inter)

p <- low(dur) * high(dist.inter)

sky <- psel(response.space, p)
sky

# grid = 3时的输出结果（我跑了很久）：
# PR.pcf IR.gcw SR.brcs SG.brcf MP.stb FR.stb SSRC.rlw_slp
# 65967      44     40      60      40     20     20           60
# 85650      44     40      60      40     20     20           60
# 105333     44     40      60      40     20     20           60
# FRR.rlw_rls rlwt_RP.rep rlwt_TS.full rlwt_SB.geo dur dist.inter
# 65967           80          12           20          80 145          4
# 85650           80          12           20          80 145          4
# 105333          80          12           20          80 145          4

# Rank all alternatives
pareto2 <- psel(response.space, p, top = nrow(response.space))

# Visualize ranking levels
ggplot(response.space, aes(x = dur, y = dist.inter)) +
  geom_point(shape = 21) +
  geom_point(
    data = pareto2,
    size = 3,
    aes(color = factor(.level))
  ) +
  theme(legend.position = "none")



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


# =========================
# P15-P16 导入材料数据，得出LCA.results，并把环境影响折算为cost
# 我觉得tutorial上的方法很粗糙，通过输入桥的参数以确定每次维护的数据的方式也不适合我们
# 我还对我们的数据单位有点疑惑。我的理解是EVENTS_INPUT是每次维护消耗材料类型+消耗材料数量
# LCI_INPUT和LCI connection是每个材料对应的LCI数据
# 我对scope的理解是它是一个组，当一次维护涉及到的部件涉及多个材料时，那么这个部件是scope，要在function中找到对应材料
# 然后用每次维护事件消耗材料*维护次数*LCI数据来得到LCA.results
# =========================
# 导入数据
LCI.materials <- read.csv(file.choose(), stringsAsFactors = FALSE)
events_input <- read.csv(file.choose(), stringsAsFactors = FALSE)
# check
setdiff(events_input$material, LCI.materials$material)

# 相当于教程中的LCA.bridge，但是魔改版
LCA.byEvents <- function(events_input, timeline, LCI_materials) {
  
  # 1) 每个 event 在生命周期出现次数
  event_freq <- as.data.frame(table(timeline$event), stringsAsFactors = FALSE)
  colnames(event_freq) <- c("event_code", "n_occurrences")
  event_freq <- subset(event_freq, !(event_code %in% c("DC", "END")))
  
  # 2) 把次数 merge 到 events_input
  df <- merge(events_input, event_freq, by = "event_code", all.x = TRUE)
  df$n_occurrences[is.na(df$n_occurrences)] <- 0  # timeline里没出现的事件 → 0次
  
  # 3) 做一个“material 或 scope 都能匹配”的 LCI 映射表
  LCI_keyed <- rbind(
    transform(LCI_materials, join_key = material),
    transform(LCI_materials, join_key = scope)
  )
  LCI_keyed <- LCI_keyed[!is.na(LCI_keyed$join_key) & LCI_keyed$join_key != "", ]
  
  # 4) 用 events_input$material 去匹配 LCI_keyed$join_key
  df$join_key <- df$material
  df2 <- merge(df, LCI_keyed, by = "join_key", all.x = TRUE)
  
  # 5) 检查有没有没匹配上的（非常重要）
  if (any(is.na(df2$energy))) {
    missing <- unique(df2$join_key[is.na(df2$energy)])
    stop(paste("These event_input.material values did not match any LCI material/scope:", 
               paste(missing, collapse = ", ")))
  }
  
  # 6) 生命周期材料用量
  # amount_per_event * n_occurrences 是“事件层面的功能单位用量”
  # 再乘 quantities 是“把功能单位换成 kg(或你LCI单位的基准) 的换算系数”
  df2$total_material <- df2$quantities * df2$amount_per_event * df2$n_occurrences
  
  # 7) 生命周期影响
  df2$Energy_LC <- df2$total_material * df2$energy
  df2$CO2_LC    <- df2$total_material * df2$CO2
  df2$NOx_LC    <- df2$total_material * df2$NOx
  df2$Cost_LC   <- df2$total_material * df2$Cost
  
  list(
    Energy = sum(df2$Energy_LC),
    CO2    = sum(df2$CO2_LC),
    NOx    = sum(df2$NOx_LC),
    Cost   = sum(df2$Cost_LC),
    detail = df2   # 方便你debug：每个事件×材料的贡献
  )
}


# stb和rlwt的LCA数据，其他subsystem的待加入
events_input.stb  <- subset(events_input, system == "stb")
LCI.stb           <- subset(LCI.materials, system == "stb")

events_input.rlwt <- subset(events_input, system == "rlwt")
LCI.rlwt          <- subset(LCI.materials, system == "rlwt")

LCA.stb  <- LCA.byEvents(events_input.stb,  maintenance.stb,  LCI.stb)
LCA.rlwt <- LCA.byEvents(events_input.rlwt, maintenance.rlwt, LCI.rlwt)

integrated.Design <- as.data.frame(list(
  Energy = LCA.stb$Energy + LCA.rlwt$Energy,
  CO2    = LCA.stb$CO2    + LCA.rlwt$CO2,
  NOx    = LCA.stb$NOx    + LCA.rlwt$NOx,
  Cost   = LCA.stb$Cost   + LCA.rlwt$Cost
))
integrated.Design


# 把environmental impact换算为cost
Energy.costs <- 0.128
CO2.unitcost <- 26
NOx.unitCost <- 42

integrated.Design <- mutate(
  integrated.Design,
  TotalCosts = Energy * Energy.costs +
    (CO2/1000) * CO2.unitcost +
    (NOx/1000) * NOx.unitCost + 
    Cost
)

integrated.Design
# =========================
# P18-P22, Multi-objective optimization
# 通过GA，让z[1]-z[7]越小越好
# bridge width这个变量不太适合我们，连我的系统也不欢迎它
# =========================

# 在GA中可调整的参数，先用major list
param_cols_list <- list(
  pcf  = c("PR.pcf"),
  gcw  = c("IR.gcw"),
  brc  = c("SR.brcs", "SG.brcf"),
  stb  = c("MP.stb", "FR.stb"),
  rlwc = c("SSRC.rlw_slp", "FRR.rlw_rls", "ROP.rlw_plt"),
  rlwt = c("rlwt_RP.rep", "rlwt_TS.full", "rlwt_SB.geo")
)
name_list <- names(param_cols_list)   # c("pcf","gcw","brc","stb","rlwc","rlwt")

# nsga2 会给你一个向量 x，我们要按顺序分配给每个系统的参数列
split_x_to_params <- function(x, param_cols_list) {
  x <- round(x, 0)
  out <- list()
  k <- 1
  for (sys in names(param_cols_list)) {
    cols <- param_cols_list[[sys]]
    out[[sys]] <- setNames(x[k:(k + length(cols) - 1)], cols)
    k <- k + length(cols)
  }
  out
}

# fitness函数
fitness <- function(x) {
  # 输出向量（nsga2 要 numeric）
  z <- numeric(6)
  
  # 1) 把 x 拆成各系统的事件频率参数
  params_by_sys <- split_x_to_params(x, param_cols_list)
  
  # 2) 为每个系统构造 events 向量 -> dist.Events
  timelines <- list()
  
  for (sys in name_list) {
    ev <- params_by_sys[[sys]]
    ev <- c(ev, END = lifetime)
    
    timelines[[sys]] <- dist.Events(
      lifetime   = lifetime,
      events     = ev,
      start.Date = start.Date,
      option.Name = sys,
      do.plot    = FALSE
    )
  }
  
  # 3) 合并 6 系统时间线 -> downtime
  combined <- combine.lifeTimelinesN(
    timelines = timelines,
    durs      = dur_list[name_list],
    rule      = "max"
  )
  
  z[1] <- sum(combined$duration)
  
  # 4) dist.inter（只看 major events）
  major_patterns <- unique(unlist(major_list))
  major_regex <- paste(major_patterns, collapse = "|")
  
  freq_major <- combined$frequency[
    grepl(major_regex, combined$Names)
  ]
  
  dist.inter <- if (length(freq_major) >= 2) min(diff(freq_major)) else 0
  z[2] <- -dist.inter  # 最大化 -> 取负
  
  # 5) LCA（每个系统单算，再加总）
  #    注意：LCA.byEvents 里会 table(timeline$event) 得到次数，所以 timeline 要传对应系统自己的 timeline
  lca_sum <- list(Energy = 0, CO2 = 0, NOx = 0, Cost = 0)
  
  for (sys in name_list) {
    ev_in  <- subset(events_input,  system == sys)
    lci_in <- subset(LCI.materials, system == sys)
    
    # 有些系统你可能还没填 events_input/LCI，就跳过（避免报错）
    if (nrow(ev_in) == 0 || nrow(lci_in) == 0) next
    
    lca_sys <- LCA.byEvents(
      events_input  = ev_in,
      timeline      = timelines[[sys]],
      LCI_materials = lci_in
    )
    
    lca_sum$Energy <- lca_sum$Energy + lca_sys$Energy
    lca_sum$CO2    <- lca_sum$CO2    + lca_sys$CO2
    lca_sum$NOx    <- lca_sum$NOx    + lca_sys$NOx
    lca_sum$Cost   <- lca_sum$Cost   + lca_sys$Cost
  }
  
  z[3] <- lca_sum$Energy
  z[4] <- lca_sum$CO2
  z[5] <- lca_sum$NOx
  z[6] <- lca_sum$Cost
  
  return(z)
}

# nsga2函数，遗传算法
idim <- sum(sapply(param_cols_list, length))
idim

r2 <- nsga2(
  fitness,
  idim = idim,
  odim = 6,
  generations = 5,
  popsize = 4,
  lower.bounds = c(
    # 按 param_cols_list 的顺序拼出来
    40,         # PR.pcf
    40,         # IR.gcw
    50, 30,     # SR.brcs, SG.brcf
    10, 20,     # MP.stb, FR.stb
    40, 60, 60, # SSRC, FRR, ROP
    8,  15, 80  # RP.rep, TS.full, SB.geo
  ),
  upper.bounds = c(
    60,
    60,
    70, 50,
    20, 35,
    60, 100, 100,
    15, 30, 120
  )
)
r2$par              # GA 给出的参数组合（x）
r2$value            # 对应的 fitness 输出 z
r2$pareto.optimal   # 是否属于 Pareto front

# 不行，这个跑出来的结果不对
# 受不了了，先睡觉吧，真是没招了


















