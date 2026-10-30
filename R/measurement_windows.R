# Functions for chamber measurement windows: checking them against the sheet
# and the Picarro log, grouping them into measurement days, summarising logged
# series over them, and drawing the logging timeline.
# Windows are rows of metadata_joined (02_join_metadata.R), with PotID, Date,
# Order_Index, Condition, Chamber_Start_DateTime and Chamber_End_DateTime.

window_tz <- "America/Los_Angeles"

#' Chamber windows with missing or implausible times
#'
#' Usually typos in the sheet (e.g. an end time before the start time). No
#' readings can fall inside such a window, so it would drop out silently.
#'
#' @param windows Data frame of chamber windows.
#' @param min_minutes,max_minutes Numeric. Plausible window length (closures are ~10 min).
#' @return Data frame of the flagged windows: PotID, Date, Condition, Start,
#'   End (hh:mm:ss text) and Duration_min.
check_window_durations <- function(windows, min_minutes = 8, max_minutes = 13) {
  windows %>%
    mutate(Duration_min = round(as.numeric(Chamber_End_DateTime - Chamber_Start_DateTime, units = "mins"), 1)) %>%
    filter(is.na(Duration_min) | Duration_min < min_minutes | Duration_min > max_minutes) %>%
    transmute(PotID, Date, Condition,
              Start = format(Chamber_Start_DateTime, "%H:%M:%S"),
              End = format(Chamber_End_DateTime, "%H:%M:%S"),
              Duration_min) %>%
    arrange(Date, PotID)
}

#' Valid chamber windows that matched no Picarro readings
#'
#' For example when the analyser was not logging (2026-04-29, 14:24-17:17).
#'
#' @param windows Data frame of chamber windows.
#' @param joined Picarro readings joined to windows (\code{picarro_data_joined}).
#' @return Data frame of PotID, Date, Condition, Start and End (hh:mm:ss text).
find_windows_without_data <- function(windows, joined) {
  windows %>%
    filter(!is.na(Chamber_Start_DateTime), Chamber_End_DateTime > Chamber_Start_DateTime) %>%
    anti_join(distinct(joined, PotID, Order_Index, Condition),
              by = c("PotID", "Order_Index", "Condition")) %>%
    transmute(PotID, Date, Condition,
              Start = format(Chamber_Start_DateTime, "%H:%M:%S"),
              End = format(Chamber_End_DateTime, "%H:%M:%S")) %>%
    arrange(Date, Start)
}

#' One row per measurement day
#'
#' Each occasion spans two days: blocks A-B on day 1, C-D on day 2.
#'
#' @param windows Data frame of chamber windows.
#' @return Data frame of Order_Index, Date, start and end (first chamber
#'   closure to last), Day (1 or 2) and Session label.
measurement_days <- function(windows) {
  windows %>%
    group_by(Order_Index, Date) %>%
    summarise(start = min(Chamber_Start_DateTime), end = max(Chamber_End_DateTime), .groups = "drop") %>%
    group_by(Order_Index) %>%
    mutate(Day = if_else(Date == min(Date), 1L, 2L),
           Session = if_else(Day == 1L, "Day 1 (blocks A-B)", "Day 2 (blocks C-D)")) %>%
    ungroup() %>%
    arrange(Date)
}

#' Summarise a logged time series over time windows
#'
#' Two methods, chosen with \code{method}:
#' \describe{
#'   \item{\code{"readings"} (default)}{Mean, median and SD of the raw readings
#'     inside the window; if there are none, of the \code{n_closest} readings
#'     nearest to the window (distance to the nearer window edge). The SD is
#'     \code{NA} when only one reading is used.}
#'   \item{\code{"interpolate"}}{The series is linearly interpolated to regular
#'     steps from the window start to its end (inclusive), and the mean, median
#'     and SD are taken over those values, so every part of the window counts
#'     equally. The readings just before and after the window set its edges.}
#' }
#' Either way, windows not fully inside the logged period get \code{NA}; a
#' logger recording every ~15 min usually has zero or one reading inside a
#' 10-min chamber window.
#'
#' @param log Data frame with columns \code{time} (POSIXct) and \code{value}
#'   (numeric), e.g. one greenhouse variable.
#' @param windows Data frame with one row per window.
#' @param start,end Character. Names of the POSIXct start and end columns in
#'   \code{windows}.
#' @param prefix Character or \code{NULL}. Prefix for the new columns, e.g.
#'   \code{"Temp"} gives \code{Temp_mean}, \code{Temp_median}, ...
#' @param method Character. \code{"readings"} (default) or \code{"interpolate"}.
#' @param step_s Numeric. Interpolation step in seconds (\code{"interpolate"}).
#' @param n_closest Integer. Readings used when none fall inside the window
#'   (\code{"readings"}).
#'
#' @return \code{windows} with added columns (prefixed by \code{prefix}):
#'   \code{mean}, \code{median}, \code{sd}; \code{n_readings}, the raw readings
#'   inside the window; \code{basis}, how the values were obtained
#'   ("interpolated", "inside window" or "closest <n>"); and
#'   \code{max_offset_min}, how far outside the window the farthest reading
#'   used lies (0 when all are inside; \code{NA} for "interpolated").
summarise_in_windows <- function(log, windows, start = "Chamber_Start_DateTime",
                                 end = "Chamber_End_DateTime", prefix = NULL,
                                 method = c("readings", "interpolate"),
                                 step_s = 60, n_closest = 2) {
  method <- match.arg(method)
  log <- log[!is.na(log$time) & !is.na(log$value), ]
  log <- log[order(log$time), ]
  log_t <- as.numeric(log$time)
  log_range <- range(log_t)

  empty <- list(mean = NA_real_, median = NA_real_, sd = NA_real_, n_readings = NA_integer_,
                basis = NA_character_, max_offset_min = NA_real_)

  window_stats <- function(s, e) {
    if (is.na(s) || is.na(e) || e < s || s < log_range[1] || e > log_range[2]) return(empty)
    inside <- log_t >= s & log_t <= e
    n <- sum(inside)

    if (method == "interpolate") {
      grid <- unique(c(seq(s, e, by = step_s), e))
      v <- stats::approx(log_t, log$value, xout = grid, ties = mean)$y
      return(list(mean = mean(v), median = stats::median(v), sd = stats::sd(v), n_readings = n,
                  basis = "interpolated", max_offset_min = NA_real_))
    }

    if (n > 0) {
      used <- which(inside)
      basis <- "inside window"
    } else {
      # distance of each reading to the nearer window edge
      dist <- pmax(s - log_t, log_t - e, 0)
      used <- order(dist)[seq_len(min(n_closest, length(dist)))]
      basis <- paste("closest", length(used))
    }
    v <- log$value[used]
    offset <- max(pmax(s - log_t[used], log_t[used] - e, 0)) / 60
    list(mean = mean(v), median = stats::median(v), sd = if (length(v) > 1) stats::sd(v) else NA_real_,
         n_readings = n, basis = basis, max_offset_min = offset)
  }

  stats <- do.call(rbind, lapply(
    Map(window_stats, as.numeric(windows[[start]]), as.numeric(windows[[end]])),
    as.data.frame
  ))
  if (!is.null(prefix)) names(stats) <- paste(prefix, names(stats), sep = "_")
  cbind(windows, stats)
}

#' Continuous Picarro logging segments
#'
#' Splits the raw log into segments wherever the source file changes or
#' readings stop for more than \code{max_gap_s}.
#'
#' @param picarro_data Raw Picarro readings with \code{timestamp} and
#'   \code{source_file} (01_load_data.R).
#' @param days Date vector or \code{NULL}. Keep only these days.
#' @param max_gap_s Numeric. Longest pause (s) within one segment.
#' @return Data frame of Date, source_file, start, end, n (readings), rate
#'   (readings/s) and Sampling (rate band).
picarro_log_segments <- function(picarro_data, days = NULL, max_gap_s = 60) {
  picarro_data %>%
    mutate(Date = as.Date(timestamp, tz = window_tz)) %>%
    filter(is.null(days) | Date %in% days) %>%
    arrange(timestamp) %>%
    mutate(gap = as.numeric(timestamp - lag(timestamp), units = "secs"),
           seg = cumsum(is.na(gap) | gap > max_gap_s | source_file != lag(source_file))) %>%
    group_by(Date, seg, source_file) %>%
    summarise(start = min(timestamp), end = max(timestamp), n = n(), .groups = "drop") %>%
    mutate(rate = n / pmax(as.numeric(end - start, units = "secs"), 1),
           Sampling = if_else(rate < 1, "≈0.8 readings/s", "≈1.2 readings/s")) %>%
    select(-seg)
}

#' Gaps between Picarro logging segments within a day
#'
#' @param segments Output of \code{picarro_log_segments()}.
#' @param min_gap_min Numeric. Report gaps longer than this (minutes).
#' @return Data frame of Date, gap_start, gap_end and gap_min.
picarro_logging_gaps <- function(segments, min_gap_min = 10) {
  segments %>%
    arrange(start) %>%
    group_by(Date) %>%
    mutate(gap_start = lag(end), gap_min = as.numeric(start - lag(end), units = "mins")) %>%
    ungroup() %>%
    filter(!is.na(gap_min), gap_min > min_gap_min) %>%
    transmute(Date, gap_start, gap_end = start, gap_min)
}

# Treatment colours for the timeline: six well-separated hues (Okabe-Ito based,
# colour-blind safe); the conditions and the Picarro log use greys and tans
timeline_treatment_colors <- c(
  "Conventional soil" = "#D55E00",
  "Organic soil"      = "#8C510A",
  "Biochar"           = "#F0E442",
  "Biochar+SMT"       = "#009E73",
  "Biochar+MPSS"      = "#0072B2",
  "Biochar+MPSS+SMT"  = "#CC79A7"
)

#' Timeline of Picarro logging and chamber windows per measurement day
#'
#' One row per measurement day, top to bottom: Picarro log segments (fill by
#' sampling rate, white edges at file boundaries), chamber windows coloured by
#' treatment with the block letter inside, and a thin bar under each window for
#' its condition (Light, Dark, After watering). Logging gaps
#' are outlined in red; optionally the times of another logger (e.g. the
#' greenhouse CSV) are drawn as red lines, kept only where they overlap Picarro
#' logging or a chamber window.
#'
#' @param windows Data frame of chamber windows, with Treatment and Block.
#' @param segments Output of \code{picarro_log_segments()}.
#' @param logger_times Data frame with \code{time} (POSIXct) and \code{Logged}
#'   (character category, drawn as line type), or \code{NULL}.
#' @param logger_name Character. Legend title for \code{logger_times}.
#' @param treatment_colors Named character vector. Colour per treatment, in
#'   legend order. Block letters are drawn in black or white, whichever
#'   contrasts more with the fill.
#' @param hours Numeric length 2. Time-of-day range shown (hours).
#' @param row_fill Numeric. Share of each row used by the bars; the rest is the
#'   space between days.
#' @return A ggplot object.
plot_window_timeline <- function(windows, segments, logger_times = NULL,
                                 logger_name = "Greenhouse log time",
                                 treatment_colors = timeline_treatment_colors,
                                 hours = c(7.5, 18), row_fill = 0.78) {
  tod <- function(t) { lt <- as.POSIXlt(t, tz = window_tz); lt$hour + lt$min / 60 + lt$sec / 3600 }
  days <- measurement_days(windows)
  day_levels <- days$Date
  day_labels <- sprintf("%s  occ %d · day %d", format(days$Date, "%b %d"), days$Order_Index, days$Day)
  # vertical position within a row: 0 = top of the row band, 1 = bottom (y axis is reversed)
  yi <- function(d) match(d, day_levels)
  yb <- function(d, f) yi(d) - row_fill / 2 + f * row_fill

  # block letters in black or white, whichever contrasts more with the treatment colour
  luminance <- colSums(grDevices::col2rgb(treatment_colors) / 255 * c(0.299, 0.587, 0.114))
  label_colors <- setNames(ifelse(luminance > 0.5, "black", "white"), names(treatment_colors))
  windows <- windows %>%
    mutate(Condition = factor(Condition, levels = c("Light", "Dark", "After_Watering")),
           Treatment = factor(Treatment, levels = names(treatment_colors)),
           label_color = unname(label_colors[as.character(Treatment)]))
  segments <- segments %>% filter(Date %in% day_levels)
  gaps <- picarro_logging_gaps(segments) %>% mutate(inside = gap_min > 60)

  p <- ggplot2::ggplot() +
    # Picarro log segments
    ggplot2::geom_rect(data = segments,
                       ggplot2::aes(xmin = tod(start), xmax = tod(end), ymin = yb(Date, 0), ymax = yb(Date, 0.34),
                                    fill = Sampling),
                       color = "white", linewidth = 0.25) +
    ggplot2::scale_fill_manual(values = c("≈0.8 readings/s" = "#EFE6D2", "≈1.2 readings/s" = "#CDB99A"),
                               labels = c("≈0.8 readings/s", "≈1.2 readings/s"), name = "Picarro log",
                               guide = ggplot2::guide_legend(order = 1)) +
    ggnewscale::new_scale_fill() +
    # chamber windows coloured by treatment, block letter inside; white edges
    # separate back-to-back windows of one pot
    ggplot2::geom_rect(data = windows,
                       ggplot2::aes(xmin = tod(Chamber_Start_DateTime), xmax = tod(Chamber_End_DateTime),
                                    ymin = yb(Date, 0.40), ymax = yb(Date, 0.82), fill = Treatment),
                       color = "white", linewidth = 0.2) +
    ggplot2::geom_text(data = windows,
                       ggplot2::aes(x = (tod(Chamber_Start_DateTime) + tod(Chamber_End_DateTime)) / 2,
                                    y = yb(Date, 0.61), label = Block, color = I(label_color)),
                       size = 2.1, fontface = "bold") +
    ggplot2::scale_fill_manual(values = treatment_colors, drop = FALSE, name = "Treatment (letter = block)",
                               guide = ggplot2::guide_legend(order = 3, nrow = 1)) +
    ggnewscale::new_scale_fill() +
    # condition bar under each window
    ggplot2::geom_rect(data = windows,
                       ggplot2::aes(xmin = tod(Chamber_Start_DateTime), xmax = tod(Chamber_End_DateTime),
                                    ymin = yb(Date, 0.86), ymax = yb(Date, 1), fill = Condition),
                       color = "white", linewidth = 0.15) +
    ggplot2::scale_fill_manual(values = c("Light" = "#D0D0D0", "Dark" = "#1A1A1A", "After_Watering" = "#7A7A7A"),
                               labels = c("Light", "Dark", "After watering"), name = "Condition (bar under window)",
                               guide = ggplot2::guide_legend(order = 2))

  if (!is.null(logger_times)) {
    # logger times overlapping Picarro logging or a chamber window, full row height
    spans <- bind_rows(segments %>% select(start, end),
                       windows %>% transmute(start = Chamber_Start_DateTime, end = Chamber_End_DateTime))
    span_start <- as.numeric(spans$start)
    span_end <- as.numeric(spans$end)
    logger_times <- logger_times %>%
      mutate(Date = as.Date(time, tz = window_tz)) %>%
      filter(Date %in% day_levels) %>%
      filter(vapply(as.numeric(time), \(t) any(t >= span_start & t <= span_end), logical(1)))
    p <- p +
      ggplot2::geom_segment(data = logger_times,
                            ggplot2::aes(x = tod(time), xend = tod(time), y = yb(Date, -0.04), yend = yb(Date, 1.04),
                                         linetype = Logged),
                            color = "#B03A2E", linewidth = 0.35) +
      ggplot2::scale_linetype_manual(values = c("solid", "11"), name = logger_name) +
      ggplot2::guides(linetype = ggplot2::guide_legend(order = 4, override.aes = list(linewidth = 0.8)))
  }

  p +
    ggplot2::geom_rect(data = gaps,
                       ggplot2::aes(xmin = tod(gap_start), xmax = tod(gap_end), ymin = yb(Date, -0.08), ymax = yb(Date, 1.08)),
                       fill = NA, color = "#C0392B", linewidth = 0.6, linetype = "22") +
    # long gaps: label inside the gap; short gaps: label just after it
    ggplot2::geom_label(data = gaps,
                        ggplot2::aes(x = if_else(inside, (tod(gap_start) + tod(gap_end)) / 2, tod(gap_end)),
                                     y = yb(Date, 0.17), hjust = if_else(inside, 0.5, -0.03),
                                     label = sprintf("no Picarro data (%.0f min)", gap_min)),
                        size = 2.5, color = "#C0392B", fill = "white", label.size = 0,
                        label.padding = grid::unit(0.08, "lines")) +
    ggplot2::scale_y_reverse(breaks = seq_along(day_levels), labels = day_labels, expand = ggplot2::expansion(add = 0.6)) +
    ggplot2::scale_x_continuous(breaks = ceiling(hours[1]):floor(hours[2]),
                                labels = sprintf("%02d:00", ceiling(hours[1]):floor(hours[2])),
                                limits = hours, expand = c(0, 0)) +
    ggplot2::labs(title = "Picarro logging and chamber measurement windows",
                  subtitle = paste0("One row per measurement day. Top: Picarro log (white edges = hourly files). ",
                                    "Middle: chamber windows by treatment (letter = block). Bottom: condition.",
                                    if (!is.null(logger_times)) paste0(" Red lines: ", tolower(logger_name), "s.")),
                  x = "Time of day (Pacific)", y = NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top", legend.box = "vertical", legend.justification = "left",
                   legend.text = ggplot2::element_text(size = 8),
                   panel.grid.minor = ggplot2::element_blank(), panel.grid.major.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size = 8, family = "mono"))
}
