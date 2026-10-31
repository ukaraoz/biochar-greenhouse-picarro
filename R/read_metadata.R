#' Convert a time value to an HH:MM:SS character string
#'
#' Accepts POSIXt, difftime, or numeric (fractional day) values and returns
#' a character string in HH:MM:SS format. Falls back to \code{as.character()}
#' for any other type.
#'
#' @param x A POSIXt, difftime, numeric (fractional day), or other object.
#' @return A character string formatted as HH:MM:SS.
as_hms_text <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(format(x, "%H:%M:%S"))
  }

  if (inherits(x, "difftime")) {
    seconds <- as.numeric(x, units = "secs")
    return(format(as.POSIXct(seconds, origin = "1970-01-01", tz = "UTC"), "%H:%M:%S"))
  }

  if (is.numeric(x)) {
    seconds <- x * 24 * 60 * 60
    return(format(as.POSIXct(seconds, origin = "1970-01-01", tz = "UTC"), "%H:%M:%S"))
  }

  as.character(x)
}

#' Read and harmonise chamber measurement metadata from Google Sheets
#'
#' Reads the LightDark and After-Watering measurement sheets from the project
#' Google Sheet, converts raw time columns to POSIXct datetimes
#' (America/Los_Angeles), pivots the LightDark sheet from wide to long so that
#' Light and Dark measurements are stacked as a \code{Condition} column, assigns
#' a per-PotID \code{Order_Index} to each measurement occasion, and returns both
#' tables in a named list ready for \code{bind_rows()}.
#'
#' @param metadata_url Character. URL of the Google Sheet containing the
#'   \code{LightDark_Measurment} sheet.
#' @param afterwatering_url Character. URL of the Google Sheet containing the
#'   \code{After_WateringMeasurment} sheet.
#'
#' @return A named list with two elements:
#'   \describe{
#'     \item{lightdark_metadata}{Long-format data frame with columns
#'       \code{PotID}, \code{Date}, \code{Chamber_Start_DateTime},
#'       \code{Chamber_End_DateTime}, \code{Order_Index}, and \code{Condition}
#'       (\code{"Light"} or \code{"Dark"}).}
#'     \item{afterwatering_metadata}{Data frame with the same key columns plus
#'       \code{Condition = "After_Watering"}.}
#'   }
read_metadata <- function(metadata_url, afterwatering_url) {
  # --- LightDark sheet --------------------------------------------------
  # The sheet stores Light and Dark windows as paired wide columns.
  # We convert the raw hh:mm:ss time strings to POSIXct before pivoting.
  # .keep = "unused" drops the original raw time columns after mutate.
  lightdark_metadata = read_sheet(metadata_url, sheet = "LightDark_Measurment") %>%
    # googlesheets4 sometimes appends "...N" sentinel columns — remove them
    select(-contains("...")) %>%
    mutate(
      Date = as.Date(Date),
      Chamber_Start_DateTime_Light = as.POSIXct(
        paste(as.Date(Date), as_hms_text(Chamber_Start_Time_Light_hh_mm_ss)),
        format = "%Y-%m-%d %H:%M:%S",
        tz = "America/Los_Angeles"
      ),
      Chamber_End_DateTime_Light = as.POSIXct(
        paste(as.Date(Date), as_hms_text(Chamber_End_Time_Light_hh_mm_ss)),
        format = "%Y-%m-%d %H:%M:%S",
        tz = "America/Los_Angeles"
      ),
      Chamber_Start_DateTime_Dark = as.POSIXct(
        paste(as.Date(Date), as_hms_text(Chamber_Start_Time_Dark_hh_mm_ss)),
        format = "%Y-%m-%d %H:%M:%S",
        tz = "America/Los_Angeles"
      ),
      Chamber_End_DateTime_Dark = as.POSIXct(
        paste(as.Date(Date), as_hms_text(Chamber_End_Time_Dark_hh_mm_ss)),
        format = "%Y-%m-%d %H:%M:%S",
        tz = "America/Los_Angeles"
      ),
      .keep = "unused"
    ) %>%
    group_by(PotID) %>%
    # Order_Index ranks measurement occasions chronologically within each pot
    mutate(
      Light_Order_Index = row_number(Chamber_Start_DateTime_Light),
      Dark_Order_Index = row_number(Chamber_Start_DateTime_Dark)
    ) %>%
    ungroup()

  # Pivot wide → long so Light and Dark rows are stacked under a
  # single `Condition` column. The regex pattern splits column names like
  # "Chamber_Start_DateTime_Light" into value-key ("Start") and condition ("Light"),
  # producing unified Chamber_Start_DateTime / Chamber_End_DateTime columns.
  lightdark_metadata_long = lightdark_metadata %>%
    rename(
      Order_Index_Light = Light_Order_Index,
      Order_Index_Dark = Dark_Order_Index
    ) %>%
    tidyr::pivot_longer(
      cols = c(
        Chamber_Start_DateTime_Light,
        Chamber_End_DateTime_Light,
        Chamber_Start_DateTime_Dark,
        Chamber_End_DateTime_Dark,
        Order_Index_Light,
        Order_Index_Dark
      ),
      names_to = c(".value", "Condition"),
      names_pattern = "^(?:Chamber_)?(Start|End|Order_Index)(?:_DateTime)?_(Light|Dark)$"
    ) %>%
    rename(
      Chamber_Start_DateTime = Start,
      Chamber_End_DateTime = End
    ) %>%
    relocate(Condition, .after = last_col())

  # --- After-Watering sheet ---------------------------------------------
  # Already in long format (one measurement condition per row), so only datetime
  # conversion and Order_Index assignment are needed. Each row is also one
  # watering of the pot, with the water added in Water_g_60WHC.
  # Only columns A:N are read for now. The tab has a second, hidden block in
  # columns Q:U that repeats Water_g_60WHC, Soil_Temperature_C_7cm, surface
  # temperature and EC (typed-in copies, identical to H:K as of 2026-10-30, with
  # the "_13cm" suffix on the other column) and adds Soil_Moisture_percent.
  # Reading A:N keeps one copy of each column under its own name.
  afterwatering_metadata = read_sheet(afterwatering_url, sheet = "After_WateringMeasurment", range = "A:N") %>%
    select(-contains("...")) %>%
    mutate(
      Date = as.Date(Date),
      Chamber_Start_DateTime = as.POSIXct(
        paste(as.Date(Date), as_hms_text(Chamber_Start_Time_After_Watering_hh_mm_ss)),
        format = "%Y-%m-%d %H:%M:%S",
        tz = "America/Los_Angeles"
      ),
      Chamber_End_DateTime = as.POSIXct(
        paste(as.Date(Date), as_hms_text(Chamber_End_Time_After_Watering_hh_mm_ss)),
        format = "%Y-%m-%d %H:%M:%S",
        tz = "America/Los_Angeles"
      ),
      Condition = "After_Watering",
      .keep = "unused"
    ) %>%
    group_by(PotID) %>%
    mutate(Order_Index = row_number(Chamber_Start_DateTime)) %>%
    ungroup()

  list(
    lightdark_metadata = lightdark_metadata_long,
    afterwatering_metadata = afterwatering_metadata
  )
}

#' Total water added to each pot over the experiment
#'
#' Sums the water added at each watering (\code{Water_g_60WHC}, g added to
#' bring the pot to 60% of water-holding capacity; one After-Watering sheet row
#' per watering) to the end-point total per pot.
#'
#' @param afterwatering_metadata The \code{afterwatering_metadata} element of
#'   \code{read_metadata()}.
#'
#' @return Data frame with one row per pot: \code{PotID}, \code{Soil},
#'   \code{Treatment}, \code{Replicate}, \code{Block}, \code{N_Waterings},
#'   \code{First_Watering}, \code{Last_Watering} (dates) and
#'   \code{Water_Total_g}.
total_water_added <- function(afterwatering_metadata) {
  n_blank <- sum(is.na(afterwatering_metadata$Water_g_60WHC))
  if (n_blank > 0) {
    warning(n_blank, " blank Water_g_60WHC cell(s) in After_WateringMeasurment, counted as no water",
            call. = FALSE)
  }

  afterwatering_metadata %>%
    group_by(PotID, Soil, Treatment, Replicate, Block) %>%
    summarise(
      N_Waterings = sum(!is.na(Water_g_60WHC)),
      First_Watering = min(Date),
      Last_Watering = max(Date),
      Water_Total_g = sum(Water_g_60WHC, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(Block, PotID)
}

#' Water use efficiency of each pot
#'
#' Water use efficiency (mg shoot dry weight per g water added) = end-point
#' shoot dry weight (\code{Shoot dry weigh_mg} in the end-point sheet) divided
#' by the total water added (\code{total_water_added()}). The end-point sheet
#' has one row per pot and soil fraction (\code{Soil_Type} Bulk and
#' Rhizosphere) with the same shoot weight in both, plus T0 and biochar
#' reference rows without plants; only the pots in \code{water_per_pot} are
#' kept. The function stops if a pot has no shoot weight or two different ones.
#'
#' @param endpoint_url Character. URL of the Google Sheet containing the
#'   \code{pH_EC_EndPoint_July2026} sheet.
#' @param water_per_pot Output of \code{total_water_added()}.
#'
#' @return \code{water_per_pot} with \code{Shoot_Dry_Weight_mg} and
#'   \code{WUE_mg_per_g} added.
water_use_efficiency <- function(endpoint_url, water_per_pot) {
  shoots <- read_sheet(endpoint_url, sheet = "pH_EC_EndPoint_July2026") %>%
    transmute(PotID = trimws(`Sample ID`), Shoot_Dry_Weight_mg = `Shoot dry weigh_mg`) %>%
    filter(PotID %in% water_per_pot$PotID) %>%
    distinct()

  conflicting <- shoots$PotID[duplicated(shoots$PotID)]
  if (length(conflicting) > 0) {
    stop("pot(s) with different shoot dry weights in the end-point sheet: ",
         paste(unique(conflicting), collapse = ", "), call. = FALSE)
  }
  missing <- setdiff(water_per_pot$PotID, shoots$PotID[!is.na(shoots$Shoot_Dry_Weight_mg)])
  if (length(missing) > 0) {
    stop("pot(s) without a shoot dry weight in the end-point sheet: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  water_per_pot %>%
    left_join(shoots, by = "PotID") %>%
    mutate(WUE_mg_per_g = Shoot_Dry_Weight_mg / Water_Total_g)
}

#' Read the plant measurements over time
#'
#' Reads the \code{Raw_Plant_Measurements_Shoot_Chlorophyll} sheet (one row per
#' pot and date) and averages the raw readings per pot and date: shoot count
#' and shoot height over the 7 shoots (\code{_R1}-\code{_R7}) and chlorophyll
#' index over \code{Leaf_1}-\code{Leaf_5}. \code{Leaf_6} is ignored: on
#' 2026-04-09 it holds the shoot count average instead of a leaf reading, which
#' also makes the sheet's \code{Chlorophyll_index_Average} wrong for that date.
#'
#' @param plant_url Character. URL of the Google Sheet containing the
#'   \code{Raw_Plant_Measurements_Shoot_Chlorophyll} sheet.
#'
#' @return Data frame with one row per pot and date: \code{PotID},
#'   \code{Block}, \code{Date}, \code{Shoot_Count}, \code{Shoot_Height_cm} and
#'   \code{Chlorophyll_Index}.
read_plant_measurements <- function(plant_url) {
  sheet <- read_sheet(plant_url, sheet = "Raw_Plant_Measurements_Shoot_Chlorophyll")
  row_mean <- function(columns) rowMeans(as.matrix(sheet[columns]), na.rm = TRUE)
  sheet %>%
    transmute(
      PotID = trimws(PotID),
      Block,
      # dates are typed as yyyymmdd numbers
      Date = as.Date(as.character(Date), format = "%Y%m%d"),
      Shoot_Count = row_mean(paste0("Shoots_Count_R", 1:7)),
      Shoot_Height_cm = row_mean(paste0("Shoots_Height_cm_R", 1:7)),
      Chlorophyll_Index = row_mean(paste0("Leaf_", 1:5))
    )
}

#' Plant measurements averaged over dates
#'
#' @param plant_measurements Output of \code{read_plant_measurements()}.
#' @return Data frame with one row per pot: \code{PotID}, \code{Block},
#'   \code{N_Dates} and the mean over dates of \code{Shoot_Count},
#'   \code{Shoot_Height_cm} and \code{Chlorophyll_Index}.
plant_measurements_per_pot <- function(plant_measurements) {
  plant_measurements %>%
    group_by(PotID, Block) %>%
    summarise(N_Dates = n_distinct(Date),
              across(c(Shoot_Count, Shoot_Height_cm, Chlorophyll_Index), mean),
              .groups = "drop")
}

# Output variables of the root imaging (WinRHIZO) sheet, in sheet order
root_imaging_variables <- c("Length_cm", "ProjArea_cm2", "SurfArea_cm2", "AvgDiam_mm", "LenPerVol_cm/m3",
                            "RootVolume_cm3", "Tips", "Forks", "Crossings")

#' Read the root imaging (WinRHIZO) results, mapped to the standard pot IDs
#'
#' Reads the \code{Greenhouse} tab of the "winrhizo_Result_20260528" Sheet: 4
#' replicate measurements per pot (the sheet's \code{Replicate} column, 1-4;
#' 96 rows) of the 9 output variables in \code{root_imaging_variables}: root
#' length, projected and surface area, average diameter, length per volume
#' (identical to length in this sheet), root volume, tips, forks and crossings.
#' Only \code{Treatment}, \code{Replicate}, \code{Block}, \code{PotID} and
#' those variables are read; the \code{Inoculum} and \code{Biochar_Addition}
#' columns are treatment labels and, like any other column, are ignored.
#'
#' Pot IDs: the sheet originally used its own, inconsistent pot IDs (e.g.
#' \code{Org1A_1}, \code{CON-1A_1}, \code{BC-A1_1}, \code{BC-MPSS_1A_1}, with
#' the replicate measurement as suffix). On 2026-10-30 its \code{PotID} column
#' was changed to the standard IDs (\code{<treatment>_<replicate>_<block>},
#' e.g. \code{Conv_BC-SMT_1_A}), the same for the 4 replicate measurements of
#' a pot.
#'
#' Mapping: the sheet's \code{Treatment} and \code{Block} columns are spelled
#' exactly as in the measurement sheet, and each Treatment x Block is one pot,
#' so measurements are matched to standard pots on Treatment + Block; this
#' does not depend on the sheet's pot IDs. As a cross-check, a filled-in
#' \code{PotID} must equal the matched standard ID (blank ones get a warning).
#' The function stops if a measurement matches no pot, a filled-in PotID
#' disagrees, or a pot does not have exactly the 4 replicate measurements 1-4.
#'
#' @param root_url Character. URL of the root imaging Google Sheet.
#' @param pots Data frame of the standard pots: \code{PotID},
#'   \code{Treatment}, \code{Block} (e.g. from \code{total_water_added()}).
#'
#' @return Data frame with one row per measurement: \code{PotID} (standard),
#'   \code{Treatment}, \code{Block}, \code{Replicate} (1-4, the replicate
#'   measurement) and the 9 variables in \code{root_imaging_variables}.
read_root_imaging <- function(root_url, pots) {
  sheet <- read_sheet(root_url, sheet = "Greenhouse", .name_repair = "unique_quiet")
  missing <- setdiff(c("PotID", "Treatment", "Replicate", "Block", root_imaging_variables), names(sheet))
  if (length(missing) > 0) {
    stop("root imaging sheet is missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }

  mapped <- sheet %>%
    transmute(Sheet_PotID = trimws(as.character(PotID)), Treatment = trimws(Treatment),
              Block = trimws(Block), Replicate = as.integer(Replicate),
              across(all_of(root_imaging_variables))) %>%
    filter(!is.na(Treatment)) %>%
    left_join(select(pots, PotID, Treatment, Block), by = c("Treatment", "Block"))

  unmatched <- mapped %>% filter(is.na(PotID))
  if (nrow(unmatched) > 0) {
    stop("root measurements with no standard pot of the same Treatment and Block: ",
         paste(unmatched$Treatment, unmatched$Block, collapse = ", "), call. = FALSE)
  }
  disagree <- mapped %>% filter(!is.na(Sheet_PotID), Sheet_PotID != "", Sheet_PotID != PotID)
  if (nrow(disagree) > 0) {
    stop("root sheet PotIDs that disagree with their Treatment and Block: ",
         paste(unique(paste(disagree$Sheet_PotID, "vs", disagree$PotID)), collapse = ", "), call. = FALSE)
  }
  blank <- mapped %>% filter(is.na(Sheet_PotID) | Sheet_PotID == "")
  if (nrow(blank) > 0) {
    warning(nrow(blank), " root imaging row(s) with a blank PotID, mapped by Treatment and Block to: ",
            paste(unique(blank$PotID), collapse = ", "), call. = FALSE)
  }
  bad_replicates <- pots %>%
    select(PotID) %>%
    left_join(mapped %>% group_by(PotID) %>%
                summarise(replicates = paste(sort(Replicate), collapse = ","), .groups = "drop"),
              by = "PotID") %>%
    filter(is.na(replicates) | replicates != "1,2,3,4")
  if (nrow(bad_replicates) > 0) {
    stop("expected root replicate measurements 1-4 per pot; found: ",
         paste(bad_replicates$PotID, bad_replicates$replicates, sep = " = ", collapse = ", "), call. = FALSE)
  }

  mapped %>%
    select(PotID, Treatment, Block, Replicate, all_of(root_imaging_variables)) %>%
    arrange(Block, PotID, Replicate)
}

#' Root imaging results averaged per pot
#'
#' @param root_measurements Output of \code{read_root_imaging()}.
#' @return Data frame with one row per pot: \code{PotID}, \code{Treatment},
#'   \code{Block}, \code{N_Replicates} and the mean of the 4 replicate
#'   measurements of each variable in \code{root_imaging_variables}.
root_imaging_per_pot <- function(root_measurements) {
  root_measurements %>%
    group_by(PotID, Treatment, Block) %>%
    summarise(N_Replicates = n(),
              across(all_of(root_imaging_variables), mean),
              .groups = "drop")
}

# Standard PotID prefix for each treatment name in the mycorrhizal sheet
# (both tabs spell the treatments this way and have no pot ID)
mycorrhizal_prefixes <- c("Convential_soil" = "Conv_Ctrl", "Organic_Soil" = "Org_Ctrl",
                          "Conv_Bc" = "Conv_BC", "Conv_Bc_SMT" = "Conv_BC-SMT",
                          "Conv_Bc_MPSS" = "Conv_BC-MPSS", "Conv_Bc_MPSS-SMT" = "Conv_BC-MPSS-SMT")

#' Map mycorrhizal sheet rows to the standard pots
#'
#' Translates \code{Mycorrhizal_Treatment} to the standard PotID prefix
#' (\code{mycorrhizal_prefixes}) and joins, with \code{Block}, to the standard
#' pot of that prefix and block (each block holds one pot per treatment).
#' Stops on an unknown treatment name or a row that matches no pot.
#'
#' @param sheet Data frame with \code{Mycorrhizal_Treatment} and \code{Block}.
#' @param pots Data frame of the standard pots with \code{PotID},
#'   \code{Treatment}, \code{Block} (other columns are carried along).
#' @return \code{sheet} with the columns of \code{pots} added.
map_mycorrhizal_pots <- function(sheet, pots) {
  unknown <- setdiff(sheet$Mycorrhizal_Treatment, names(mycorrhizal_prefixes))
  if (length(unknown) > 0) {
    stop("unknown treatment name(s) in the mycorrhizal sheet: ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  mapped <- sheet %>%
    mutate(prefix = unname(mycorrhizal_prefixes[Mycorrhizal_Treatment])) %>%
    left_join(pots %>% mutate(prefix = sub("_[0-9]+_[A-D]$", "", PotID)), by = c("prefix", "Block")) %>%
    select(-prefix)
  unmatched <- mapped %>% filter(is.na(PotID))
  if (nrow(unmatched) > 0) {
    stop("mycorrhizal rows with no standard pot: ",
         paste(unique(paste(unmatched$Mycorrhizal_Treatment, unmatched$Block)), collapse = ", "), call. = FALSE)
  }
  mapped
}

#' Read the root mycorrhizal colonization, mapped to the standard pot IDs
#'
#' Reads the \code{PlotingData} tab of the "GreenhouseMycorrhizal_Colonization"
#' Sheet: one row per pot with the number of root points (out of 50) scored as
#' colonized, averaged over 4 replicate reads
#' (\code{Root_mycorrhizal_colonization_COUNT}, hence fractional), the
#' colonization percentage (\code{Root_mycorrhizal_colonization_(\%)} = 2 x the
#' count), and shoot and root dry weight. The per-point scores are in the
#' RawData tab (not read here).
#'
#' The sheet has no pot ID: a pot is identified by its \code{Treatment} and
#' \code{Block}, and the treatments are spelled differently from the
#' measurement sheet:
#' \preformatted{
#'   sheet Treatment    standard PotID prefix
#'   Convential_soil    Conv_Ctrl
#'   Organic_Soil       Org_Ctrl
#'   Conv_Bc            Conv_BC
#'   Conv_Bc_SMT        Conv_BC-SMT
#'   Conv_Bc_MPSS       Conv_BC-MPSS
#'   Conv_Bc_MPSS-SMT   Conv_BC-MPSS-SMT
#' }
#' Mapping (\code{map_mycorrhizal_pots()}): the sheet's treatment is
#' translated to the standard PotID prefix (table above) and matched, with the
#' block, to the standard pot of that prefix and block (each block holds one
#' pot per treatment). As a
#' cross-check, the sheet's shoot dry weight must equal the end-point sheet's
#' for the matched pot (\code{Shoot_Dry_Weight_mg} in \code{pots}, when
#' present). The function stops if a treatment name is unknown, a row matches
#' no pot, a pot has no row or more than one, or the shoot weights disagree.
#' The \code{Average_Root_mycorrhizal_colonization_(\%)} column (the treatment
#' mean, filled only on each treatment's first row) is not kept.
#'
#' @param mycorrhizal_url Character. URL of the mycorrhizal colonization
#'   Google Sheet.
#' @param pots Data frame of the standard pots: \code{PotID},
#'   \code{Treatment}, \code{Block}, and optionally \code{Shoot_Dry_Weight_mg}
#'   (e.g. the output of \code{water_use_efficiency()}).
#'
#' @return Data frame with one row per pot: \code{PotID}, \code{Treatment}
#'   (standard), \code{Block}, \code{Mycorrhizal_Treatment} (the sheet's
#'   name), \code{Colonization_Count}, \code{Colonization_pct},
#'   \code{Shoot_Dry_Weight_mg} and \code{Root_Dry_Weight_mg} (from this sheet).
read_mycorrhizal_colonization <- function(mycorrhizal_url, pots) {
  sheet <- read_sheet(mycorrhizal_url, sheet = "PlotingData") %>%
    transmute(Mycorrhizal_Treatment = trimws(Treatment),
              Block = trimws(Block),
              Colonization_Count = Root_mycorrhizal_colonization_COUNT,
              Colonization_pct = `Root_mycorrhizal_colonization_(%)`,
              Sheet_Shoot_mg = `Shoot dry weigh_mg`,
              Root_Dry_Weight_mg = `Root dry weigh_mg`) %>%
    filter(!is.na(Mycorrhizal_Treatment))

  mapped <- map_mycorrhizal_pots(sheet, pots)

  row_counts <- pots %>%
    select(PotID) %>%
    left_join(count(mapped, PotID), by = "PotID") %>%
    mutate(n = coalesce(n, 0L)) %>%
    filter(n != 1)
  if (nrow(row_counts) > 0) {
    stop("expected one mycorrhizal row per pot; found: ",
         paste(row_counts$PotID, row_counts$n, sep = " = ", collapse = ", "), call. = FALSE)
  }
  if ("Shoot_Dry_Weight_mg" %in% names(mapped)) {
    shoot_mismatch <- mapped %>% filter(abs(Sheet_Shoot_mg - Shoot_Dry_Weight_mg) > 1e-6)
    if (nrow(shoot_mismatch) > 0) {
      stop("shoot dry weight in the mycorrhizal sheet differs from the end-point sheet for: ",
           paste(shoot_mismatch$PotID, collapse = ", "), call. = FALSE)
    }
  }

  mapped %>%
    transmute(PotID, Treatment, Block, Mycorrhizal_Treatment, Colonization_Count, Colonization_pct,
              Shoot_Dry_Weight_mg = Sheet_Shoot_mg, Root_Dry_Weight_mg) %>%
    arrange(Block, PotID)
}

#' Read the 4 replicate mycorrhizal colonization reads per pot
#'
#' Reads the \code{RawData} tab of the mycorrhizal colonization Sheet: per pot
#' (\code{Traitment} + \code{Block}), 50 scored points (5 root fragments,
#' \code{Replicate_Fragments} 1-5, x 10 points, \code{Point} 1-10), each scored
#' 0/1 (not colonized / colonized by spores, vesicles or fungal hyphae) in 4
#' replicate reads (columns \code{Replicate1_...} to \code{Replicate4_...}).
#' The colonization of a read is the percentage of the 50 points scored 1
#' (2 x the count). The tab also holds a summary row after each pot
#' (\code{Total}/\code{total}, one labelled \code{Conv_Bc}) without a block or
#' point number; rows without \code{Point} or \code{Block} are skipped.
#'
#' Pots are mapped to the standard PotIDs as in
#' \code{read_mycorrhizal_colonization()} (\code{map_mycorrhizal_pots()}). The
#' function stops if a pot does not have exactly 5 x 10 points, a score is not
#' 0/1, or (when \code{colonization} is given) the mean of the 4 reads differs
#' from the PlotingData percentage.
#'
#' @param mycorrhizal_url Character. URL of the mycorrhizal colonization
#'   Google Sheet.
#' @param pots Data frame of the standard pots: \code{PotID},
#'   \code{Treatment}, \code{Block}.
#' @param colonization Optional output of \code{read_mycorrhizal_colonization()},
#'   used as a cross-check.
#'
#' @return Data frame with one row per pot and read: \code{PotID},
#'   \code{Treatment}, \code{Block}, \code{Replicate} (1-4),
#'   \code{Colonized_Points} (of 50) and \code{Colonization_pct}.
read_mycorrhizal_replicates <- function(mycorrhizal_url, pots, colonization = NULL) {
  raw <- read_sheet(mycorrhizal_url, sheet = "RawData")
  read_columns <- grep("^Replicate[1-4]_", names(raw), value = TRUE)
  if (length(read_columns) != 4) {
    stop("expected 4 Replicate<n>_ columns in the mycorrhizal RawData tab, found: ",
         paste(read_columns, collapse = ", "), call. = FALSE)
  }

  points <- raw %>%
    filter(!is.na(Point), !is.na(Block)) %>%
    transmute(Mycorrhizal_Treatment = trimws(Traitment), Block = trimws(Block),
              Fragment = Replicate_Fragments, Point, across(all_of(read_columns))) %>%
    tidyr::pivot_longer(all_of(read_columns), names_to = "Replicate", values_to = "Score") %>%
    mutate(Replicate = as.integer(sub("^Replicate([1-4])_.*", "\\1", Replicate))) %>%
    map_mycorrhizal_pots(select(pots, PotID, Treatment, Block))

  bad_scores <- points %>% filter(is.na(Score) | !Score %in% c(0, 1))
  if (nrow(bad_scores) > 0) {
    stop("mycorrhizal scores other than 0/1 for: ", paste(unique(bad_scores$PotID), collapse = ", "), call. = FALSE)
  }
  layout <- pots %>%
    select(PotID) %>%
    left_join(points %>% group_by(PotID) %>%
                summarise(points = n_distinct(paste(Fragment, Point)), fragments = n_distinct(Fragment),
                          reads = n() / n_distinct(paste(Fragment, Point)), .groups = "drop"),
              by = "PotID") %>%
    filter(is.na(points) | points != 50 | fragments != 5 | reads != 4)
  if (nrow(layout) > 0) {
    stop("expected 5 fragments x 10 points, each read 4 times, per pot; not so for: ",
         paste(layout$PotID, collapse = ", "), call. = FALSE)
  }

  replicates <- points %>%
    group_by(PotID, Treatment, Block, Replicate) %>%
    summarise(Colonized_Points = sum(Score), .groups = "drop") %>%
    mutate(Colonization_pct = 100 * Colonized_Points / 50) %>%
    arrange(Block, PotID, Replicate)

  if (!is.null(colonization)) {
    mismatch <- replicates %>%
      group_by(PotID) %>%
      summarise(mean_pct = mean(Colonization_pct), .groups = "drop") %>%
      inner_join(select(colonization, PotID, Colonization_pct), by = "PotID") %>%
      filter(abs(mean_pct - Colonization_pct) > 1e-6)
    if (nrow(mismatch) > 0) {
      stop("mean of the 4 mycorrhizal reads differs from the PlotingData percentage for: ",
           paste(mismatch$PotID, collapse = ", "), call. = FALSE)
    }
  }
  replicates
}
