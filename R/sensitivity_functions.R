
#### Ensure required packages are installed ####
pkgs_cran <- c("rredlist", "purrr", "stringr", "tidyr", "dplyr", "readxl", "galah")

##################################
# install CRAN packages if missing
for (pkg in pkgs_cran) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}
##################################

#### organise_piaout ####

## This function organise the final species list output from PIA. Note that this function is highly dependent on the column names of each data sources (ALA, PIA outputs)

organise_piaout <- function(df) {
  
  # 1. label subspecies with 1/0, add a column of listing hierarchy, add prefix pia_
  final <- df %>%   # Remove overlapping species
    dplyr::select(-model_used) %>% 
    dplyr::distinct() %>%  # Remove duplicates due to different models
    dplyr::mutate(
      subspecies = dplyr::case_when(   # Label subspecies
        stringr::str_count(npw_listed_name, "\\S+") > 2 |
          stringr::str_count(future_listed_name, "\\S+") > 2 ~ 1L,
        TRUE ~ 0L
      ),
      listing_hierarchy = dplyr::case_when(   # Label listing hierarchy
        epbc_status != "" & !is.na(epbc_status) ~ "epbc",
        npw_status != "" & !is.na(npw_status) ~ "npw",
        future_status != "" & !is.na(future_status) ~ "future",
        TRUE ~ "reg_cont"
      )
    )

  final <- final %>% 
    dplyr::rename_with(~ base::paste0("pia_", .x), base::names(final[-1]))
  
  # 3. Extract class columns from ALA
  ala <- galah::search_taxa(final$taxa) %>%
    dplyr::distinct() %>%  # 398
    dplyr::select(search_term, kingdom, class, family, vernacular_name, match_type)
  
  ala <- ala %>% 
    dplyr::rename_with(~ base::paste0("ala_", .x), base::names(ala[-1]))
  
  # 4. ALA join final
  splist <- final %>%
    dplyr::left_join(ala, by = join_by(taxa == search_term)) %>% 
    dplyr::rename(search_term = taxa)
  
  # 5. Filter out invert and non-vascular plants that are from regional contribution and split the dataset to major groups by class
  class_list <- c("Equisetopsida", "Aves", "Mammalia", "Reptilia", "Actinopterygii", "Amphibia")
  
  splist <- splist %>%
    dplyr::filter(pia_listing_hierarchy != "reg_cont" | ala_class %in% class_list)
  
  return(splist)
}


#### iucn_poptrend ####

## This function query the IUCN population trend data for the input species and return a data frame containing four columns: taxa, year (the most updated trend of the assessed species), IUCN ID and population trend.

iucn_poptrend <- function(taxon_name_vec) {
  
  # 0. Check API key is in the environment
  api_key <- Sys.getenv("IUCN_REDLIST_KEY")
  
  if (is.null(api_key) || api_key == "") {
    stop(
      "IUCN API key not found.\n",
      "Please set your API key using:\n",
      'Sys.setenv(IUCN_REDLIST_KEY = "YOUR_KEY_HERE")\n',
      "You can obtain a free key from: https://api.iucnredlist.org/"
    )}
  
  # 1. Prepare input
  safe_rl_species <- purrr::possibly(rl_species, otherwise = NA, quiet = TRUE)
  
  genus   <- stringr::str_split_fixed(taxon_name_vec, " ", 2)[, 1]
  species <- stringr::str_split_fixed(taxon_name_vec, " ", 2)[, 2]
  
  # 2. Query IUCN for each species
  rl_result <- purrr::map2(genus, species, safe_rl_species)
  names(rl_result) <- taxon_name_vec
  
  # 3. Extract assessment IDs and years
  assessment_ids <- purrr::map(rl_result[!is.na(rl_result)], ~ {
    ids <- .x$assessments$assessment_id
    if (length(ids) > 0) ids[1] else NA_character_
  })
  
  assessment_year <- purrr::map(rl_result[!is.na(rl_result)], ~ {
    year <- .x$assessments$year_published
    if (length(year) > 0) year[1] else NA_character_
  })
  
  df <- data.frame(
    search_term = names(assessment_ids),
    year = as.integer(unlist(assessment_year, use.names = FALSE)),
    id   = as.integer(unlist(assessment_ids, use.names = FALSE)),
    stringsAsFactors = FALSE
  ) %>%
    tidyr::drop_na()
  
  if (nrow(df) == 0) {
    warning("No valid IUCN assessments found for the provided taxa.")
    return(df)
  }
  
  # 4. Retrieve and extract assessment details
  assessment_list <- rredlist::rl_assessment_list(df$id)
  names(assessment_list) <- df$search_term
  
  dd <- rredlist::rl_assessment_extract(assessment_list, "population_trend")
  
  poptrend <- purrr::map_chr(dd, function(x) {
    if (is.null(x$description)) return(NA_character_)
    lang_key <- names(x$description)[1]
    val <- x$description[[lang_key]]
    if (length(val) > 0) val else NA_character_
  })
  
  df$poptrend <- poptrend
  df <- df %>%
    rename_with(~ paste0("trend_", .x), .cols = -search_term) %>% 
    distinct()
  
  return(df)
}


#### join_database ####

## This function is aiming for joining animal databases, do not use it for plants.
## This function joins trait database to the PIA/ALA names using two steps matching. The first step matches the exact scientific names used in PIA/ALA and excluded them from the second step. The second step fuzzy matches (by two character difference) the unmatched ones from the first step using a table documenting alternative names. The function outputs all taxa from the queried PIA/ALA names and includes a column indicating which step a species found a match (or not).

# data A = primary dataset, column of search_term is required.
# data B = reference dataset, columns of Genus and Species are required.
# A_sci = columns in A for scientific name
# prefix = prefix added to the joining dataset as identifier
# alt = alternative name table, required to be loaded prior.

### Look up table

alt.names <- data.frame(
  search_term = c(
    "Antigone rubicunda",
    "Cinclosoma castanotum",
    "Coturnix ypsilophora",
    "Amytornis whitei",
    "Hylacola cauta",
    "Parvipsitta porphyrocephala",
    "Petroica boodang",
    "Ardea intermedia",
    "Lophochroa leadbeateri",
    "Spatula rhynchotis",
    "Hypotaenidia philippensis",
    "Amytornis modestus",
    "Zapornia tabuensis",
    "Oedura cincta",
    "Morelia imbricata",
    "Tympanocryptis tolleyi",
    "Nyctophilus corbeni"
  ),
  
  alt_name = c(
    "Grus rubicunda",
    "Cinclosoma castanotus",
    "Synoicus ypsilophorus",
    "Amytornis striatus",
    "Calamanthus cauta",
    "Glossopsitta porphyrocephala",
    "Petroica multicolor",
    "Mesophoyx intermedia",
    "Cacatua leadbeateri",
    "Anas rhynchotis",
    "Gallirallus philippensis",
    "Amytornis textilis",
    "Porzana tabuensis",
    "Oedura marmorata",
    "Morelia spilota",
    "Tympanocryptis lineata",
    "Nyctophilus timoriensis")
)

join_database <- function(A, B, A_sci = "search_term", alt = alt.names, 
                          prefix) {
  
  # 0. Paste genus and species into a column, add prefix
  B <- B %>% mutate(B_sci = paste(Genus, Species)) %>% 
    rename_with(~ paste0(prefix, .x), 
                .cols = -c(Genus, Species, B_sci)) 
  
  # 1. First round matched: join by scientific name
  first_match <- A %>%
    left_join(B, by = setNames("B_sci", A_sci)) %>% 
    filter(!is.na(Genus)) %>% distinct() %>% 
    mutate(match = "first")
  
  # 2. First round unmatched
  first_unmatch <- A %>%
    filter(!search_term %in% first_match$search_term)
  
  # 3. Second round: Join unmatched by alternative names
  second_match <- first_unmatch %>% 
    left_join(alt, by = "search_term") %>% 
    filter(!is.na(alt_name)) %>% 
    fuzzyjoin::stringdist_left_join(B, by = c("alt_name" = "B_sci"), max_dist = 2, method = "osa") %>% 
    filter(!is.na(B_sci)) %>% 
    mutate(match = "second")
  
  # 4. Organised the still unmatched in the second round
  second_unmatch <- first_unmatch %>% 
    filter(!search_term %in% second_match$search_term) %>% 
    left_join(B, by = setNames("B_sci", A_sci)) %>% 
    mutate(match = "unmatched")
  
  # 5. Bind first_match, second_match, and still unmatched df together
  result <- first_match %>% 
    bind_rows(second_match %>% select(-alt_name, -B_sci)) %>% 
    bind_rows(second_unmatch) %>% 
    select(-Genus, -Species)
  names(result)[names(result) == "match"] <- paste0(prefix, "match")
  
  return(result)
}

#### iucn_threat ####

# This function calls and organises the Ward2020 threat database and add customised threat columns for literature reviews.

iucn_threat <- function(file_path,
                        sheet_name = "Species-Threat-Impact",
                        threat_prefix = "threat_") {
  
  # 1. Read the Excel sheet
  ward <- read_excel(file_path, sheet = sheet_name)
  
  # 2. Select and clean columns
  threat <- ward %>%
    select(
      `Species name adjusted`,
      `Species name`,
      `EPBC Act status`,
      `Broad level threat`,
      `Impact score`
    ) %>%
    distinct(`Species name adjusted`, `Broad level threat`, .keep_all = TRUE) %>%
    pivot_wider(
      names_from = `Broad level threat`,
      values_from = `Impact score`,
      values_fill = "Not_Identified"
    ) %>%
    mutate(taxa = word(`Species name adjusted`, 1, 2)) # keep binomial
  
  # 3. Rename columns
  colnames(threat)[1:2] <- c("ward_species_adjusted", "ward_species")
  colnames(threat) <- gsub(" ", "_", colnames(threat))
  
  # 4. Add prefix to all except ID columns
  threat <- threat %>%
    rename_with(
      ~ paste0(threat_prefix, .x),
      .cols = -c("ward_species_adjusted", "ward_species", "taxa")
    ) %>%
    mutate(
      Lit_habitatloss = "",
      Lit_degradation = "",
      Lit_fragmentation = "",
      Lit_grazing = "",
      Lit_road = "",
      Lit_invasive = "",
      Lit_fire = "",
      Lit_climate = "",
      Lit_mainthreat_ref = ""
    )
  
  return(threat)
}

#### get_austraits ####

## This function is to query the species from austraits database and will return the requested traits. Data sources will be collapsed into one cell by each species. All subspecies of the queried species are included, therefore the entries will be more than the queried species.

get_austraits <- function(taxon_vector,
                          traits,
                          version = "6.0.0",
                          path = "database/austraits") {
  
  if (!requireNamespace("austraits", quietly = TRUE)){install.packages("austraits")}
  library("austraits")
  
  # 1. Load Austraits database
  message("Loading Austraits version ", version, " from ", path, "...")
  austraits_data <- austraits::load_austraits(version = version, path = path)
  
  # 2. Extract taxa and selected traits
  message("Extracting taxa and trait data...")
  plants <- austraits::extract_taxa(austraits_data, taxon_name = taxon_vector) %>%
    austraits::extract_trait(trait_names = traits)
  
  # 3. Clean and reshape trait data
  planttraits <- plants$traits %>%
    dplyr::select(taxon_name, trait_name, value) %>%
    tidyr::pivot_wider(names_from = trait_name, values_from = value) %>%
    dplyr::mutate(dplyr::across(
      where(is.list),
      ~ sapply(.x, function(x) paste(x, collapse = "; "))
    ))
  
  # 4. Add prefix
  planttraits <- planttraits %>%
    rename_with(~ paste0("trait_", .x), names(planttraits[-1]))
  
  message("Finished! Returning a data frame with ", nrow(planttraits), " observations")
  
  return(planttraits)
}

#### majority_value ####

## This function applies a majority rule to semicolon-separated entries in a column. For each cell, the most frequent value is kept; if there’s a tie, all unique values are returned.

majority_value <- function(cols) {
  
  # Ensure input is character
  cols <- base::as.character(cols)
  
  base::sapply(cols, function(cell) {
    if (base::is.na(cell) || cell == "") return(NA_character_)
    
    if (base::grepl(";", cell)) {
      parts <- base::trimws(base::strsplit(cell, ";\\s*")[[1]])
      ux <- base::unique(parts)
      counts <- base::tabulate(base::match(parts, ux))
      max_count <- base::max(counts)
      
      # If tie, return all unique parts joined by "; "
      if (base::sum(counts == max_count) > 1) {
        base::paste(ux, collapse = "; ")
      } else {
        ux[base::which.max(counts)]
      }
    } else {
      cell
    }
  }, USE.NAMES = FALSE)
}


#### filter_by_coverage ####

## This function filters out the columns in a dataframe with data coverage less than the specified threshold. Coverage = non-empty, non-NA entries / total rows.

filter_by_coverage <- function(df, threshold = 0.5) {
  
  # Calculate coverage (non-empty, non-NA)
  coverage <- base::data.frame(
    column = base::names(df),
    percent = base::colSums(df != "" & !base::is.na(df)) / base::nrow(df)
  )
  
  # Keep only columns with coverage >= threshold
  df_filtered <- df[, coverage$column[coverage$percent >= threshold], drop = FALSE]
  
  return(df_filtered)
}

#### add_ref_and_lit_cols ####

## This function add "_ref" column for each trait and at the end append columns for literature review. The columns are organised so that the ref cols are next to their trait cols.

add_ref_and_lit_cols <- function(df, lit_traits) {
  
  ## ---------- Add reference columns ----------
  df_ref <- df %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("trait_"),
        ~ dplyr::if_else(!is.na(.) & . != "", "austraits", NA_character_),
        .names = "{.col}_ref"
      ),
      dplyr::across(
        dplyr::starts_with("trend_"),
        ~ dplyr::if_else(!is.na(.) & . != "", "iucn", NA_character_),
        .names = "{.col}_ref"
      ),
      dplyr::across(
        dplyr::starts_with("threat_"),
        ~ dplyr::if_else(!is.na(.) & . != "", "ward2020", NA_character_),
        .names = "{.col}_ref"
      )
    )
  
  ## ---------- Reorder columns ----------
  ref_cols <- base::grep("_ref$", base::names(df_ref), value = TRUE)
  orig_cols <- base::sub("_ref$", "", ref_cols)
  
  new_order <- base::c()
  for (i in base::seq_along(orig_cols)) {
    if (orig_cols[i] %in% base::names(df_ref)) {
      new_order <- base::c(new_order, orig_cols[i], ref_cols[i])
    }
  }
  
  remaining_cols <- base::setdiff(base::names(df_ref), base::c(orig_cols, ref_cols))
  final_order <- base::c(remaining_cols, new_order)
  df_reordered <- df_ref[, final_order, drop = FALSE]
  
  ## ---------- Add literature trait columns ----------

  
  # Create all new column names (Lit_x, Lit_x_des, Lit_x_ref)
  lit_cols <- unlist(lapply(lit_traits, function(x)
    paste0("Lit_", c(x, paste0(x, "_des"), paste0(x, "_ref")))))
  
  # Add them efficiently (initialize as NA)
  df_final <- df_reordered %>%
    dplyr::mutate(!!!rlang::set_names(rep(list(NA), length(lit_cols)), lit_cols))
  
  ## ---------- Return result ----------
  return(df_final)
}

#### check_genus_traits ####
## Examine a specific trait within a genus

check_genus_traits <- function(genus, trait) {
  df <- extract_taxa(austraits, genus = genus,
                     partial_matches_allowed = FALSE) %>%
    extract_trait(trait, partial_matches_allowed = FALSE) %>%
    (\(x) x[["traits"]])()
  return(df)
}

#### missing_by_col ####
# A function that returns a table with column names and percentage of NA/blanks/-999 for each column.

missing_by_col <- function(df, threshold = NULL, direction = c("above", "below")) {
  
  direction <- match.arg(direction)
  
  n <- base::nrow(df)
  
  # Count NA, "", or -999
  result <- base::sapply(df, function(col) {
    base::sum(base::is.na(col) | col == "" | col == -999 | col == "NAV", na.rm = FALSE)
  })
  
  percent_missing <- (result / n) * 100
  
  output <- base::data.frame(
    column = base::names(result),
    n_missing = result,
    n_total = n,
    percent_missing = percent_missing,
    row.names = NULL
  )
  
  # If threshold is provided, filter the results
  if (!is.null(threshold)) {
    if (direction == "above") {
      output <- output[output$percent_missing >= threshold, ]
    } else {
      output <- output[output$percent_missing <= threshold, ]
    }
  }
  
  return(output)
}


#### make_dirs_from_column ####
## This function makes directories for each species if the directories do not exist yet (avoid overwrite).

# make_dirs_from_column(df, "species_name", "output/species")
make_dirs_from_column <- function(df, col_name, parent_dir) {
  
  # Extract folder names and clean up
  folder_names <- base::unique(df[[col_name]])
  folder_names <- folder_names[!base::is.na(folder_names) & folder_names != ""] # remove NA/blank
  
  # Full paths
  folder_paths <- base::file.path(parent_dir, folder_names)
  
  # Create missing directories
  new_dirs <- folder_paths[!base::dir.exists(folder_paths)]
  
  if (length(new_dirs) > 0) {
    base::sapply(new_dirs, base::dir.create, recursive = TRUE, USE.NAMES = FALSE)
    base::message(length(new_dirs), " new directories created.")
  } else {
    base::message("All directories already exist.")
  }
  
  return(folder_paths)
}

#### map_by_rowcol ####
## Map values from A to B by matching row names and specified column names.
# Arguments:
# A: df to map from (mtable)
# B: df to map to (info table)
# x: reference col (search_term)
# Atype: if == "long", pivot_wider the table, otherwise ignore.
# overwrite: whether to overwrite the values in B

map_by_rowcol <- function(A, B, x, Atype = NULL, overwrite = FALSE) {
  
  if (Atype == "long") {
    A <- A %>% pivot_wider(names_from = "trait", values_from = "value")
    } else {
    A
  }

  # Check reference column exists
  if (!x %in% names(A) || !x %in% names(B)) {
    stop("Reference column x must exist in both A and B.")
  }
  
  # Determine columns to map (must exist in both A and B)
  map_cols <- intersect(names(A), names(B))
  if (length(map_cols) == 0) stop("No columns exist in BOTH A and B to map.")
  
  # Identify matching rows by reference column x
  common_vals <- intersect(A[[x]], B[[x]])
  if (length(common_vals) == 0) stop("No matching values in reference column x.")
  
  # Pull vectors from A for matching rows
  a_vals <- A[A[[x]] %in% common_vals, map_cols, drop = FALSE]
  
  # Define missing values in B
  is_missing <- c(NA, "", -999, "NAV")
  
  # Loop over columns
  for (col in map_cols) {
    for (val in common_vals) {
      rows_B <- B[[x]] == val
      rows_A <- a_vals[[x]] == val
      
      if (overwrite) {
        B[rows_B, col] <- a_vals[rows_A, col]
      } else {
        # Only replace missing values
        missing_idx <- rows_B & B[[col]] %in% is_missing
        B[missing_idx, col] <- a_vals[rows_A, col]
      }
    }
  }
  
  return(B)
}

  
  
#### make_manual_table ####
## This function is to (1) extract the missing data to make a long table; (2) append new unique rows to the old table; (3) write the combined table into a csv for manual filling.
# cols = columns
# df_old = previous table version; can be a file path or a variable
# df_new = this table to make

make_manual_table <- function(df_old = NULL, df_new) {
  
  # --- 1. Detect whether df_old is a file path ---
  
  df_old_is_path <- is.character(df_old) && !is.null(df_old)
  
  if (df_old_is_path) {
    original_path <- df_old
    df_old <- read.csv(df_old, stringsAsFactors = FALSE)
  }
  
  # --- 2. Identify columns to pivot ---
  cols <- setdiff(names(df_new), c("search_term", "aub_Taxon_common_name_2"))
  
  # --- 3. Create long table of missing values ---
  tbl <- df_new %>%
    mutate(across(all_of(cols), as.character)) %>%
    pivot_longer(
      cols = all_of(cols),
      names_to = "trait",
      values_to = "value"
    ) %>%
    filter(is.na(value) | value %in% c("-999", "NAV")) %>%
    distinct()
  
  # --- 4.1 When df_old is NULL → simple output ---
  if (is.null(df_old)) {
    csv <- tbl
  } else {
    # --- 4.2 When df_old is supplied, compute new rows ---
    new_rows <- tbl %>%
      select(-value) %>%
      anti_join(df_old %>% select(-value)) %>%   # adjust keys if needed
      mutate(value = NA)
    
    # --- 6. Append ---
    csv <- bind_rows(df_old, new_rows)
  }
  
  # --- 7. Filename logic ---
  today   <- format(Sys.time(), "%Y%m%d_%H%M")
  df_name <- deparse(substitute(df_new))
  
  if (df_old_is_path) {
    # Use original filename but append date
    base <- tools::file_path_sans_ext(original_path)
    out_path <- paste0(base, "-", today, ".csv")
  } else {
    # Normal naming
    out_path <- paste0("output/", df_name, "_mtable_", today, ".csv")
  }
  
  write.csv(csv, out_path, row.names = FALSE)
  return(csv)
}

#### score_nest ####
# This function uses a lookup table to score nest type for sensitivity analysis

score_nest <- function(x) {
  
  # The look up table
  nest_lookup <- data.frame(
    code = c("BU","CP","CR","CV","DM","HC","NO","O","PL","PN","SA","SC","SP","M"),
    category = c("Cavity","Open","Cavity","Cavity","Enclosed","Open","Open",
                 "Open","Open","Enclosed","Open","Open","Enclosed","Enclosed"),
    score = c(1,3,1,1,2,3,3,3,3,2,3,3,2,2),   # 1=cavity, 2=enclosed, 3=open
    stringsAsFactors = FALSE
  )
  
  if (is.na(x)) return(NA_real_)
  codes <- trimws(strsplit(x, ",")[[1]])
  scores <- nest_lookup$score[match(codes, nest_lookup$code)]
  max(scores, na.rm = TRUE)  # choose max
}

#### clean_strtypes ####
## This function assigns correct data type (numeric, integer, character) based on the content of the column.

clean_strtypes <- function(df) {
  
  df[] <- lapply(df, function(col) {
    
    # If already numeric, keep as numeric
    if (is.numeric(col)) return(col)
    
    # Convert to character (ensures consistent handling)
    col_chr <- as.character(col)
    
    # Case 1: all values are integers (no decimals, only digits or NA)
    if (all(grepl("^\\d+$", col_chr[!is.na(col_chr)]) | is.na(col_chr))) {
      return(as.integer(col_chr))
    }
    
    # Case 2: all values are numeric (allow decimals)
    if (all(grepl("^\\d*\\.?\\d+$", col_chr[!is.na(col_chr)]) | is.na(col_chr))) {
      return(as.numeric(col_chr))
    }
    
    # Otherwise: keep as character
    return(col_chr)
  })
  
  return(df)
}


#### scale_aubird_minmax ####
## This function gets the min and max of a continuous trait from all the Australian birds and scale the values of the final species into 0 and 1.

scale_aubird_minmax <- function(sp_col, df, df_col) {
  
  col_sym <- sym(paste0("minmax_", df_col))
  
  value <- australian_birds %>%
    mutate(search_term = paste(Genus, Species)) %>%
    select(search_term) %>%
    distinct() %>%
    join_database(df, prefix = "minmax_") %>%
    summarise(
      min = min(!!col_sym, na.rm = TRUE),
      max = max(!!col_sym, na.rm = TRUE)
    )
  
  scaled_col <- (sp_col - value$min) / (value$max - value$min)
  
  return(list(col = scaled_col, minmax = value))
}

#### calc_diet_diversity ####

## This function calculates simpson and shannon biodiversity index for diet and habitat breadth

calc_diversity <- function(df, cols) {
  
  calc_df <- df %>%
    mutate(across(where(is.integer), ~as.numeric(.))) %>% 
    rowwise() %>%
    mutate(
      total = sum(c_across(all_of(cols)), na.rm = TRUE),
      p = list(c_across(all_of(cols)) / total),
      
      # Shannon: -Σ p * ln(p)
      shannon = -sum(p * log(p), na.rm = TRUE),
      
      # Simpson: 1 - Σ p²  (Gini–Simpson)
      simpson = 1 - sum(p^2, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    select(shannon, simpson)
  
  return(calc_df)
}

