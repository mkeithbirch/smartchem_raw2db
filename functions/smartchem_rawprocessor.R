smartchem_rawprocessor <- function(filename) {
  path <- file.path("raw_data", filename) # Define path to workbook
  scdatawb <- lapply(excel_sheets(path = path), read_excel, path = path) # Load in workbook as a list of worksheets (tibbles)
  method <- scdatawb[[5]][[1,1]] # Extract the method name (BNO3, SMPL, WNHA, etc.) and save it as an object "method"
  run_date <- (run_dates_df %>% filter(filenames_raw == filename) %>% select(run_dates))[[1]] # Extract run_date for this file
  filecode <- sub(".XLS","",filename, ignore.case = T) # create filecode for tracking
  # Extract Results sheet from workbook; add run_date and combine with RunTime as POSIX (datetime) type, add method name
  result <- scdatawb[[1]] %>% mutate(run_date = date(run_date), RunTime = ymd_hms(paste(run_date, RunTime)), method = method)
  
  # Extract table of sample results (exclude controls and dups)
  result_samples <- result %>% filter(SampleType == 0, !grepl("Dup", SampleID))
  
  # Extract table of dups
  result_dups <- result %>% filter(SampleType == 0, Position == dplyr::lead(Position)) %>% 
    bind_rows(result %>% filter(SampleType == 0, Position == dplyr::lag(Position))) %>% arrange(Position)
  # Summarize dups by percent difference and absolute difference
  result_dups <- result_dups %>% group_by(Position) %>% 
    mutate(pct_diff_conc = 100*(Concentration-lag(Concentration))/((Concentration+lag(Concentration))/2), 
           diff_conc = Concentration - lag(Concentration)) %>% 
    ungroup() %>% 
    filter(grepl("Dup", SampleID))# filter to include only the dups
  
  # Extract Controls sheet from workbook; Add run_date to RunTime and define RunTime variable as POSIX type
  controls <- scdatawb[[4]] %>% mutate(run_date = run_date, RunTime = ymd_hms(paste(run_date, RunTime)))
  # For nitrate method, add coil_efficiency results
  if(method == "BNO3") {
    controls <- controls %>% arrange(RunTime) %>% mutate(coil_effic = ifelse(SampleID == "CCN3", (Abs/Nominal)/(lag(Abs)/lag(Nominal)), NA))
  }

# Output a list object of each of the processed data.frames  
  return(list(result = result_samples, 
              dups = result_dups,
              controls = controls,
              calibrants = scdatawb[[3]],
              rbl = mean(scdatawb[[2]]$Abs),
              method = method,
              run_date = run_date))
}