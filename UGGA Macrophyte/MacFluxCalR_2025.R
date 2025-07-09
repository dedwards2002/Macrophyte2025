## Script to use FluxCalR for calculating fluxes from UGGA data
## A. Hounshell, 25 Jan 2021
## Substantially modified by A. Lewis in January and April of 2024
## ABP adapted for 2025

# Script following: https://github.com/junbinzhao/FluxCalR

# Load in 'remotes' package
pacman::p_load(remotes,tidyverse,lubridate)

# Install FluxCalR (if not already installed! If installed, skip to library(FluxCalR))
remotes::install_github("junbinzhao/FluxCalR",build_vignettes = TRUE)
library(FluxCalR)

# Load in data: will need to load in individual files - I recommend doing this by year

wd <- setwd("./UGGA Macrophyte")
# You'll want to save this script in the same working directory to keep a record of what files
# you have corrected.

field_sheet <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1K-SMsMnMQ7To9xV7S6b1EcRrJFnnvf2ci2OHAWadBps/")

# Here, we create a function that will be used for processing each file. 
# You don't need to modify this
process_txt <- function(file,
                        file_name = sub(".txt","",file),
                        reservoir = field_sheet$Reservoir[field_sheet$file == file_name],
                        site = field_sheet$Site[field_sheet$file == file_name],
                        date = field_sheet$Date[field_sheet$file == file_name],
                        time = field_sheet$Time[field_sheet$file == file_name]) {
  file=files_to_process[3]
  file_name = sub(".txt","",file)
  reservoir = field_sheet$Reservoir[field_sheet$file == file_name]
  site = field_sheet$Site[field_sheet$file == file_name]
  date = field_sheet$Date[field_sheet$file == file_name]
  time = field_sheet$Time[field_sheet$file == file_name]
  
  
  message("Processing ", file_name,"
          Reservoir: ", reservoir, "
          Site: ", site, "
          Date: ", date)
  flux_lgr <- LoadLGR(file = file,
                      time_format = "mdy_HMS")
  
  # Ask user to select times when the UGGA was on/off the water
  time_cue <- SelCue(flux_lgr,flux="CH4", cue="End", save=F) %>%
    mutate(Reservoir = reservoir, 
           Site = site, 
           Date_real = as.Date(date, format="%m/%d/%Y"),
           Time = time)
  
  # Then calculate fluxes
  flux_output <- FluxCal(data = flux_lgr, # Dataframe loaded in
                          win = 4, # Window length = 4 minutes
                          vol = 0.020876028*1000, # Volume of trap in liters
                          area = 0.1451465, # Area of trap in m^2
                          df_cue = time_cue, # End times selected using SelCue
                          cue_type = "End", # Designate that these times are for the end
                          ext = 1, # Multiplier for time window to look at data (5 min x 1 = use full 5 min interval)
                          other = c("Reservoir","Site","Date_real", "Time"),
                          output = FALSE)
  
  save_name <- paste0("processed_csvs/", sub(".txt", ".csv", file))
  write.csv(flux_output, save_name, row.names = F)
}



#Identify all files we SHOULD have (from field sheet)
files <- paste0(field_sheet$File,".txt")
#Identify all files that have already been processed (in the processed_csvs folder)
processed_files <- sub(".csv", ".txt", list.files("processed_csvs"))
#Compare these two to figure out which files still need to be processed
files_to_process <- files[!files %in% processed_files]
#Exclude any files that we have decided not to process (see notes above)
#files_to_process <- files_to_process[!files_to_process %in% c()] # very messy and doesn't seem usable


## RUN THE PROCESSING!!
# Instructions: Use the cursor to select the timepoint before the peak; click once for the first peak and again for
# the second peak. When finished, click on 'Stop' in the upper left-hand corner and then click 'Stop locator'
# This generates a list of 'end' times for each peak saved as time_cue_x
for (file in files_to_process) {
  process_txt(files[2])
}
#As you go, this saves each output file as a csv for later use

# QAQC: there were a few times that there was only one peak but we still had to click twice. 
# Here, we manually remove the second peak from these files
one_peak <- c("processed_csvs/gga_2001-12-31_f0139.csv", # specify casts with one peak. Can still use them. 
  "processed_csvs/gga_2001-12-31_f0177.csv")
#Filter to only the first peak in these files
for (file in one_peak) {
  data <- read.csv(file) %>%
    filter(Num == 1)
  write.csv(data, file, row.names = F)
}

### Compile all data and prepare for output
# Combine all csvs in the processed_csvs folder
flux_output <- read_csv(paste0("processed_csvs/", list.files("processed_csvs")))

#Fix time issues
flux_output2 <- flux_output %>%
  group_by(Date_real, Reservoir, Site) %>%
  mutate(Start = as_datetime(paste0(Date_real, Start)),
         End = as_datetime(paste0(Date_real, End)),
         Time2 = as_datetime(paste0(Date_real, Time)),
         Min_start = min(Start),
         Start = Start - Min_start + Time2,
         End = End - Min_start + Time2,
         Date = Date_real,
         Start = format(Start, format = "%H:%M"),
         End = format(End, format = "%H:%M")) %>%
  ungroup() %>%
  select(-Min_start, -Date_real, -Time2) %>%
  mutate(End_dif = as.POSIXct(End, format = "%H:%M")-as.POSIXct(Start, format = "%H:%M"),
         End = ifelse(is.na(Time),
                           format(as.POSIXct("12:00", format = "%H:%M")+End_dif,"%H:%M"),
                           End),
         Start = ifelse(is.na(Time),"12:00", Start))%>%
dplyr::select(-End_dif, -Time)

# Get together for publication to EDI
flux_co2 <- flux_output2 %>% 
  filter(Gas == "CO2") %>% 
  rename(co2_slope_ppmS = Slope, co2_R2 = R2, co2_flux_umolCm2s = Flux) %>% 
  select(-Gas)

flux_ch4 <- flux_output2 %>% 
  filter(Gas == "CH4") %>% 
  rename(ch4_slope_ppmS = Slope, ch4_R2 = R2, ch4_flux_umolCm2s = Flux) %>% 
  select(-Gas)

flux_all <- left_join(flux_co2,flux_ch4,by=c("Num",
                                             "Date",
                                             "Start",
                                             "End",
                                             "Ta",
                                             "Reservoir",
                                             "Site"))

flux_all <- flux_all %>% 
  rename(Rep = Num, Temp_C = Ta) %>% 
  mutate(co2_flux_umolCm2s_flag = 0, ch4_flux_umolCm2s_flag = 0,
         Start = format(Start, format = "%H:%M"),
         End = format(End, format = "%H:%M"))

# Export out fluxes
write.csv(flux_all,"./2025_season_Flux_Output.csv", row.names = F)
