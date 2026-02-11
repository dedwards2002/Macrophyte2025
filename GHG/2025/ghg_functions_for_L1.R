# Functions for processing GHG files
# By: Adrienne Breef-Pilz
# Created on 22 Nov 23
# Edited: 29 Aug 2025

# This is the source script for the functions used in the L1 script to process the GHG files. 


 ### 1. Function to read in files from the GC ####
# Create a function that reads in the file from the GC, selects the columns we want, get clean vial numbers, 
# and get notes from the vial numbers

read_ghg_files<-function(FILES){
  
  df <- read_excel(FILES, skip=7, col_names=T)%>%
    select("Date Acquired...3","Sample Name...4",
           "Season specific ranged CAL Measured headspace CH4  in ppm from GC in ppm",
           "Season specific CAL Measured headspace CO2 in ppm from GC in ppm", Note)%>%
    dplyr::rename("date_acquired"="Date Acquired...3",
                  "vial_number"="Sample Name...4",
                  "CH4_GC_headspace_ppm"="Season specific ranged CAL Measured headspace CH4  in ppm from GC in ppm",
                  "CO2_GC_headspace_ppm"="Season specific CAL Measured headspace CO2 in ppm from GC in ppm")%>%
    filter(grepl("^[0-9]", vial_number))%>%
    mutate(clean_vial_number=as.numeric(gsub(" .*$|_.*$|[a-z]", "", vial_number)), # take out letters in vial number
           notes_from_vial_number= gsub("\\d+", "", vial_number),
           CH4_GC_headspace_ppm = as.numeric(gsub("nd", NA, CH4_GC_headspace_ppm)), # if headspace labeled nd change it to NA
           CO2_GC_headspace_ppm = as.numeric(gsub("nd", NA, CO2_GC_headspace_ppm)),
           count = nchar(gsub("[^0-9]+", "", clean_vial_number)), # count the number of digits and if there are too many then it is caught in the if statement below
           notes = coalesce(Note, notes_from_vial_number))%>%
    select(!c(Note, notes_from_vial_number))
  
  print(paste0("Read in ", FILES))
  
  # There are numbers over 3 digits then duplicate the row and split the numbers
 # for (i in 1:nrow(df)){
  #   if(df$count[i]>3){
  #     fg <- df[i,]
  #     
  #     # now allit up the vial numbers
  #     # only take the first three and replace in the data frame
  #     df[i,"clean_vial_number"] <-as.numeric(sub("\\D*(\\d{3}).*", "\\1.", df$clean_vial_number[i]))
  #     
  #     # now we just want the last three digits because we took the first three
  #     fg[1,"clean_vial_number"]<-as.numeric(str_sub(fg$clean_vial_number[1], start= -3))
  #     
  #     # bind everything together so we have the duplicate  
  #     df<-bind_rows(df,fg)
  #     
  #   }
  # }  
  # Now that we have the correct number of digits we should recount. Do we need to do this?
  #df$count<- nchar(gsub("[^0-9]+", "", df$clean_vial_number))
  
  return(df)
}     

### 2. Function to get final GHG concentrations ####

# This is Bobbies GHG_MEGA_GC_SHEET_EXCEL in R form for CH4 and CO2 
# The final concentration is what is on EDI

ghg_concentration <- function(
      raw_file,
      headspace = 0.002,
      temp_headspace = 313.15,
      CH4_used_in_headspacePrep = 0,
      CO2_used_in_heaspacePrep = 0
      
  ){
  
  # For both CO2 and CH4
  
  concentration <- raw_file %>%
    mutate(
    # Head space volume in Liters
    headspace_L = headspace, #input to function
    
    # Volume of water in Liters
    vol_water_L = (21.0733341666667/1000)-headspace_L,
    
    # Temperature in Kelvin at the time when headspace concentration is equilibrated
    Temp_headspace_K = temp_headspace, #input to function
    
    # Total pressure in atm- calculated from amount of HS 
    # added at room temp then adjusted to pressure at agitator temperature of 40 C (p2 = (t2 *p1)/t1) 
    tot_pressure_atm = ((0.001316*((weather_station_bp*25.4)-(2.5*2053/100)))*
                          (273.15+40))/(273.15+lab_temp),
    
    
    
    ## Just for CH4
    
    # Partial Pressure for the CH4 gas
    CH4_partial_pressure = tot_pressure_atm*(CH4_GC_headspace_ppm/10^6),
    
    
    # mols compound in CH4 gas (calculated from partial pressure)
    CH4_mol = (CH4_partial_pressure*headspace_L)/(0.082057*Temp_headspace_K),
    
    # Unnamed column. Check with Bobbie but used to calculate the mol compund in water
    hen_law_CH4 = 101.325*(0.000014*exp(1600*((1/Temp_headspace_K)-(1/298.15)))),
    
    
    # mol compound in water (estimated from Henry's)
    CH4_mol_water = CH4_partial_pressure*hen_law_CH4*vol_water_L,
    
    # Mols of CH4 gas used to make the headspace. Needs to be taken out of the final concentration
    CH4_headspace_mol = CH4_used_in_headspacePrep,
    
    # mols of CH4 in gas in vessel (calculated from partial pressure)
    CH4_tot_vessel = CH4_mol + CH4_mol_water,
    
    ### Total Concentration of CH4.  
    # Total concentration of CH4 in the original aqueous sample in µmol/L
    CH4_umolL = 10^6*(CH4_tot_vessel-CH4_headspace_mol)/vol_water_L,
    
    
    ## Just for CO2 
    
    # Partial Pressure for CO2 gas from amount of CO2 in the headspace
    CO2_partial_pressure = tot_pressure_atm*(CO2_GC_headspace_ppm/10^6),
    
    # mols of CO2 in gas (estimated from Henry's)
    CO2_mol = (CO2_partial_pressure*headspace_L)/(0.082057*Temp_headspace_K),
    
    
    # Unnamed column. Check with Bobbie but used to calculate the mol compund in water
    hen_law_CO2 = 101.325*(0.00033*exp(2400*((1/Temp_headspace_K)-(1/298.15)))),
    
    # mol of CO2 in water
    CO2_mol_water = CO2_partial_pressure*hen_law_CO2*vol_water_L,
    
    # Total mols of CO2 in gas in vessel (calculated from partial pressure)
    CO2_tot_vessel = CO2_mol + CO2_mol_water,
    
    # mols compound in gas used to make headspace
    CO2_headspace_mol = CO2_used_in_heaspacePrep,
    
    ### Total Concentration of CH4.  
    # Total concentration of CH4 in the original aqueous sample in µmol/L
    CO2_umolL = 10^6*(CO2_tot_vessel-CO2_headspace_mol)/vol_water_L
  )
  
  return(concentration)
  
}
