library (xlsx)
library (dplyr)

# This is a generic file that can be used by any occurrence dataset

# File names and other parameters specific to the program are here
source ("configurations.R")

# All individual conversion functions specific to the program are here
source ("convertFunctions.R")

# Read Data
data <- read.xlsx(inputfile, sheetIndex = datasheetindex)

format <- read.xlsx(inputfile, sheetIndex = formatsheetindex)

# Replace space by dot. Thats how R reads the columns
format$Source <- gsub(" ", ".", format$Source)

# Convert entire column into GBIF format - input is columnName
# This is a generic function, not specific to Hornbill Watch
convertField <- function (columnName, data, format)
{
  print(paste("Converting",columnName))
  temp <- NULL
  selFormat <- format %>% filter (GBIF == columnName)
  
  if(!(selFormat$Mandatory %>% is.na()) && (selFormat$Mandatory == "Yes"))
  { #Mandatory GBIF fields
    if(is.na(selFormat$Source))
    { #Does not exist in source file, copy default value
      temp <- rep(selFormat$DefaultValue,times=nrow(data)) %>% as.data.frame()
      colnames(temp) <- columnName
    }
    else
    {
      if(is.na(selFormat$Conversion))
      { # No format conversion specified 
        if(is.na(selFormat$Prefix))
        { #Plain copy
          temp <- data %>% select (selFormat$Source) 
          colnames(temp) <- columnName
        }
        else
        { #Prepend the prefix
          temp <- paste0 (rep(selFormat$Prefix, nrow(data)), data [,selFormat$Source]) %>% as.data.frame()
          colnames(temp) <- columnName
        }
      }
      else
      { # Call format conversion function with all required values
        temp <- do.call(selFormat$Conversion, list (data = data, infield = selFormat$Source, outfield = columnName))
      }
    }
  }
  else
  { #Non-mandatory GBIF fields
    if(!is.na(selFormat$Source))
    {  # No format conversion specified 
      if(is.na(selFormat$Conversion))
      { 
        if(is.na(selFormat$Prefix))
        {  #Plain copy
          temp <- data %>% select (selFormat$Source) 
          colnames(temp) <- columnName
        }
        else
        {  #Prepend the prefix
          temp <- paste0 (rep(selFormat$Prefix, nrow(data)), data [,selFormat$Source]) %>% as.data.frame()
          colnames(temp) <- columnName
        }
      }
      else
      { # Call format conversion function with all required values
        temp <- do.call(selFormat$Conversion, list (data = data, infield = selFormat$Source, outfield = columnName))
      }
    }
  }
  return (temp)
}

# Convert entire measurement column into GBIF format - input is sourceName
# This is a generic function, not specific to Hornbill Watch
convertMeasurement <- function (sourceName, data, format)
{
  print(paste("Converting",sourceName))
  selFormat <- format %>% filter (Source == sourceName)
  temp <- convertField ("occurrenceID", data, format)
  
  if(!is.na(selFormat$Conversion))
  { # Conversion function should exists 
    mValue <- do.call(selFormat$Conversion, list (data = data, infield = selFormat$Source, outfield = "measurementValue"))
  }
  else
  { # No Conversion function should exists
    if(is.na(selFormat$Prefix))
    {  #Plain copy
      mValue <- data %>% select (selFormat$Source) 
    }
    else
    {  #Prepend the prefix
      mValue <- paste0 (rep(selFormat$Prefix, nrow(data)), data [,selFormat$Source]) %>% as.data.frame()
    }
  }
  temp <- temp %>% cbind (rep(selFormat$Source, nrow(data)) %>% as.data.frame(), mValue) %>% as.data.frame()
  colnames(temp) <- c("occurrenceID",	"measurementType",	"measurementValue")
  return (temp)
}


# Apply the convert function over every non-empty fields of GBIF
GBIFOut <- mapply(convertField, format$GBIF[format$GBIF != "measurement"] %>% na.omit(), MoreArgs = list (data = data, format = format)) %>% as.data.frame()
colnames(GBIFOut) <- format$GBIF[format$GBIF != "measurement"] %>% na.omit()

write.csv(GBIFOut, outfile_gbif, row.names = FALSE)

if (format$Source[format$GBIF == "measurement"] %>% na.omit() %>% length() > 0)
{
  GBIFOutMeas <- mapply(convertMeasurement, format$Source[format$GBIF == "measurement"] %>% na.omit(), MoreArgs = list (data = data, format = format), SIMPLIFY = FALSE)
  GBIFOutMeas <- do.call (rbind, GBIFOutMeas)
  
  write.csv(GBIFOutMeas, outfile_gbif_meas, row.names = FALSE)
}

print ("Conversion Complete")
