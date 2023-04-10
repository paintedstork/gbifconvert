library (tidyverse)
library (tidyr)
library (parsedate)

# This should ideally be read from a table
targetSpecies <- data.frame (Species = c (
  "Rufous-necked Hornbill",
  "Indian Grey Hornbill", 
  "Oriental Pied Hornbill",
  "Great Hornbill",
  "Malabar Pied Hornbill",
  "Malabar Grey Hornbill",
  "Wreathed Hornbill",
  "Narcondam Hornbill",
  "White-throated brown Hornbill"),
  ScientificName = c (
    "Aceros nipalensis",
    "Ocyceros birostris", 
    "Anthracoceros albirostris",
    "Buceros bicornis",
    "Anthracoceros coronatus",
    "Ocyceros griseus",
    "Rhyticeros undulatus",
    "Rhyticeros narcondami",
    "Anorrhinus austeni")) 

# Return scientific name based on English name
getScientificName <- function (data, infield, outfield)
{
  temp <- data %>% 
    select (infield) %>% 
    inner_join (targetSpecies, by = "Species") %>% 
    select (ScientificName)
  colnames(temp) <- outfield
  return (temp)
}

# Return genus name based on English name
getGenus <- function (data, infield, outfield)
{
  temp <- getScientificName (data, infield, "ScientificName") %>% 
    separate(ScientificName, c('Genus', 'Species')) %>% 
    select ("Genus")
  colnames(temp) <- outfield
  return (temp)
}

# Return specific name based on English name
getSpecies <- function (data, infield, outfield)
{
  temp <- getScientificName (data, infield, "ScientificName") %>% 
    separate(ScientificName, c('Genus', 'Species')) %>% 
    select ("Species")
  colnames(temp) <- outfield
  return (temp)
}

# Convert date format
Convert2DDMMYYYY <- function (data, infield, outfield)
{
  temp <- data [,infield] %>% 
    parse_datetime(format = "%d %B, %Y") %>% 
    as.character.Date(format = "%d%m%Y") %>% 
    as.data.frame()
  colnames(temp) <- outfield
  return (temp)
}

# Convert with time and date appended
Convert2UTCWithDate <- function (data, infield, outfield)
{
  temp <- Convert2DDMMYYYY (data, "Observed.Date", "ODate")
  temp <- paste (temp$ODate, data [,infield]) %>% as.data.frame()
  colnames(temp) <- outfield
  return (temp)
}
