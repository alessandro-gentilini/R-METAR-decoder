# R METAR decoder

# Author: Alessandro Gentilini - Copyright 2013

# METAR decoder based on the volume:
#
# Manual on Codes
# International Codes
# Volume I.1
# (Annex II to WMO Technical Regulations)
# Part A – Alphanumeric Codes
# WMO-No. 306
# 2011 edition
# Updated in 2012
# ftp://ftp.wmo.int/Documents/MediaPublic/Publications/CodesManual_WMO_No_306/WMO306_Vol_I.1_2012_en.pdf (accessed 20130528)
#
# from page A-27 to A-38

# Some METAR samples:

# http://www.wunderground.com/metarFAQ.asp
wu = "METAR  KORD	041656Z	19020G26KT	6SM	-SHRA	BKN070	12/08	A3016	RMK AO2"



ICAO_station_length = 4
YYGGggZ_length = 7

extract_ICAO_location_indicator = function(CCCC)
{
  ICAO_location_indicator = ""
  if ( nchar(CCCC) == ICAO_station_length ) {
    ICAO_location_indicator = CCCC
  } else {
    stop(paste("Expected an 'ICAO location indicator', found",CCCC))  
  }
  ICAO_location_indicator
}

extract_ddhhmm = function(YYGGggZ)
{
  if ( nchar(YYGGggZ) != YYGGggZ_length || substr(YYGGggZ,7,7) != 'Z' ) {
    stop(paste("Expected the YYGGggZ group, found",YYGGggZ))
  }
  dd = as.numeric(substr(YYGGggZ,1,2))
  hh = as.numeric(substr(YYGGggZ,3,4))
  mm = as.numeric(substr(YYGGggZ,5,6))
  data.frame(dd,hh,mm)
}

extract_wind_info = function(dddffGfmfm)
{
  CALM = F
  VRB = F
  direction = NA
  speed = 0
  GUST = F
  gust_speed = 0
  wind_speed_UOM = "MPS"
  if ( substr(dddffGfmfm,1,5)=="00000" ) {
    CALM = T
    VRB = F
    direction = NA
    speed = 0
  } else if ( substr(dddffGfmfm,1,3)=="VRB" ) {
    CALM = F
    VRB = T
    direction = NA
    speed = substr(dddffGfmfm,4,5)
  } else {
    CALM = F
    VRB = F
    direction = as.numeric(substr(dddffGfmfm,1,3))
    speed = as.numeric(substr(dddffGfmfm,4,5))
  }
  
  if ( nchar(dddffGfmfm) > 5 ) {
    if ( substr(dddffGfmfm,6,6)=='G' ) {
      GUST = T
      gust_speed = as.numeric(substr(dddffGfmfm,7,8))
      if ( nchar(dddffGfmfm) == 10 && substr(dddffGfmfm,9,10) == "KT") {
        wind_speed_UOM = "KT"
      } else if ( nchar(dddffGfmfm) == 11 && substr(dddffGfmfm,9,11) == "MPS") {
        wind_speed_UOM = "MPS"
      } else {
        stop(paste("Expected dddffGfmfm group, found",dddffGfmfm))  
      }
    } else {
      if ( nchar(dddffGfmfm)==7 && substr(dddffGfmfm,6,7) == "KT" ) {
        wind_speed_UOM = "KT"
      } else if ( nchar(dddffGfmfm)==8 && substr(dddffGfmfm,6,8) == "MPS") {
        wind_speed_UOM = "MPS"
      } else {
        stop(paste("Expected dddffGfmfm group, found",dddffGfmfm))
      }
    }
  }
  data.frame(CALM,VRB,direction,speed,GUST,gust_speed,wind_speed_UOM)
}


metar_decoder = function(metar_string)
{
  groups = scan(what=character(),text=metar_string)
  
  next_index = 0
  
  METAR = T
  SPECI = F
  COR = F
  ICAO_location_indicator = ""
  
  if ( groups[1] == "METAR") {
    METAR = T
    SPECI = F
    if ( groups[2] == "COR" ) {
      COR = T
      ICAO_location_indicator = extract_ICAO_location_indicator(groups[3])
      next_index = 4
    } else {
      COR = F
      ICAO_location_indicator = extract_ICAO_location_indicator(groups[2])
      next_index = 3
    }
  } else if ( groups[1] == "SPECI" ) {
    METAR = F
    SPECI = T
    if ( groups[2] == "COR" ) {
      COR = T
      ICAO_location_indicator = extract_ICAO_location_indicator(groups[3])
      next_index = 4
    } else {
      COR = F
      ICAO_location_indicator = extract_ICAO_location_indicator(groups[2])
      next_index = 3
    }    
  } else if ( groups[1] == "COR" ) {
    METAR = F
    SPECI = F
    COR = T
    ICAO_location_indicator = extract_ICAO_location_indicator(groups[2])
    next_index = 3
  } else {
    METAR = F
    SPECI = F
    COR = F
    ICAO_location_indicator = extract_ICAO_location_indicator(groups[1])    
    next_index = 2
  }
  
  ddhhmm = extract_ddhhmm(groups[next_index])
  day = ddhhmm$dd
  hour = ddhhmm$hh
  minute = ddhhmm$mm
  
  next_index = next_index + 1
  
  NIL = F
  AUTO = F
  wind_info = data.frame()
  if ( groups[next_index] == "NIL" ) {
    NIL = T
    AUTO = F
    wind_info = extract_wind_info(groups[next_index+1])
    next_index = next_index+2
  } else if ( groups[next_index] == "AUTO" ) {
    NIL = F
    AUTO = T
    wind_info = extract_wind_info(groups[next_index+1])
    next_index = next_index+2
  } else {
    NIL = F
    AUTO = F
    wind_info = extract_wind_info(groups[next_index])
    next_index = next_index+1    
  }
  
  CALM = wind_info$CALM
  VRB = wind_info$VRB
  direction = wind_info$direction
  speed = wind_info$speed
  GUST = wind_info$GUST
  gust_speed = wind_info$gust_speed
  wind_speed_UOM = wind_info$wind_speed_UOM
  
  print(paste("CALM",CALM))
  data.frame(METAR,SPECI,COR,ICAO_location_indicator,day,hour,minute,NIL,AUTO,CALM,VRB,direction,speed,GUST,gust_speed,wind_speed_UOM)
}

str(metar_decoder(wu))

