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

# FEDERAL COORDINATOR
# FOR
# METEOROLOGICAL SERVICES AND
# SUPPORTING RESEARCH
# 8455 COLESVILLE ROAD, SUITE 1500
# SILVER SPRING, MARYLAND 20910
# 301-427-2002
# www.ofcm.gov
# FEDERAL METEOROLOGICAL HANDBOOK
# NUMBER 1
# SURFACE WEATHER OBSERVATIONS AND REPORTS
# FCM-H1-2005
# Washington, D.C.
# September 2005
# http://www.ofcm.gov/fmh-1/pdf/FMH1.pdf (accessed 20130529)


# Some METAR samples:

# http://www.wunderground.com/metarFAQ.asp
wu = "METAR  KORD	041656Z	19020G26KT	6SM	-SHRA	BKN070	12/08	A3016	RMK AO2"

# http://aviationweather.gov/adds/metars/
lipe = "LIPE 282020Z 13006KT 9999 FEW080 15/13 Q1004"
live = "LIVE 281955Z 16005KT 9999 BKN030 08/05 Q1004 RMK BKN VIS MIN 9999"
live_1 = "LIVE 291055Z 02006KT 3000 -RA BR SCT010 BKN020 03/M01 Q0999 RMK OVC VIS MIN 3000"
birk = "BIRK 282000Z 17009KT 9999 FEW032 SCT050 10/04 Q1009"
llbg = "LLBG 282020Z VRB03KT CAVOK 22/16 Q1016 NOSIG"
licr = "LICR 281950Z 02016KT 9999 FEW035 20/13 Q1004 RMK VIS MIN 9999"
kslc = "KSLC 282040Z 29006KT 5SM +RA BR SCT011 BKN020 OVC041 12/10 A2983 RMK AO2 P0012"
eidw = "EIDW 282030Z 35007KT 9999 FEW015 BKN100 09/07 Q1004 NOSIG"
paed = "PAED 291155Z AUTO 00000KT 10SM CLR 11/06 A2966 RMK AO2 SLP045 T01090057 10184 20105 51004"
klxv = "KLXV 291130Z AUTO 07005KT 1/4SM +SN FG VV010 00/M01 A2991 RMK AO2 P0002 FZRANO"
klxv_1 = "KLXV 291114Z AUTO 36003KT 3/4SM -SN BKN019 OVC035 01/M02 A2992 RMK AO2 P0000 FZRANO"
klxv_2 = "KLXV 291110Z AUTO 33004KT 2 1/2SM -SN BKN024 OVC035 01/M02 A2991 RMK AO2 P0000"
kccu = "KCCU 291112Z AUTO 24009KT 180V280 1 3/4SM -SN BKN002 BKN006 OVC011 01/M01 A2998 RMK AO2 LTG DSNT NE"

FMH_table_12_1_visibility = c("M1/4","1/4","1/2","3/4","1","1 1/4","1 1/2","1 3/4","2","2 1/2","3","4","5","6","7","8","9","10",
                              "0","1/16","1/8","3/16","1/4","5/16","3/8","1/2","5/8","3/4","7/8","1","1 1/8","1 1/4","1 3/8","1 1/2","1 5/8","1 3/4","1 7/8","2","2 1/4","2 1/2","2 3/4","3","4","5","6","7","8","9","10","11","12","13","14","15","20","25","30","35")


recognize_METAR = function(field)
{
  return(grepl("METAR",field,fixed=T))
}

extract_METAR = function(field)
{
  return(grepl("METAR",field,fixed=T))
}

recognize_SPECI = function(field)
{
  return(grepl("SPECI",field,fixed=T))
}

extract_SPECI = function(field)
{
  return(grepl("SPECI",field,fixed=T))
}

recognize_COR = function(field)
{
  return(grepl("COR",field,fixed=T))
}

extract_COR = function(field)
{
  return(grepl("COR",field,fixed=T))
}

recognize_ICAO_location_indicator = function(field)
{
  return(grepl("[A-Z][A-Z][A-Z][A-Z]",field))
}

extract_ICAO_location_indicator = function(field)
{
  return(field)
}

recognize_timestamp = function(field)
{
  return(grepl("[0-9][0-9][0-9][0-9][0-9][0-9]Z",field))
}

extract_timestamp = function(field)
{
  dd = as.numeric(substr(field,1,2))
  hh = as.numeric(substr(field,3,4))
  mm = as.numeric(substr(field,5,6))
  return(data.frame(dd,hh,mm))
}

recognize_NIL = function(field)
{
  return(grepl("NIL",field,fixed=T))
}

extract_NIL = function(field)
{
  return(grepl("NIL",field,fixed=T))
}

recognize_AUTO = function(field)
{
  return(grepl("AUTO",field,fixed=T))
}

extract_AUTO = function(field)
{
  return(grepl("AUTO",field,fixed=T))
}

recognize_wind = function(field)
{
  return( grepl("00000KT" ,field,fixed=T) || 
          grepl("00000MPS",field,fixed=T) ||
          grepl("[0-9][0-9][0-9][0-9][0-9]KT" , field) ||
          grepl("[0-9][0-9][0-9][0-9][0-9]MPS", field) || 
          grepl("VRB[0-9][0-9]KT" , field) ||
          grepl("VRB[0-9][0-9]MPS", field) ||
          grepl("[0-9][0-9][0-9][0-9][0-9]G[0-9][0-9]KT" , field) ||
          grepl("[0-9][0-9][0-9][0-9][0-9]G[0-9][0-9]MPS", field) 
          
  )
}

extract_wind = function(field)
{
  CALM = F
  UOM = "MPS"
  speed = NA
  direction = NA
  VRB = F
  GUST = F
  gust_speed = NA
  if( grepl("00000KT",field,fixed=T) ) {
    CALM = T
    UOM = "KT"
  } else if( grepl("00000MPS",field,fixed=T) ) {
    CALM = T
    UOM = "MPS"
  } else if( grepl("[0-9][0-9][0-9][0-9][0-9]KT" , field) ) {
    res = regexec("([0-9][0-9][0-9])([0-9][0-9])KT", field)
    direction = as.numeric(regmatches(field,res)[[1]][[2]])
    speed = as.numeric(regmatches(field,res)[[1]][[3]])
    UOM = "MPS"
  } else if( grepl("[0-9][0-9][0-9][0-9][0-9]MPS", field) ) {
    res = regexec("([0-9][0-9][0-9])([0-9][0-9])MPS", field)
    direction = as.numeric(regmatches(field,res)[[1]][[2]])
    speed = as.numeric(regmatches(field,res)[[1]][[3]])
    UOM = "MPS"
  } else if( grepl("VRB[0-9][0-9]KT" , field) ) {
    res = regexec("VRB([0-9][0-9])KT", field)
    speed = as.numeric(regmatches(field,res)[[1]][[2]])
    VRB = T
    UOM = "KT"
  } else if( grepl("VRB[0-9][0-9]MPS", field) ) {
    res = regexec("VRB([0-9][0-9])MPS", field)
    speed = as.numeric(regmatches(field,res)[[1]][[2]])
    VRB = T
    UOM = "MPS"
  } else if( grepl("[0-9][0-9][0-9][0-9][0-9]G[0-9][0-9]KT" , field) ) {
    res = regexec("([0-9][0-9][0-9])([0-9][0-9])G([0-9][0-9])KT" , field)
    direction = as.numeric(regmatches(field,res)[[1]][[2]])
    speed = as.numeric(regmatches(field,res)[[1]][[3]])
    gust_speed = as.numeric(regmatches(field,res)[[1]][[4]])
    GUST = T
    UOM = "KT"
  } else if( grepl("[0-9][0-9][0-9][0-9][0-9]G[0-9][0-9]MPS", field) ) {
    res = regexec("([0-9][0-9][0-9])([0-9][0-9])G([0-9][0-9])MPS" , field)
    direction = as.numeric(regmatches(field,res)[[1]][[2]])
    speed = as.numeric(regmatches(field,res)[[1]][[3]])
    gust_speed = as.numeric(regmatches(field,res)[[1]][[4]])
    GUST = T
    UOM = "MPS"
  }
  return(data.frame(CALM,UOM,speed,direction,VRB,GUST,gust_speed))
}

recognize_wind_direction_variation = function(field)
{
  return(grepl("[0-9][0-9][0-9]V[0-9][0-9][0-9]",field))
}

extract_wind_direction_variation = function(field)
{
  res = regexec("([0-9][0-9][0-9])V([0-9][0-9][0-9])",field)
  WIND_DIRECTION_VARIATION = T
  extreme_wind_direction_n = as.numeric(regmatches(field,res)[[1]][[2]])
  extreme_wind_direction_x = as.numeric(regmatches(field,res)[[1]][[3]])
  return(data.frame(WIND_DIRECTION_VARIATION,extreme_wind_direction_n,extreme_wind_direction_x))
}

recognize_visibility = function(field)
{
  return(grepl("[0-9][0-9][0-9][0-9]",field)||
           grepl(".*SM",field) ||
           grepl("CAVOK",field,fixed=T))
}

extract_visibility = function(field)
{
  visibility = NA
  UOM = NA
  CAVOK = F
  if ( grepl("[0-9][0-9][0-9][0-9]",field) ) {
    res = regexec("([0-9][0-9][0-9][0-9])",field)
    visibility = as.numeric(regmatches(field,res)[[1]][[2]])
    UOM = "M"
  } else if (grepl(".*SM",field)) {
    res = regexec("(.*)SM",field)
    visibility = as.numeric(eval(parse(text=regmatches(field,res)[[1]][[2]])))
    UOM = "SM"
  } else if ( grepl("CAVOK",field,fixed=T)){
    CAVOK = T
  }
  return(data.frame(visibility,UOM,CAVOK))
}


parse_field = function(field,index,recognizer,extractor,is_compulsory,field_description)
{
  data = NA
  found_optional_field = F
  if ( recognizer(field) ) {
    data = extractor(field)
    index = index + 1
    found_optional_field = T
  } else {
    if ( is_compulsory ) {
      stop(sprintf("Expected compulsory field '%s', found '%s'.",field_description,field))
    }  
  }
  return(data.frame(data,index,found_optional_field))
}

fix_white_space_in_FMH_visibility = function(metar_string)
{
  re1="\\d";  # Any Single Digit 1
  re2="(\\s+)";  # White Space 1
  re3="\\d";  # Any Single Digit 2
  re4="\\/";  # Any Single Character 1
  re5="\\d";	# Any Single Digit 3
  re6="S";	# Any Single Word Character (Not Whitespace) 1
  re7="M";	# Any Single Word Character (Not Whitespace) 2
  re = paste(re1,re2,re3,re4,re5,re6,re7,sep="")
  result = regexec(re,metar_string)
  if ( result[[1]][[1]] != -1 ) {
    white_space_position = result[[1]][[2]]
    substr(metar_string,white_space_position,white_space_position) = '+'
  }
  return(metar_string)
}



metar_decoder = function(metar_string,low_visibility=1/32)
{
  metar_string = fix_white_space_in_FMH_visibility(metar_string)
  groups = scan(what=character(),text=metar_string)
  
  METAR = NA
  df = parse_field(groups[1],1,recognize_METAR,extract_METAR,F, "METAR")
  METAR = df$data
  
  SPECI = NA
  df = parse_field(groups[df$index],df$index,recognize_SPECI,extract_SPECI,F, "SPECI")
  SPECI = df$data
  
  COR = NA
  df = parse_field(groups[df$index],df$index,recognize_COR,extract_COR,F, "COR")
  COR = df$data
  
  ICAO_location_indicator = NA
  df = parse_field(groups[df$index],df$index,recognize_ICAO_location_indicator,extract_ICAO_location_indicator,T, "ICAO_location_indicator")
  ICAO_location_indicator = df$data
  
  day = NA
  hour = NA
  minute = NA
  df = parse_field(groups[df$index],df$index,recognize_timestamp,extract_timestamp,T, "timestamp")
  day = df$dd
  hour = df$hh
  minute = df$mm
  
  NIL = NA
  df = parse_field(groups[df$index],df$index,recognize_NIL,extract_NIL,F, "NIL")
  NIL = df$data  
  
  AUTO = NA
  df = parse_field(groups[df$index],df$index,recognize_AUTO,extract_AUTO,F, "AUTO")
  AUTO = df$data  
  
  CALM = F
  UOM = "MPS"
  speed = NA
  direction = NA
  VRB = F
  GUST = F
  gust_speed = NA
  df = parse_field(groups[df$index],df$index,recognize_wind,extract_wind,T, "AUTO")
  CALM = df$CALM
  wind_UOM = df$UOM
  speed = df$speed
  direction = df$direction
  VRB = df$VRB
  GUST = df$GUST
  gust_speed = df$gust_speed
  
  WIND_DIRECTION_VARIATION = F
  extreme_wind_direction_n = NA
  extreme_wind_direction_x = NA
  df = parse_field(groups[df$index],df$index,recognize_wind_direction_variation,extract_wind_direction_variation,F,"wind direction variation")
  if ( df$found_optional_field ) {
    WIND_DIRECTION_VARIATION = df$WIND_DIRECTION_VARIATION
    extreme_wind_direction_n = df$extreme_wind_direction_n
    extreme_wind_direction_x = df$extreme_wind_direction_x    
  }
  
  visibility = NA
  visibility_UOM = NA
  CAVOK = F
  df = parse_field(groups[df$index],df$index,recognize_visibility,extract_visibility,T,"visibility")
  visibility = df$visibility
  visibility_UOM = df$UOM
  CAVOK = df$CAVOK
  
  print(metar_string)
  return(data.frame(METAR,
                    SPECI,
                    COR,
                    ICAO_location_indicator,
                    day,hour,minute,
                    NIL,
                    AUTO,
                    CALM,wind_UOM,speed,direction,VRB,GUST,gust_speed,
                    WIND_DIRECTION_VARIATION,extreme_wind_direction_n,extreme_wind_direction_x,
                    visibility,visibility_UOM,CAVOK))
}

print(metar_decoder(wu))
print(metar_decoder(lipe))
print(metar_decoder(live))
print(metar_decoder(live_1))
print(metar_decoder(birk))
print(metar_decoder(llbg))
print(metar_decoder(licr))
print(metar_decoder(kslc))
print(metar_decoder(eidw))
print(metar_decoder(paed))
print(metar_decoder(klxv))
print(metar_decoder(klxv_1))
print(metar_decoder(klxv_2))
str(metar_decoder(kccu))