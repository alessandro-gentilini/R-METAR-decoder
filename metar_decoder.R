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

library(RCurl)

dbgprint = function(s)
{
  cat(s)
}

WMO_strict = F

FMH_table_12_1_visibility = c("M1/4","1/4","1/2","3/4","1","1 1/4","1 1/2","1 3/4","2","2 1/2","3","4","5","6","7","8","9","10",
                              "0","1/16","1/8","3/16","1/4","5/16","3/8","1/2","5/8","3/4","7/8","1","1 1/8","1 1/4","1 3/8","1 1/2","1 5/8","1 3/4","1 7/8","2","2 1/4","2 1/2","2 3/4","3","4","5","6","7","8","9","10","11","12","13","14","15","20","25","30","35")
options(error=traceback)

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
  return(grepl("[A-Z][0-9A-Z][0-9A-Z][0-9A-Z]",field))
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
          grepl("VRB[0-9][0-9]G[0-9][0-9]KT" , field) ||
          grepl("VRB[0-9][0-9]G[0-9][0-9]MPS", field) ||            
          grepl("[0-9][0-9][0-9][0-9][0-9]G[0-9][0-9]KT" , field) ||
          grepl("[0-9][0-9][0-9][0-9][0-9]G[0-9][0-9]MPS", field) 
          
          # as per the following METAR uom is omitted after 00000
          #"2013/06/14 23:50"
          #"KAPG 142350Z 00000 7SM SCT050 25/13 A2989 RMK LAST"
          # This is in contrast with FMH 12.6.5d which prescribes 00000KT and WMO 15.5.4 which prescribes 00000KT or 00000MPS
          || grepl("00000",field,fixed=T)
          
  )
}

extract_wind = function(field)
{
  CALM = F
  UOM = NA
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
    UOM = "KT"
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
  } else if ( grepl("VRB[0-9][0-9]G[0-9][0-9]KT",field) ) {
    res = regexec("VRB([0-9][0-9])G([0-9][0-9])KT", field)
    speed = as.numeric(regmatches(field,res)[[1]][[2]])
    gust_speed = as.numeric(regmatches(field,res)[[1]][[3]])
    VRB = T
    GUST = T
    UOM = "KT"    
  } else if ( grepl("VRB[0-9][0-9]G[0-9][0-9]MPS",field) ) {
    res = regexec("VRB([0-9][0-9])G([0-9][0-9])MPS", field)
    speed = as.numeric(regmatches(field,res)[[1]][[2]])
    gust_speed = as.numeric(regmatches(field,res)[[1]][[3]])
    VRB = T
    GUST = T
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
  } else if(grepl("00000",field,fixed=T)){
    CALM=T
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
  return(grepl("^[0-9][0-9][0-9][0-9]",field)||
           grepl(".*SM",field) ||
           grepl("M.*SM",field) ||
           grepl("CAVOK",field,fixed=T))
}

extract_visibility = function(field)
{
  visibility = NA
  UOM = NA
  CAVOK = F
  LESS_THAN = F
  if ( grepl("[0-9][0-9][0-9][0-9]",field) ) {
    res = regexec("([0-9][0-9][0-9][0-9])",field)
    visibility = as.numeric(regmatches(field,res)[[1]][[2]])
    UOM = "M"
  } else if ( grepl("M(.*)SM",field)) {
    res = regexec("M(.*)SM",field)
    visibility = as.numeric(eval(parse(text=regmatches(field,res)[[1]][[2]])))
    UOM = "SM"    
    LESS_THAN = T
  } else if (grepl(".*SM",field)) {
    res = regexec("(.*)SM",field)
    visibility = as.numeric(eval(parse(text=regmatches(field,res)[[1]][[2]])))
    UOM = "SM"
  } else if ( grepl("CAVOK",field,fixed=T)){
    CAVOK = T
  }
  return(data.frame(visibility,UOM,CAVOK,LESS_THAN))
}

recognize_runway_visual_range = function(field)
{
  return(grepl("R[0-9][0-9]\\/(M|P)?[0-9][0-9][0-9][0-9](U|D|N)?",field)||
         grepl("R[0-9][0-9]\\/(M|P)?[0-9][0-9][0-9][0-9]V(M|P)?[0-9][0-9][0-9][0-9](U|D|N)?",field))
}

set_NA_if_empty_string = function(s)
{
  if ( s=="" ) {
    return(NA)
  }else{
    return(s)
  }
}

extract_runway_visual_range = function(field)
{
  # todo: 12.6.7 FMH (feet as UOM)
  runway = NA
  runway_visual_range = NA
  tendency = NA
  extreme_value = NA
  runway_visual_range_variation_1 = NA
  runway_visual_range_variation_2 = NA
  extreme_value_1 = NA
  extreme_value_2 = NA
  tendency_12 = NA
  if(grepl("R[0-9][0-9]\\/(M|P)?[0-9][0-9][0-9][0-9](U|D|N)?",field)){
    res = regexec("R([0-9][0-9])\\/(M|P)?([0-9][0-9][0-9][0-9])(U|D|N)?",field)
    runway = regmatches(field,res)[[1]][[2]]
    extreme_value = set_NA_if_empty_string(regmatches(field,res)[[1]][[3]])
    runway_visual_range = as.numeric(regmatches(field,res)[[1]][[4]])
    tendency = set_NA_if_empty_string(regmatches(field,res)[[1]][[5]])
  } else if(grepl("R[0-9][0-9]\\/(M|P)?[0-9][0-9][0-9][0-9]V(M|P)?[0-9][0-9][0-9][0-9](U|D|N)?",field)) {
    res = regexec("R([0-9][0-9])\\/(M|P)?([0-9][0-9][0-9][0-9])V(M|P)?([0-9][0-9][0-9][0-9])(U|D|N)?",field)
    runway = regmatches(field,res)[[1]][[2]]
    extreme_value_1 = set_NA_if_empty_string(regmatches(field,res)[[1]][[3]])
    runway_visual_range_variation_1 = as.numeric(regmatches(field,res)[[1]][[4]])
    extreme_value_2 = set_NA_if_empty_string(regmatches(field,res)[[1]][[5]])
    runway_visual_range_variation_2 = as.numeric(regmatches(field,res)[[1]][[6]])
    tendency_12 = set_NA_if_empty_string(regmatches(field,res)[[1]][[7]])
  }
  
  
  return(data.frame(runway,runway_visual_range,tendency,extreme_value,
                    extreme_value_1,runway_visual_range_variation_1,
                    extreme_value_2,runway_visual_range_variation_2,
                    tendency_12))
}

recognize_weather = function(field)
{
  # Code table 4678, page A-359 in WMO
  # Table 12-2, in 12.6.8 FMH, page 12-4
  res = regexec("(\\-|\\+|VC)?(MI|BC|PR|DR|BL|SH|TS|FZ)?(.*)",field)
  supposed_phenomena = regmatches(field,res)[[1]][[4]]
  found = T
  if ( nchar(supposed_phenomena) != 0 && nchar(supposed_phenomena)%%2 == 0 ){
    start = 1
    for ( i in nchar(supposed_phenomena)/2 ) {
      found = found && (substr(supposed_phenomena,start,start+2) %in% c("DZ","RA","SN","SG","IC","PL","GR","GS","UP","BR","FG","FU","VA","DU","SA","HZ","PY","PO","SQ","FC","SS","DS") )
      start = start + 2
    }
  } else if ( supposed_phenomena=="" ) {
    found = T
  } else {
    found = F
  }
  return(found)
}

extract_weather = function(field)
{
  res = regexec("(\\-|\\+|VC)?(MI|BC|PR|DR|BL|SH|TS|FZ)?(.*)",field)
  intensity = set_NA_if_empty_string(regmatches(field,res)[[1]][[2]])
  descriptor = set_NA_if_empty_string(regmatches(field,res)[[1]][[3]])
  phenomena = set_NA_if_empty_string(regmatches(field,res)[[1]][[4]])
  return(data.frame(intensity,descriptor,phenomena))
}

recognize_clouds = function(field)
{
  return(grepl("(FEW|SCT|BKN|OVC)([0-9][0-9][0-9])(CB|TCU|///)?",field) ||
         grepl("(NSC|NCD|SKC|CLR)",field)                  ||
         grepl("VV([0-9][0-9][0-9])",field)                ||
         grepl("//////",field)                  )
}

extract_clouds = function(field)
{
  amount = NA
  height = NA
  cloud_abbreviation = NA
  VV = F
  vertical_visibility = NA
  unobservable = F
  convective_cloud = NA
  vertical_visibility_unavailable = F
  if ( grepl("(FEW|SCT|BKN|OVC)([0-9][0-9][0-9])(CB|TCU|///)?",field) ) {
    res = regexec("(FEW|SCT|BKN|OVC)([0-9][0-9][0-9])(CB|TCU|///)?",field)
    amount = regmatches(field,res)[[1]][[2]]
    # WMO 15.9.1.5 height is in 30meter steps
    height = 30*as.numeric(regmatches(field,res)[[1]][[3]])
    convective_cloud = set_NA_if_empty_string(regmatches(field,res)[[1]][[4]])
  } else if ( grepl("(NSC|NCD|SKC|CLR)",field) ) {
    res = regexec("(NSC|NCD|SKC|CLR)",field)
    cloud_abbreviation = regmatches(field,res)[[1]][[2]]
  } else if (grepl("VV([0-9][0-9][0-9])",field) ) {
    res = regexec("VV([0-9][0-9][0-9])",field)
    VV = T
    # WMO 15.9.2 vertical visibility is in 30meter steps
    vertical_visibility = 30*as.numeric(regmatches(field,res)[[1]][[2]]);
  }  else if (grepl("VV///",field)) {
    vertical_visibility_unavailable = T
  } else if (grepl("//////",field)) {
    unobservable = T
  }
  return(data.frame(amount,height,cloud_abbreviation,VV,vertical_visibility,unobservable,convective_cloud,vertical_visibility_unavailable))
}

recognize_temperature = function(field)
{
  return(grepl("(M)?([0-9][0-9])/(M)?([0-9][0-9])",field) ||
         grepl("(M)?([0-9][0-9])/",field) )
}

extract_temperature = function(field)
{
  temperature = NA
  dew_point = NA
  if ( grepl("(M)?([0-9][0-9])/(M)?([0-9][0-9])",field) ){
    sign = +1
    res = regexec("(M)?([0-9][0-9])/(M)?([0-9][0-9])",field)
    if ( "M" == regmatches(field,res)[[1]][[2]] ) {
      sign = -1
    }
    temperature = sign*as.numeric(regmatches(field,res)[[1]][[3]])
    sign = +1
    if ( "M" == regmatches(field,res)[[1]][[4]] ) {
      sign = -1
    } 
    dew_point = sign*as.numeric(regmatches(field,res)[[1]][[5]])    
  } else if ( grepl("(M)?([0-9][0-9])/",field) ) {
    sign = +1
    res = regexec("(M)?([0-9][0-9])/",field)
    if ( "M" == regmatches(field,res)[[1]][[2]] ) {
      sign = -1
    }
    temperature = sign*as.numeric(regmatches(field,res)[[1]][[3]])    
  }
  
  return(data.frame(temperature,dew_point))
}

recognize_altimeter = function(field)
{
  return( grepl("(A|Q)([0-9][0-9][0-9][0-9])",field) )
}

extract_altimeter = function(field)
{
  UOM = NA
  pressure = NA
  res = regexec("(A|Q)([0-9][0-9][0-9][0-9])",field)
  type = regmatches(field,res)[[1]][[2]]
  if ( type == "A") {
    UOM="Hg_inches"
    pressure = as.numeric(regmatches(field,res)[[1]][[3]])/100
  } else if ( type == "Q" ){
    UOM="hPa"
    pressure = as.numeric(regmatches(field,res)[[1]][[3]])
  }
  return(data.frame(UOM,pressure))
}

parse_field = function(groups,index,recognizer,extractor,is_compulsory,field_description)
{
  data = NA
  found_optional_field = F
  if ( index <= length(groups) ) {
    field = groups[index]
    if ( recognizer(field) ) {
      data = extractor(field)
      index = index + 1
      found_optional_field = T
    } else {
      if ( is_compulsory ) {
        stop(sprintf("Expected compulsory field '%s', found '%s'.",field_description,field))
      }  
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



metar_string_decoder = function(metar_string,low_visibility=1/32)
{
  metar_string = fix_white_space_in_FMH_visibility(metar_string)
  print(metar_string)
  groups = scan(what=character(),text=metar_string)
  
  METAR = NA
  df = parse_field(groups,1,recognize_METAR,extract_METAR,F, "METAR")
  METAR = df$data
  
  SPECI = NA
  df = parse_field(groups,df$index,recognize_SPECI,extract_SPECI,F, "SPECI")
  SPECI = df$data
  
  COR = NA
  df = parse_field(groups,df$index,recognize_COR,extract_COR,F, "COR")
  COR = df$data
  
  ICAO_location_indicator = NA
  df = parse_field(groups,df$index,recognize_ICAO_location_indicator,extract_ICAO_location_indicator,T, "ICAO_location_indicator")
  ICAO_location_indicator = df$data
  
  day = NA
  hour = NA
  minute = NA
  df = parse_field(groups,df$index,recognize_timestamp,extract_timestamp,T, "timestamp")
  day = df$dd
  hour = df$hh
  minute = df$mm
  
  NIL = NA
  df = parse_field(groups,df$index,recognize_NIL,extract_NIL,F, "NIL")
  NIL = df$data  
  
  AUTO = F
  df = parse_field(groups,df$index,recognize_AUTO,extract_AUTO,F, "AUTO")
  if ( df$found_optional_field ) {
    AUTO = df$data  
  }
  
  CALM = F
  wind_UOM = NA
  speed = NA
  direction = NA
  VRB = F
  GUST = F
  gust_speed = NA
  # In the following metar, wind is missing
  # 2013/06/14 23:53
  # KLCH 142353Z CLR A2986 RMK AO2 LTG DSNT NE AND E SLP121 58011 $  
  #compulsory=!AUTO && T
  compulsory=F
  df = parse_field(groups,df$index,recognize_wind,extract_wind,compulsory, "Wind")
  if ( compulsory ) {
    CALM = df$CALM
    wind_UOM = df$UOM
    speed = df$speed
    direction = df$direction
    VRB = df$VRB
    GUST = df$GUST
    gust_speed = df$gust_speed    
  } else if ( df$found_optional_field ) {
    CALM = df$CALM
    wind_UOM = df$UOM
    speed = df$speed
    direction = df$direction
    VRB = df$VRB
    GUST = df$GUST
    gust_speed = df$gust_speed    
  }
  
  WIND_DIRECTION_VARIATION = F
  extreme_wind_direction_n = NA
  extreme_wind_direction_x = NA
  df = parse_field(groups,df$index,recognize_wind_direction_variation,extract_wind_direction_variation,F,"wind direction variation")
  if ( df$found_optional_field ) {
    WIND_DIRECTION_VARIATION = df$WIND_DIRECTION_VARIATION
    extreme_wind_direction_n = df$extreme_wind_direction_n
    extreme_wind_direction_x = df$extreme_wind_direction_x    
  }
  
  visibility = NA
  visibility_UOM = NA
  CAVOK = F
  LESS_THAN = F
  # In the following metar, visibility is missing
  # 2013/06/14 23:53
  # KLCH 142353Z CLR A2986 RMK AO2 LTG DSNT NE AND E SLP121 58011 $  
  #compulsory=!AUTO && T
  compulsory=F
  df = parse_field(groups,df$index,recognize_visibility,extract_visibility,compulsory,"visibility")
  if ( compulsory ) {
    visibility = df$visibility
    visibility_UOM = df$UOM
    CAVOK = df$CAVOK
    LESS_THAN = df$LESS_THAN
  } else if ( df$found_optional_field ) {
    visibility = df$visibility
    visibility_UOM = df$UOM
    CAVOK = df$CAVOK
    LESS_THAN = df$LESS_THAN    
  }
  
  
  # todo: Implement WMO 15.6.2
  
  # As per WMO 15.7.2 up to four runway visual ranges can be reported
  runway_1 = NA
  runway_visual_range_1 = NA
  tendency_1 = NA
  extreme_value_1 = NA
  runway_visual_range_variation_1_1 = NA
  runway_visual_range_variation_2_1 = NA
  extreme_value_1_1 = NA
  extreme_value_2_1 = NA
  tendency_12_1 = NA
  df = parse_field(groups,df$index,recognize_runway_visual_range,extract_runway_visual_range,F,"runway visual range 1")
  if ( df$found_optional_field ) {
    runway_1 = df$runway
    runway_visual_range_1 = df$runway_visual_range   
    tendency_1 = df$tendency
    extreme_value_1 = df$extreme_value
    runway_visual_range_variation_1_1 = df$runway_visual_range_variation_1
    runway_visual_range_variation_2_1 = df$runway_visual_range_variation_2
    extreme_value_1_1 = df$extreme_value_1
    extreme_value_2_1 = df$extreme_value_2
    tendency_12_1 = df$tendency_12    
  }
  
  runway_2 = NA
  runway_visual_range_2 = NA
  tendency_2 = NA
  extreme_value_2 = NA
  runway_visual_range_variation_1_2 = NA
  runway_visual_range_variation_2_2 = NA
  extreme_value_1_2 = NA
  extreme_value_2_2 = NA
  tendency_12_2 = NA  
  df = parse_field(groups,df$index,recognize_runway_visual_range,extract_runway_visual_range,F,"runway visual range 2")
  if ( df$found_optional_field ) {
    runway_2 = df$runway
    runway_visual_range_2 = df$runway_visual_range   
    tendency_2 = df$tendency
    extreme_value_2 = df$extreme_value
    runway_visual_range_variation_1_2 = df$runway_visual_range_variation_1
    runway_visual_range_variation_2_2 = df$runway_visual_range_variation_2
    extreme_value_1_2 = df$extreme_value_1
    extreme_value_2_2 = df$extreme_value_2
    tendency_12_2 = df$tendency_12    
  }  
  
  runway_3 = NA
  runway_visual_range_3 = NA
  tendency_3 = NA
  extreme_value_3 = NA
  runway_visual_range_variation_1_3 = NA
  runway_visual_range_variation_2_3 = NA
  extreme_value_1_3 = NA
  extreme_value_2_3 = NA
  tendency_12_3 = NA    
  df = parse_field(groups,df$index,recognize_runway_visual_range,extract_runway_visual_range,F,"runway visual range 3")
  if ( df$found_optional_field ) {
    runway_3 = df$runway
    runway_visual_range_3 = df$runway_visual_range   
    tendency_3 = df$tendency
    extreme_value_3 = df$extreme_value
    runway_visual_range_variation_1_3 = df$runway_visual_range_variation_1
    runway_visual_range_variation_2_3 = df$runway_visual_range_variation_2
    extreme_value_1_3 = df$extreme_value_1
    extreme_value_2_3 = df$extreme_value_2
    tendency_12_3 = df$tendency_12     
  }   
  
  runway_4 = NA
  runway_visual_range_4 = NA
  tendency_4 = NA
  extreme_value_4 = NA
  runway_visual_range_variation_1_4 = NA
  runway_visual_range_variation_2_4 = NA
  extreme_value_1_4 = NA
  extreme_value_2_4 = NA
  tendency_12_4 = NA   
  df = parse_field(groups,df$index,recognize_runway_visual_range,extract_runway_visual_range,F,"runway visual range 4")
  if ( df$found_optional_field ) {
    runway_4 = df$runway
    runway_visual_range_4 = df$runway_visual_range   
    tendency_4 = df$tendency
    extreme_value_4 = df$extreme_value
    runway_visual_range_variation_1_4 = df$runway_visual_range_variation_1
    runway_visual_range_variation_2_4 = df$runway_visual_range_variation_2
    extreme_value_1_4 = df$extreme_value_1
    extreme_value_2_4 = df$extreme_value_2
    tendency_12_4 = df$tendency_12        
  }  
  
  # As per WMO 15.8.1 up to three observed weather phenomena can be reported
  intensity_1 = NA
  descriptor_1 = NA
  phenomena_1 = NA
  df= parse_field(groups,df$index,recognize_weather,extract_weather,F,"weather 1")
  if ( df$found_optional_field ) {
    intensity_1 = df$intensity
    descriptor_1 = df$descriptor
    phenomena_1 = df$phenomena
  }
  
  intensity_2 = NA
  descriptor_2 = NA
  phenomena_2 = NA
  df= parse_field(groups,df$index,recognize_weather,extract_weather,F,"weather 2")
  if ( df$found_optional_field ) {
    intensity_2 = df$intensity
    descriptor_2 = df$descriptor
    phenomena_2 = df$phenomena
  }  
  
  intensity_3 = NA
  descriptor_3 = NA
  phenomena_3 = NA
  df= parse_field(groups,df$index,recognize_weather,extract_weather,F,"weather 3")
  if ( df$found_optional_field ) {
    intensity_3 = df$intensity
    descriptor_3 = df$descriptor
    phenomena_3 = df$phenomena
  }    
  
  
  # As per WMO 15.9.1.3 up to three clouds can be reported
  # Some METARs has four clouds, like this one (20130614)
  # CYYB 122345Z 31007KT 15SM FEW030TCU BKN060 BKN100 BKN250 16/13 A2980 RETS RMK TCU1SC4AC1CI1 PRESFR SLP093 DENSITY ALT 1700FT
  cloud_amount_1 = NA
  cloud_height_1 = NA
  cloud_abbreviation_1 = NA
  VV_1 = NA
  vertical_visibility_1 = NA
  cloud_unobservable_1 = NA
  convective_cloud_1 = NA
  vertical_visibility_unavailable_1 = NA
  df= parse_field(groups,df$index,recognize_clouds,extract_clouds,F,"clouds 1")
  if ( df$found_optional_field ) {
    cloud_amount_1 = df$amount
    cloud_height_1 = df$height
    cloud_abbreviation_1 = df$cloud_abbreviation
    VV_1 = df$VV
    vertical_visibility_1 = df$vertical_visibility
    cloud_unobservable_1 = df$unobservable
    convective_cloud_1 = df$convective_cloud
    vertical_visibility_unavailable_1 = df$vertical_visibility_unavailable
  }
  
  cloud_amount_2 = NA
  cloud_height_2 = NA
  cloud_abbreviation_2 = NA
  VV_2 = NA
  vertical_visibility_2 = NA  
  cloud_unobservable_2 = NA
  convective_cloud_2 = NA
  vertical_visibility_unavailable_2 = NA
  df= parse_field(groups,df$index,recognize_clouds,extract_clouds,F,"clouds 2")
  if ( df$found_optional_field ) {
    cloud_amount_2 = df$amount
    cloud_height_2 = df$height
    cloud_abbreviation_2 = df$cloud_abbreviation
    VV_2 = df$VV
    vertical_visibility_2 = df$vertical_visibility    
    cloud_unobservable_2 = df$unobservable
    convective_cloud_2 = df$convective_cloud
    vertical_visibility_unavailable_2 = df$vertical_visibility_unavailable    
  }  

  cloud_amount_3 = NA
  cloud_height_3 = NA
  cloud_abbreviation_3 = NA
  VV_3 = NA
  vertical_visibility_3 = NA    
  cloud_unobservable_3 = NA
  convective_cloud_3 = NA
  vertical_visibility_unavailable_3 = NA
  df= parse_field(groups,df$index,recognize_clouds,extract_clouds,F,"clouds 3")
  if ( df$found_optional_field ) {
    cloud_amount_3 = df$amount
    cloud_height_3 = df$height
    cloud_abbreviation_3 = df$cloud_abbreviation
    VV_3 = df$VV
    vertical_visibility_3 = df$vertical_visibility        
    cloud_unobservable_3 = df$unobservable
    convective_cloud_3 = df$convective_cloud
    vertical_visibility_unavailable_3 = df$vertical_visibility_unavailable    
  }    
  
  cloud_amount_4 = NA
  cloud_height_4 = NA
  cloud_abbreviation_4 = NA
  VV_4 = NA
  vertical_visibility_4 = NA    
  cloud_unobservable_4 = NA
  convective_cloud_4 = NA
  vertical_visibility_unavailable_4 = NA
  df= parse_field(groups,df$index,recognize_clouds,extract_clouds,F,"clouds 4")
  if ( df$found_optional_field ) {
    cloud_amount_4 = df$amount
    cloud_height_4 = df$height
    cloud_abbreviation_4 = df$cloud_abbreviation
    VV_4 = df$VV
    vertical_visibility_4 = df$vertical_visibility        
    cloud_unobservable_4 = df$unobservable
    convective_cloud_4 = df$convective_cloud
    vertical_visibility_unavailable_4 = df$vertical_visibility_unavailable    
  }     
  
  temperature = NA
  dew_point = NA
  # per FMH 12.6.10 temperature/dew point can be not available
  df= parse_field(groups,df$index,recognize_temperature,extract_temperature,F,"temperature")
  if ( df$found_optional_field ) {
    temperature = df$temperature
    dew_point = df$dew_point
  }
  
  pressure_UOM = NA
  pressure = NA
  # per FMH 12.6.10 temperature/dew point can be not available
  df= parse_field(groups,df$index,recognize_altimeter,extract_altimeter,F,"pressure")
  if ( df$found_optional_field ) {
    pressure_UOM = df$UOM
    pressure = df$pressure  
  }
  
  
  
  return(data.frame(METAR,
                    SPECI,
                    COR,
                    ICAO_location_indicator,
                    day,hour,minute,
                     NIL,
                     AUTO,
                     CALM,wind_UOM,speed,direction,VRB,GUST,gust_speed,
                     WIND_DIRECTION_VARIATION,extreme_wind_direction_n,extreme_wind_direction_x,
                     visibility,visibility_UOM,CAVOK,LESS_THAN,
                     runway_1,extreme_value_1,runway_visual_range_1,tendency_1,runway_visual_range_variation_1_1,runway_visual_range_variation_2_1,extreme_value_1_1,extreme_value_2_1,tendency_12_1,
                     runway_2,extreme_value_2,runway_visual_range_2,tendency_2,runway_visual_range_variation_1_2,runway_visual_range_variation_2_2,extreme_value_1_2,extreme_value_2_2,tendency_12_2,
                     runway_3,extreme_value_3,runway_visual_range_3,tendency_3,runway_visual_range_variation_1_3,runway_visual_range_variation_2_3,extreme_value_1_3,extreme_value_2_3,tendency_12_3,
                     runway_4,extreme_value_4,runway_visual_range_4,tendency_4,runway_visual_range_variation_1_4,runway_visual_range_variation_2_4,extreme_value_1_4,extreme_value_2_4,tendency_12_4,
                     intensity_1,descriptor_1,phenomena_1,
                     intensity_2,descriptor_2,phenomena_2,
                     intensity_3,descriptor_3,phenomena_3,
                    cloud_amount_1,cloud_height_1,cloud_abbreviation_1,
                    cloud_amount_2,cloud_height_2,cloud_abbreviation_2,
                    cloud_amount_3,cloud_height_3,cloud_abbreviation_3,
                    cloud_amount_4,cloud_height_4,cloud_abbreviation_4,
                    VV_1,vertical_visibility_1,
                    VV_2,vertical_visibility_2,
                    VV_3,vertical_visibility_3,
                    VV_4,vertical_visibility_4,
                    cloud_unobservable_1,
                    cloud_unobservable_2,
                    cloud_unobservable_3,
                    cloud_unobservable_4,
                    convective_cloud_1,
                    convective_cloud_2,
                    convective_cloud_3,
                    convective_cloud_4,
                    vertical_visibility_unavailable_1,
                    vertical_visibility_unavailable_2,
                    vertical_visibility_unavailable_3,
                    vertical_visibility_unavailable_4,
                    temperature,dew_point,
                    pressure_UOM,pressure
                    ))
}

get_metar = function(ICAO_ID)
{
  url = sprintf("https://tgftp.nws.noaa.gov/data/observations/metar/stations/%s.TXT",ICAO_ID)
  ans=scan(what=character(),text=getURL(url),sep="\n")
  return(metar_decoder(ans[2]))
}

get_metar_cycle = function()
{
  url = "https://tgftp.nws.noaa.gov/data/observations/metar/cycles/08Z.TXT"
  ans=scan(what=character(),text=getURL(url),sep="\n")
  prev = ""
  for ( s in ans ) {
    if ( grepl("[A-Z][A-Z][A-Z][A-Z].*",s)){
      print(prev)
      print(metar_decoder(s))    
    }
    prev = s
  }
}

metar_decoder = function(metar_string_vector)
{
  df = data.frame()
  for (s in metar_string_vector) {
    df1 = metar_string_decoder(s)
    df <- rbind(df,df1)
  }
  df
}