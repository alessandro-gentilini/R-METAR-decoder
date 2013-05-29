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



ICAO_station_length = 4
YYGGggZ_length = 7
wind_variation_lenght = 7
WMO_visibility_length = 4



substrRight = function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

substrLeft = function(x, n){
  substr(x, 1, nchar(x)-n)
}

fix_white_space_in_FMH_visibility = function(metar_string)
{
  re1="\\d";  # Any Single Digit 1
  re2="(\\s+)";  # White Space 1
  re3="\\d";	# Any Single Digit 2
  re4="\\/";	# Any Single Character 1
  re5="\\d";	# Any Single Digit 3
  re6="S";	# Any Single Word Character (Not Whitespace) 1
  re7="M";	# Any Single Word Character (Not Whitespace) 2
  re = paste(re1,re2,re3,re4,re5,re6,re7,sep="")
  result = regexec(re,metar_string)
  if ( result[[1]][[1]] != -1 ) {
    white_space_position = result[[1]][[2]]
    substr(metar_string,white_space_position,white_space_position) = '+'
  }
  metar_string
}

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
  # todo: 15.5.6
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


metar_decoder = function(metar_string,low_visibility=1/32)
{
  metar_string = fix_white_space_in_FMH_visibility(metar_string)
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
  
  WIND_DIRECTION_VARIATION = F
  extreme_wind_direction_n = NA
  extreme_wind_direction_x = NA
  visibility = NA
  visibility_UOM = NA
  CAVOK = F
  if (nchar(groups[next_index])==wind_variation_lenght && substr(groups[next_index],4,4)=='V'){
    WIND_DIRECTION_VARIATION = T
    extreme_wind_direction_n = as.numeric(substr(groups[next_index],1,3))
    extreme_wind_direction_x = as.numeric(substr(groups[next_index],5,7))
    next_index = next_index + 1
  } 
  
  if ( suppressWarnings(!is.na(as.numeric(groups[next_index]))) && nchar(groups[next_index])==WMO_visibility_length ){
    visibility = as.numeric(groups[next_index])
    visibility_UOM = "meter"
  } else if (substrRight(groups[next_index],2)=="SM") {
    visibility_UOM = "SM"
    if(substr(groups[next_index],1,1)=='M'){
      visibility = low_visibility
    } else {
      visibility = eval(parse(text=(substrLeft(groups[next_index],2))))
    }
  } else if (groups[next_index]=="CAVOK") {
    CAVOK = T
  } else {
    stop(paste("Expected visibility group, found",groups[next_index]))
  }
  
  
  data.frame(METAR,SPECI,COR,ICAO_location_indicator,
             day,hour,minute,
             NIL,AUTO,
             CALM,VRB,direction,speed,GUST,gust_speed,wind_speed_UOM,
             WIND_DIRECTION_VARIATION,extreme_wind_direction_n,extreme_wind_direction_x,CAVOK,visibility,visibility_UOM)
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
print(metar_decoder(kccu))