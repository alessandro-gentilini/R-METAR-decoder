R-METAR-decoder
===============

A METAR decoder written in R.

Status: the decoder works up to the altimeter (or `QNH`) group.
An extended test suite is needed.

The description of METAR decodification is based on two manuals, there are two main METAR formats, I call them the _international_ format and the _USA_ format; there are differences in the two formats for example regarding the units of measure (where the international format prescribes the SI units while the USA format prescribe different units) or the abbreviations.

The international format is described in a manual from the World Meteorological Organization:

Manual on Codes
International Codes
Volume I.1
(Annex II to WMO Technical Regulations)
Part A – Alphanumeric Codes
WMO-No. 306
2011 edition
Updated in 2012
ftp://ftp.wmo.int/Documents/MediaPublic/Publications/CodesManual_WMO_No_306/WMO306_Vol_I.1_2012_en.pdf (accessed 20130528)


The USA format is described in this manual:

FEDERAL COORDINATOR FOR METEOROLOGICAL SERVICES AND SUPPORTING RESEARCH
8455 COLESVILLE ROAD, SUITE 1500
SILVER SPRING, MARYLAND 20910
301-427-2002
www.ofcm.gov
FEDERAL METEOROLOGICAL HANDBOOK
NUMBER 1
SURFACE WEATHER OBSERVATIONS AND REPORTS
FCM-H1-2005
Washington, D.C.
September 2005
http://www.ofcm.gov/fmh-1/pdf/FMH1.pdf (accessed 20130529)
