source('metar_decoder.R')

# gives problem because four clouds CYYB 122345Z 31007KT 15SM FEW030TCU BKN060 BKN100 BKN250 16/13 A2980 RETS RMK TCU1SC4AC1CI1 PRESFR SLP093 DENSITY ALT 1700FT

# print(get_metar("LIPE"))
# print(get_metar("LIVE"))
# print(get_metar("KLXV"))
# print(get_metar("KCCU"))
# print(get_metar("LBBG"))
# print(get_metar("PAED"))
# print(get_metar("BIRK"))

print(metar_string_decoder("CYYB 122345Z 31007KT 15SM FEW030TCU BKN060 BKN100 BKN250 16/13 A2980 RETS RMK TCU1SC4AC1CI1 PRESFR SLP093 DENSITY ALT 1700FT"))
print(metar_string_decoder("CWZO 142345Z AUTO 19009KT RMK AO1 "))
print(metar_string_decoder("KLZU 142345Z 00000KT 10SM CLR 27/14 A2995 RMK ATIS C JW "))
print(metar_string_decoder("KY63 142355Z AUTO 13014G17KT 10SM BKN050 BKN060 25/16 A2980 RMK AO2"))
print(metar_string_decoder("K1P1 142355Z AUTO 32007KT 10SM SCT110 22/09 A2973 RMK AO2 T02210090 10252 20221"))
print(metar_string_decoder("KCQW 142355Z AUTO 10SM CLR 25/13 A2995 RMK AO2"))
print(metar_string_decoder("KAPG 142350Z 00000 7SM SCT050 25/13 A2989 RMK LAST"))
print(metar_string_decoder("PATC 142345Z AUTO 17020KT M1/4SM -RA FG VV002 04/04 A3008 RMK AO2 PK WND 17026/2331 SLP189"))
print(metar_string_decoder("KLCH 142353Z CLR A2986 RMK AO2 LTG DSNT NE AND E SLP121 58011 $"))
print(metar_string_decoder("KPHF 142354Z AUTO A2993"))

# print(metar_string_decoder(wu))
# print(metar_string_decoder(lipe))
# print(metar_string_decoder(live))
# print(metar_string_decoder(live_1))
# print(metar_string_decoder(birk))
# print(metar_string_decoder(llbg))
# print(metar_string_decoder(licr))
# print(metar_string_decoder(kslc))
# print(metar_string_decoder(eidw))
# print(metar_string_decoder(paed))
# print(metar_string_decoder(klxv))
# print(metar_string_decoder(klxv_1))
# print(metar_string_decoder(klxv_2))
# print(metar_string_decoder(kccu))
# print(metar_string_decoder(lbbg))
# print(metar_string_decoder(kttn))

# Tests from https://github.com/prcwiek/pmetar/issues/2
# first test, In the following, visibility is parsed as NA when the original METAR says 4 statute miles ("4SM"):
df <- metar_string_decoder('KDCA 041400Z AUTO 07004KT 4SM HZ OVC009 20/17 A2987 RMK T02000170 MADISHF')
stopifnot(df$visibility==4, df$visibility_UOM=='SM')
# second test, In the following, visibility is parsed as 1 when the original METAR says "1 3/4 SM":
df <- metar_string_decoder('KDCA 121244Z 05010KT 1 3/4SM R01/6000VP6000FT -RA BR OVC007 14/12 A2978 RMK AO2 P0002 T01390122')
stopifnot(df$visibility==1.75, df$visibility_UOM=='SM')
