# loop over items and get common countries


items <- c("leg", "toe", "foot", "face", "eye",
           "ear", "hand", "knee", "mouth", "line", "circle",
           "square", "hexagon", "triangle", "octagon")


all_country_codes <- list()
for (i in 1:length(items)){
  
  print(items[i])
  
  # point data
  raw_data <- read_feather(paste0(RAW_PATH, items[i], ".txt")) %>%
    data.table() 
  
  these_country_codes <- unique(c(raw_data$country_code))
  
  
  if (i == 1) {
    all_country_codes = these_country_codes
  } else {
    all_country_codes = intersect(all_country_codes,these_country_codes)
  }
}

g_countries <- c("US", "NZ", "NL", "BR" ,"IT", "KR", "AR", "BG",
                      "NO", "AU" ,"DE" ,"GB" ,"HU" ,"PL", "SE", "AE", "SA", "PH",
"RS", "ID", "DK", "VN", "SG", "CA",
"CZ", "MY", "JP", "FR", "EE", "RU",
"QA", "TR", "TH", "IE" ,"FI", "HR",
 "ES" ,"UA", "IL", "SK", "CL", "TW",
 "PT" ,"RO", "IQ", "DZ", "IN", "KH",
 "AT", "HK", "EG", "BE", "SI", "LT",
"ZA", "GR", "BY", "BA" ,"MX" ,"CH",
"CO" ,"KW", "PK" ,"LV" ,"KZ" ,"JO")