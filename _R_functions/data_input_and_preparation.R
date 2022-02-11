#Read in info from the input_dataframes excel file
input_dataframes_filepath <- list.files(path = "./_tables/", pattern = "input_dataframes.xlsx", full.names = TRUE)

sheets <- excel_sheets(input_dataframes_filepath)
input_dataframes <- lapply(excel_sheets(input_dataframes_filepath), 
                           function(x) data.frame(read_excel(input_dataframes_filepath, x, 
                                                             col_names = TRUE,  
                                                             col_types = "text")))

names(input_dataframes) <- sheets
input_dataframes <- purrr::map(.x = input_dataframes, .f = ~rename_with(.x, ~ gsub(".", " ", .x, fixed = TRUE)))


#Read in the input text word document for the Sailing Order
input_SO <- read_docx("./_text/SailingOrders/input_text_Sailing_Order.docx")
text_SO <- docx_summary(input_SO)
text_SO <- text_SO[which(nchar(text_SO$text)!=0),]
text_SO_list <- split(text_SO$text, seq(nrow(text_SO)))
text_SO_list <- text_SO_list[which(sapply(text_SO_list, function(xx) nchar(xx)!=0))]

level1 <- which(text_SO$level==1)
level2 <- which(text_SO$level==2)

text_SO_list[level1] <- paste("*", text_SO_list[level1], sep = " ")
text_SO_list[level2] <- paste("\t +", text_SO_list[level2], sep = "\t")



# #A universal file needs to be created that will contain all the areas used for 
# #each depth stratum for biomass estimations throughout DFN's operations extent. 
# #For the purposes of this report an excel file is used. Gradually this info will be drawn from the StoX projects
# areas_df <- read_excel("./_shapefiles/areas.xlsx", sheet = "areas", col_names = TRUE)
# 
# 
# spTaxa <- readRDS("./_data_input/taxa_df.rds")
# spTaxa <- spTaxa %>% 
#   mutate(Main_groups=case_when(
#     family %in% c('Sparidae', 'Haemulidae', 'Sciaenidae', 'Serranidae', 'Lutjanidae', 
#                   'Ariidae', 'Merlucciidae', 'Lethrinidae', 'Ophidiidae')~'Demersal',
#     family %in% c('Clupeidae', 'Carangidae', 'Scombridae', 'Sphyraenidae', 'Trichiuridae', 
#                   'Engraulidae', 'Dussumieriidae', 'Stromateidae')~'Pelagic',
#     family %in% c(#'Caridea', 
#       'Aciculopodidae', 'Aegeridae', 'Aristeidae', 'Benthesicymidae', 
#       'Carpopenaeidae', 'Penaeidae', 'Sicyoniidae', 'Solenoceridae')~'Shrimps', 
#     class %in% c('Cephalopoda')~'Cephalopods', 
#     order %in% c('Carcharhiniformes', 'Heterodontiformes', 'Hexanchiformes', 'Lamniformes', 
#                  'Orectolobiformes', 'Pristiophoriformes', 'Squaliformes', 'Squatiniformes')~'Sharks',
#     order %in% c('Myliobatiformes','Rajiformes', 'Torpediniformes', 'Rhinopristiformes')~'Rays',
#     TRUE~ "Other"),
#     CommImportantDemSp=case_when(family %in% c('Sciaenidae')~'Croakers',
#                                  family %in% c('Serranidae')~'Groupers',
#                                  genus %in% c('Haemulopsis')~'Grunts',
#                                  family %in% c('Sparidae')~'Seabreams',
#                                  family %in% c('Lutjanidae')~'Snappers',
#                                  TRUE~ "Other"),
#     MainPelFamilies=case_when(family %in% c('Sphyraenidae')~'Barracuda',
#                               family %in% 'Carangidae'~'Carangidae',
#                               order %in% 'Clupeiformes'~'Clupeids',
#                               family %in% 'Trichiuridae'~'Hairtails',
#                               family %in% 'Scombridae'~'Scombrids',
#                               TRUE~ "Other")) %>% distinct(scientificname, .keep_all = T)

###############################################
