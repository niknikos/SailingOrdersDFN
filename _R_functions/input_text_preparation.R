# For the report the user needs to write text in the input_text_Sailing_Order.docx located under the _text\SailingOrders\_SO_text folder of the project. 
# Ideally, the user should only be allowed to edit specific sections of the .docx but for the moment if restrictions 
# are applied to the .docx file it cannot be imported to R. Need to sort this out in a way. 
# The important thing is that the document must have manually (not automatic) numbered list for the sections are
# this is what identifies the paragraphs for the execution of the code (to break the document into chunks of text). 

# Break intro doc into chunks and remove any extra spaces created by MS Word formatting

#input_text_SO <- lapply(input_text_SO, function(x) str_squish(x))
input_text_SO <- lapply(text_SO_list, function(x) rm_white_multiple(x))
#input_text_SO <- lapply(input_text_SO, function(x) rm_white_multiple(x))
input_text_SO <- lapply(input_text_SO, function(x) rm_white_bracket(x))
input_text_SO <- lapply(input_text_SO, function(x) rm_white_comma(x))
input_text_SO <- lapply(input_text_SO, function(x) rm_white_punctuation(x))
input_text_SO <- lapply(input_text_SO, function(x) 
  gsub("\\:[[:blank:]]", 
       gsub("[[:blank:]]", "", str_extract(x, "\\:[[:blank:]]")), x, perl = TRUE))

input_text_SO <- tokenize_paragraphs(input_text_SO, )
input_text_SO <- lapply(input_text_SO, function(x) str_squish(x))

# select first element of each itemized list
first_l1 <- level1[c(TRUE,diff(level1)!=1)]
# add indentation to previous row
input_text_SO[first_l1-1] <- paste(input_text_SO[first_l1-1], "\n", sep = " ")

# select rows that starts with + (level 2 item) and add tab before
level2_bullets <-which(unlist(sapply(input_text_SO, function(xx) startsWith(xx,"+")),use.names=FALSE))
input_text_SO[level2_bullets] <- paste("\t",input_text_SO[level2_bullets],sep=" ")
 
names(input_text_SO) <- sapply(seq_along(input_text_SO), function(x) input_text_SO[[x]][str_which(input_text_SO[[x]][1],"^\\d\\.")])
names(input_text_SO) <- ifelse(names(input_text_SO)=='character(0)','NA',names(input_text_SO))

