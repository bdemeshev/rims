#' small rims package
#'
#' This package is designed for easy life of IMS Health company. 
#' 
#' To see the list of available functions click on index below!
#' Be happy, don't worry :)
#'
#' @name rims
#' @docType package
#' @author Boris Demeshev 
#' @import stringr stringdist reshape2 dplyr zoo ggplot2 erer devtools
NULL

.onLoad <- function(libname = find.package("bigr"), pkgname = "bigr") {
  # warning("One bear with balalaika and vodka has joined your R session!")
#   library("stringi")
#   library("stringr")
#   library("stringdist")
#   library("reshape2")
#   library("dplyr")
#   library("zoo")
#   
#   library("devtools")
#   library("ggplot2")
#   library("erer")
  options(stringsAsFactors = FALSE)
}





#' Convert string with a number in Russian tradition in numeric
#' 
#' Convert string with a number in Russian tradition in numeric
#' 
#' Russian standards prescribes to use comma as a decimal separator. 
#' This function removes spaces and converts string to number.
#' 
#' @param x the string with the number
#' @return numeric the number converted from the string
#' @export
#' @examples
#' rus2num("34 345,34")
rus2num <- function(x) {
  x <- gsub(",",".",x)
  x <- gsub(" ","",x)
  return(as.numeric(x))
}




#' Standartize character vector 
#' 
#' Standartize character vector 
#' 
#' Standartize means: trimming white space at the end and in the beginning,
#' elimination of punctiation, transliteration, removing capitalization
#' 
#' @param x the character vector
#' @param translit logical whether we transliterate vector into latin
#' @return standartized character vector
#' @export
#' @examples
#' str_stand("пРивет!")
str_stand <- function(z,translit=TRUE) {
  if(translit) z <- stri_trans_general(z, id = "Russian-Latin/BGN" )
  z %>% tolower() %>% 
    str_replace_all("[[:punct:]]"," ") %>%
    str_replace_all(" +"," ") %>% str_trim() %>% return()  
}

#' Convert vector of sentences to a data.frame of separate words
#' 
#' Convert vector of sentences to a data.frame of separate words
#' 
#' Convert vector of sentences to a data.frame of separate words
#' The output is a data.frame with 3 variables:
#' word, word_n (number of word), sent_n (number of sentence)
#' 
#' @param x the vector of sentences
#' @param cleanup logical indicates whether to remove punctuation and some other cleanup
#' @param sep separator of words, space by default
#' @return data.frame with 3 variables: word, word_n (number of word), sent_n (number of sentence)
#' @export
#' @examples
#' str_sent2words(c("привет","Маша, это я, Дубровский"))
str_sent2words <- function(x, cleanup=TRUE,sep=" ") {
  if (cleanup) x <- x %>% str_replace_all("[[:punct:]]"," ") %>%
    str_replace_all(" +"," ") %>% str_trim()
  d <- x %>% str_split(pattern = sep) %>% melt() %>% 
    group_by(L1) %>% mutate(n=row_number()) %>% select(word=value,sent_n=L1,word_n=n)
  return(d)    
}

#' Convert data.frame of separate words to a vector of sentences
#' 
#' Convert data.frame of separate words to a vector of sentences
#' 
#' Convert data.frame with 3 variables: 
#' word, word_n (number of word), sent_n (number of sentence)
#' To a vector of sentences
#' 
#' @param x the data.frame with 3 variables: word, word_n (number of word), sent_n (number of sentence)
#' @param sep the separator for words, space by default
#' @return a vector of sentences
#' @export
#' @examples
#' str_words2sent(str_sent2words(c("привет","Маша, это я, Дубровский")))
str_words2sent <- function(d,sep=" ") {
  d2 <- d %>% group_by(sent_n) %>% arrange(word_n) %>%
    summarise(sentence=paste(word,collapse = sep)) 
  return(d2$sentence)
}

#' Order words in a vector of sentences
#' 
#' This function order words in each sentence of a character vector
#' 
#' @param x the vector of sentences
#' @return a vector of sentences with ordered words
#' @export
#' @examples
#' str_orderwords(c("привет","Маша, это я, Дубровский"))
str_orderwords <- function(x, sep=" ") {
  d <- str_sent2words(x, sep=sep) # convert to data.frame
  d <- d %>% group_by(sent_n) %>% arrange(word) %>% mutate(word_n=row_number())
  str_words2sent(d, sep=sep) %>% # convert back to vector
    return()
}




#' Get specific word from a vector of character sentences
#'
#' Get specific word from a vector of character sentences
#'
#' This function uses the transliteration tradition where "й" goes to "y"
#' 
#' @param x the vector of cyrillic characters
#' @param num the number (numbers) of word, negative numbers mean "from the end of sentence"
#' @param remove.duplicate logical, indicates whether the same word should be reported once or twice
#' @return data frame of words with numbers
#' @export
#' @examples
#' str_word(c("привет","Маша, это я, Дубровский"))
str_word <- function(x, num=1, remove.duplicate=TRUE) {  
  # little clean up 
  # split into words and transform to data frame:
  d <- str_sent2words(x)
  
  # filter the correct words
  # from the beginning (positive num's)
  semi_plus <- d %>% filter(word_n %in% num[num>0])
  # from the end (negative num's)
  semi_minus <- d %>% group_by(sent_n) %>% filter((word_n-max(word_n)) %in% (1+num[num<0]))
  
  semi <- rbind_list(semi_plus,semi_minus) 
  if (remove.duplicate) semi <- unique(semi)
  
  # if word is missing add NA
  ans <- d %>% select(sent_n) %>% unique() %>% left_join(semi %>% filter(!word==""))
  # (!) when x="" str_split returns "" (not NA)
  
  # replace NA by ""
  ans$word[is.na(ans$word)] <- ""
  
  return(str_words2sent(ans))
}

#' Transliterate cyrillic text
#'
#' Transliterate cyrillic text
#'
#' This function uses the transliteration tradition where "й" goes to "y"
#' 
#' @param x the vector of cyrillic characters
#' @return transliterated vector
#' @export
#' @examples
#' str_translit("привет")
str_translit <- function(x) {
  return(stri_trans_general(x,"Russian-Latin/BGN"))
}

#' Change encoding of cyrillic text from "utf8" to "cp1251"
#'
#' Change encoding of cyrillic text from "utf8" to "cp1251"
#'
#' This function changes encoding of a vector from "utf8" to "cp1251"
#' This function applies changes only to character and factor variables
#' 
#' @param x the vector of cyrillic characters
#' @param backward (default FALSE) TRUE: utf8 to cp1251, FALSE: cp1251 to utf8
#' @return reencoded vector
#' @export
#' @examples
#' str_utf2cp0("привет")
str_utf2cp0 <- function(x, backward=FALSE) {
  ans <- x # this is done to preserve x that are non-character and non-factor
  if (is.character(x)) {
    if (backward) ans <- iconv(ans,from="cp1251",to="utf8")
    if (!backward) ans <- iconv(ans,from="utf8",to="cp1251")
  } 
  if (is.factor(x)) {
    if (backward) levels(ans) <- iconv(levels(ans),from="cp1251",to="utf8")
    if (!backward) levels(ans) <- iconv(levels(ans),from="utf8",to="cp1251")
  } 
  
  return(ans)
}

#' Change encoding of cyrillic text from "utf8" to "cp1251"
#'
#' Change encoding of cyrillic text from "utf8" to "cp1251"
#'
#' This function changes encoding of a vector or data.frame from "utf8" to "cp1251"
#' 
#' @param x the vector or data.frame of cyrillic characters
#' @param backward (default FALSE) TRUE: utf8 to cp1251, FALSE: cp1251 to utf8
#' @return reencoded vector or data.frame
#' @export
#' @examples
#' str_utf2cp("привет")
str_utf2cp <- function(x,backward=FALSE) {
  ans <- x
  if (is.character(x) | is.factor(x))
    ans <- str_utf2cp0(x, backward = backward)
  if (is.data.frame(x)) 
    colnames(ans) <- str_utf2cp0(colnames(ans), backward = backward)
    for (i in 1:ncol(ans)) ans[,i] <- str_utf2cp0(ans[,i], backward = backward)        
  return(ans)
}


#' Create base correspondance table from vector of etalon cathegory names
#'
#' Create base correspondance table from vector of etalon cathegory names
#'
#' This function creates basic correspondance table from vector of etalon cathegory names
#' 
#' @param x the vector of etalon cathegory names
#' @param add_original whether we include trivial correspondance etalon <-> etalon
#' @param translit logical whether we transliterate while standartising responses
#' @return data.frame with basic correspondance table 
#' @export
#' @examples
#' ct_start(c("Iphone","Samsung","HTC"))
ct_start <- function (etal_cat, add_original=FALSE,translit=TRUE) {
    ct_trivial <- data.frame(in_cat = etal_cat, out_cat = etal_cat)
    ans <- ct_trivial %>% mutate(in_cat = str_stand(in_cat,translit=translit)) %>% unique()
    if (add_original) ans <- rbind_list(ct_trivial,ans) %>% unique()
    return(ans)
  }


#' Find unmatched user responses given correspondance table
#'
#' Find unmatched user responses given correspondance table
#'
#' This function finds unmatched user responses given correspondance table
#' 
#' @param z the vector of user responces
#' @param ct actual correspondance table with in_cat and out_cat variables
#' @return vector of unmatched user responses
#' @export
#' @examples
#' ct_unmatched(z,ct)
ct_unmatched <- function(z,ct) {
  return(z[!z %in% ct$in_cat])
}


#' Create additional correspondance table from user responses and actual correspondance table
#'
#' Create additional correspondance table from user responses and actual correspondance table
#'
#' This function uses Levenstein distance to create additional entries for correspondance table
#' 
#' @param z the vector of user responces
#' @param ct actual correspondance table
#' @param max_dist maximum Levenstein distance
#' @return additional lines for correspondance table
#' @export
#' @examples
#' ct_new_block(z,ct)
ct_new_block <- function(z,ct,max_dist=1) {
  ct$erunda <- 0
  d <- data.frame(user_ans=z,erunda=0)
  
  d_ct <- left_join(d,ct,by="erunda") # формируем все возможные пары (user_ans,in_cat)
  ct_add <- d_ct %>% 
    mutate(dist=stringdist(user_ans,in_cat)) %>% # считаем расстояние
    filter(dist<=max_dist)  # отбираем те строки, где расстояние меньше max_dist
  ct_add <- ct_add %>%
    group_by(user_ans) %>% # находим наилучшие соответствия
    filter(dist==min(dist)) %>% 
    select(in_cat=user_ans,out_cat) %>% # отбираем переменные
    unique() # удаляем дубли строк
  ct_add <- ct_add[!ct_add$in_cat %in% ct$in_cat,] # только новые соответствия
  # anti_join(ct_add,ct,by="in_cat") #  found multiple encodings in character string
  
  return(ct_add)
}

#' Lucky guy function
#'
#' This function tries to create a final correspondance table 
#' starting from a vector of user answers and a vector of etalons
#' 
#' The Levenstein distance is used. When to matches give the same distance 
#' the first one is returned. Warning: for lucky guys only!
#' 
#' @param user_ans the vector of user responces
#' @param etalon the vector of etalon cathegories
#' @param translit logical whether to do transliteration
#' @param algorithm 1 means that is max_dist is set to + infinity, 
#' 2 means that iterative algorithm is used
#' 1 is better for finding one-to-one correspondance, 
#' 2 is better to find many-to-one correspondance
#' @return pretty correspondance table if you are a lucky guy
#' @export
#' @examples
#' ct_luckyguy(user_ans,etalon,translit = FALSE)
#' ct_luckyguy(user_ans,ct_start)
ct_luckyguy <- function(user_ans,etalon,algorithm = 1, ...) {
  if (is.data.frame(etalon)) ct <- etalon else ct <- ct_start(etalon,...)
  if (algorithm == 1) {
  ans <- ct_new_block(user_ans,ct,Inf) %>% group_by(in_cat) %>% 
    mutate(temp=row_number()) %>% filter(temp==1) %>% select(in_cat,out_cat) %>%
    rbind_list(ct) %>% filter(in_cat %in% user_ans) 
  }  
  if (algorithm == 2) {
    ans_unmatched <- ct_unmatched(user_ans,ct)
    while (length(ans_unmatched)>0) {
      md <- 1
      ct_more <- ct_new_block(ans_unmatched,ct)
      while (nrow(ct_more)==0) {
        md <- md + 1
        ct_more <- ct_new_block(ans_unmatched, ct, max_dist = md)
      }
      ct <- rbind_list(ct,ct_more)
      ans_unmatched <- ct_unmatched(ans_unmatched,ct)
    } 
    ans <- ct %>% filter(in_cat %in% user_ans) %>% group_by(in_cat) %>% 
      mutate(n=row_number()) %>% filter(n==1) %>% select(in_cat,out_cat)
  }
    
  return(ans)
}



#' Convert excel numeric date encoding to date
#'
#' Convert excel numeric date encoding to date
#'
#' While reading excel files dates are sometimes replaced by their numeric codes.
#' This function recovers original dates from these codes.
#' 
#' @param x the vector of numeric date codes
#' @return the date
#' @export
#' @examples
#' excel2date(12345)
excel2date <- function(x) {
  ans <- as.Date(as.POSIXct((x-25569)*86400, tz="GMT", origin="1970-01-01"))
  return(ans)  
}

#' Replace two or more factor levels by the first one.
#'
#' Replace two or more factor levels by the first one.
#'
#' In raw data the same level of a factor variable may be encoded with errors, i.e. misprints. 
#' In this case many levels may correspond actually to one true level.
#' This function replaces all mentioned levels by the first one.
#' @param x the factor variable with misprints in level names
#' @param levels the levels that should be replaced by one level, levels[1] is used as the replacement.
#' @return correctly encoded x
#' @export
#' @examples
#' x <- c("Male","Female","male","Mle","Female")
#' unite_factors(x,c("Male","male","Mle"))
one_factor <- function(x,levels) {
  x[x %in% levels] <- levels[1]
  x <- droplevels(x)
  return(x)
}

#' Write .csv file using Russian Excel standard.
#'
#' Write .csv file using Russian Excel standard.
#'
#' Write .csv file using Russian Excel standard.
#'
#' @param obj data.frame to be written 
#' @param file file name
#' @examples
#' write_rus_csv(cars, file="cars.csv")
write_rus_csv <- function(obj, file) {
  write.table(obj, file = file, sep = ";", dec=".", row.names = FALSE)
}

