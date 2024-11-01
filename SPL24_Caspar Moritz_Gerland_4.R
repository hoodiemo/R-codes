################################################################################ 
#################### Statistical Programming Languages 2024 ####################
####################            Take-home Exam              ####################
################################################################################

#-------------------------------------------------------------------------------
# Surname: Caspar Moritz
# Name: Gerland
# Student ID (Matrikelnummer): 629441
#-------------------------------------------------------------------------------

### Exercise 4 -----------------------------------------------------------------

# a) ---------------------------------------------------------------------------

# https://github.com/rstudio/cheatsheets/blob/main/strings.pdf for nice cheatsheet

library(stringr)

to_obish = function(text){
  # first check if the type of input is correct
  if (typeof(text) != "character"){
    stop("text must be of type 'character'.")
  }else{
    # split input text into individual words (at individual white spaces)
    words <- unlist(strsplit(text, "\\s+"))
    # remove the words that are not legit
    words <- words[!str_detect(words, "[0-9]|[:symbol:]")]
    # now make sure that only the first letter is capitalized if it is, 
    # put rest to lower
    first_part <- substr(words, 1, 1)
    rest = substr(words, 2, nchar(words))
    rest = tolower(rest)
    output_str = paste0(first_part, rest) # concatenate again
  }
  
  # get rid of all punctuation
  p_replace <- str_replace_all(output_str, "[:punct:]", "")
  
  # remove empty entries
  p_replace <- p_replace[p_replace != ""]
  
  # control for empty strings
  if (length(p_replace) == 0){
    return("text contains no words to be transferred.")}
  
  # replace all y with ß (not an English letter, so cannot be in an input word) 
  # and apply rule as before 
  y_replace <- str_replace_all(p_replace, pattern = "y(\\b)|y([^aeiou])", 
                               replacement = "ß\\2")
  Y_replace <- str_replace_all(y_replace, pattern = "Y([^aeiou])", 
                               replacement = "Ü\\1")
  
  # obobish as well as Obobish
  manipulate <- str_replace_all(Y_replace, 
                                pattern =  "(?<!([aeiouß]))([aeiouß])", 
                                replacement = "ob\\2")
  # define a short function that forces the matched pattern to lowercase 
  # st eg "Org" becomes "Oborg" instead of "ObOrg"
  manipulate <- str_replace(manipulate, pattern = "([AEIOUÜ])", 
                            replacement = function(match) paste0("Ob", 
                                                                 tolower(match)))
  
  # correct the ß's and ü's again
  final_output <- str_replace_all(manipulate, "ß", "y")
  final_output <- str_replace_all(final_output, "ü", "y")
  return(final_output)
}

# b) ---------------------------------------------------------------------------

from_obish = function(obish_text){
  # again check if the type of input is correct
  if (typeof(obish_text) != "character"){
    stop("obish_text must be of type 'character'.")
  }else{
    # split input text into individual words (at individual whitespaces)
    words <- unlist(strsplit(obish_text, "\\s+"))
    # remove the words that are not legit
    words <- words[!str_detect(words, "[0-9]|[:symbol:]")]
    # now make sure that only the first letter is capitalized if it is, 
    # put rest to lower
    first_part <- substr(words, 1, 1)
    rest = substr(words, 2, nchar(words))
    rest = tolower(rest)
    output_str = paste0(first_part, rest) # concatenate again
  }
  # get rid of all punctuation
  p_replace <- str_replace_all(output_str, "[:punct:]", "")
  
  # remove empty entries
  p_replace <- p_replace[p_replace != ""]
  
  # control for empty strings
  if (length(p_replace) == 0){
    return("obish_text contains no words to be transferred.")}
  
  # eliminate all "ob" if they are followed by a vowel
  reobish <- str_replace_all(p_replace, pattern = "ob([aeiou])", 
                             replacement = "\\1")
  # clear all ob if followed by y and wordbreak or a consonant
  reobish <- str_replace_all(reobish, pattern = "oby(\\b)", 
                             replacement = "y\\1")
  reobish <- str_replace_all(reobish, pattern = "oby([^aeiou])", 
                             replacement = "y\\1")
  # big ugly, but replaces eg "Obello" by "OBEllo" and then strips off the "OB"
  reobish <- str_replace_all(str_replace_all(reobish, pattern = "Ob([aeiou])", 
                                             replacement = function(match) toupper(match)), "OB", "")
  
  return(reobish) 
    
    
}

