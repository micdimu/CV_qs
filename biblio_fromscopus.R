library(rscopus)
library(tidyverse)
library(bibtex)
library(rcrossref)    
library(RefManageR)
library(lubridate)
library(quarto)

if(!dir.exists("input")){
  dir.create("input")
}

quarto_add_extension(extension = "mps9506/quarto-cv", no_prompt = T)
#### Publication section #####


rscopus::set_api_key("dcc45b1f237b20ba44fd8b10ed7a38d6")

myscopus <- rscopus::author_df(last_name = "Di Musciano", first_name = "Michele", api_key = "dcc45b1f237b20ba44fd8b10ed7a38d6", verbose = F)

citazioni <- map(myscopus$`prism:doi`, function(x)
  cr_cn(dois = x, "bibentry", "apa")
)

formatAuthor <- function(x){
  if(is.null(x)){return(NA)
  }else{
    uno <- x |> 
      str_split(pattern = " and ")
    
    due <- uno[[1]] |> 
      word(2, sep = ", ") %>% 
      gsub('(*UCP)(\\b\\p{L}|[;-])(*SKIP)(*F)|.','', . ,perl = TRUE) %>%  
      strsplit("") |> 
      map_chr(\(x)paste(x, collapse = ". ") |> 
                paste(".", sep = "") )

    tre <- paste(uno[[1]] |>
                   word(1, sep = ", "),
                 due)
    tre[grep("Musciano",tre)] <- " <b>**Di Musciano M.**</b>"
    
    quattro <- tre |> 
      paste0(collapse = "; ") |> 
      (\(.)gsub(" -. ", "-", ., fixed = T))()
    
    return(quattro)
  }
}

authors <- map(citazioni, \(x){
  print(x$title)
  formatAuthor(x$author)
}) 

cinque <- map2_df(authors, citazioni, \(x, y){
  bind_cols(aut = x, year = y$year, title = y$title, journ = y$journal, 
            vol = y$volume, ISSN = y$ISSN, doi = y$DOI)
}) |> 
  filter(!is.na(aut))

cinque$vol[is.na(cinque$vol)] <- ""
cinque$aut[1] <- paste("*", cinque$aut[1])

alfredo <- paste0(paste(cinque$aut, 
                        paste("(", cinque$year, ")", sep = ""),
                        paste("**", cinque$title, "**", sep = ""), "-",
                        paste("*", cinque$journ, "*", sep = ""), "-", cinque$vol,
                        cinque$ISSN, cinque$doi), collapse = " \\newline \\newline ")

saveRDS(alfredo, "input/publication.RDS")



##### Bibliometrics indicators

auth_info <- rscopus::author_retrieval(last_name = "Di Musciano", first_name = "Michele", 
                                 api_key = "dcc45b1f237b20ba44fd8b10ed7a38d6", verbose = F, view = "METRICS")

simpl_auth_info <- auth_info$content$`author-retrieval-response`[[1]]

sum_auth_info <- paste(
  paste("<b>**Document:**</b>", simpl_auth_info$coredata$`document-count`), 
  paste("<b>**Cited by:**</b>", simpl_auth_info$coredata$`citation-count`),
  paste("<b>**h-index:**</b>", simpl_auth_info$`h-index`), sep = "; ")

saveRDS(sum_auth_info, "input/bblio_ind.RDS")


##### Conference

confarr <- read.csv(file = "input/Conferences_NoSpecialCharacters.csv", header = T, sep = ",") |> 
  mutate(From = dmy(From)) |> 
  mutate(To = dmy(To)) |> 
  arrange(desc(To)) |> 
  mutate(Role = paste(Role, X, X.1, X.2, sep = "; ")) |> 
  mutate(Role = gsub("; ; ;", "", Role)) |> 
  mutate(Role = gsub("; ;", "", Role))


conflyout <- confarr$Conference |> 
  (\(.)gsub("th ", "^th^ ", ., fixed = T))() |> 
  (\(.)gsub("3rd ", "3^rd^ ", ., fixed = T))() |> 
  (\(.)gsub("2nd ", "2^nd^ ", ., fixed = T))() |> 
  paste(" (", confarr$Organization,")", sep = "") |> 
  paste(gsub("Di Musciano M.", " <b>**Di Musciano M.**</b>", confarr$Author), confarr$Title, 
        paste("<b>**", confarr$Role, "**</b>", sep = ""), 
        paste(confarr$Where, " (", confarr$From, " - ", confarr$To, ").", sep = ""), sep = " - ") |> 
  paste(collapse = " \\newline \\newline ") |> 
  (\(.)paste("- ", .))()

saveRDS(conflyout, "input/conference.RDS")



