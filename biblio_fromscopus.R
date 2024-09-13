library(rscopus)
library(tidyverse)
library(bibtex)
library(rcrossref)    
library(RefManageR)
library(lubridate)
library(quarto)
library(scholar)

if(!dir.exists("input")){
  dir.create("input")
}

#quarto_add_extension(extension = "mps9506/quarto-cv", no_prompt = T)
#### Publication section #####


rscopus::set_api_key("dcc45b1f237b20ba44fd8b10ed7a38d6")

myscopus <- rscopus::author_df(last_name = "Di Musciano", first_name = "Michele", api_key = "dcc45b1f237b20ba44fd8b10ed7a38d6", verbose = F)

myscopus$`prism:aggregationType`


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
            vol = y$volume, ISSN = y$ISSN, doi = y$DOI, book = y$booktitle)
}) |> 
  filter(!is.na(aut))

## solve Flora Mediterranea ##

flora_medit <- bind_cols(
  aut = "Di Cecco V.; Frattaroli A.R.; **Di Musciano M.**; Di Santo M.; Di Martino L.", 
  year = "2021", 
  title = "Seed germination reports for Policy species in the central Apennines", 
  journ = "Flora Mediterranea", 
  vol = "31", 
  ISSN = "2240-4538", 
  doi = "10.7320/flmedit31.277")

cinque <- bind_rows(cinque, flora_medit)|> 
  mutate(doi = tolower(doi)) 

## add missing from scopus ##

newpaperi <- bind_cols(
  aut = "<b>**Di Musciano M.**</b>; Calvia G.; Ruggero A.; Farris E.; Ricci L.; Frattaroli A.R.; Bagella, S.", 
  year = "2024", 
  title = "Elevational patterns of species richness and phylogenetic diversity in a Mediterranean island", 
  journ = "Perspectives in Plant Ecology, Evolution and Systematics", 
  vol = "65", 
  ISSN = "1433-8319", 
  doi = "10.1016/j.ppees.2024.125815")

cinque <- bind_rows(cinque, newpaperi)|> 
  mutate(doi = tolower(doi)) 

## be carefull of NA presence ##

cinque$vol[is.na(cinque$vol)] <- ""

## solve coauthors grouped ##
cinque$aut[-grep("Di Musciano", cinque$aut)] <- gsub("Djukic I." , "Djukic I.; **TeaComposition Network (Di Musciano M.)**",
                                                     cinque$aut[-grep("Di Musciano", cinque$aut)])

### book or not book ###

settete <- myscopus |> 
  select(doi = `prism:doi`, jb = `prism:aggregationType`) |> 
  mutate(doi = tolower(doi)) |> 
  full_join(cinque)

sei <- settete |> 
  filter(!grepl("Correction", title)) |> 
  filter(jb == "Journal" | is.na(jb))

#### order by year ###

sei <- sei |> 
  mutate(year = as.numeric(year)) |> 
  arrange(desc(year))

sei$aut[1] <- paste("*", sei$aut[1])

alfredo <- paste0(paste(sei$aut, 
                        paste("(", sei$year, ")", sep = ""),
                        paste("**", sei$title, "**", sep = ""), "-",
                        paste("*", sei$journ, "*", sep = ""), "-", paste0(sei$vol,", "),
                        sei$ISSN, "- **doi:** ", sei$doi), collapse = " \\newline \\newline ") |> 
  (\(.) gsub(" , ", "", .))()

saveRDS(alfredo, "input/publication.RDS")

### tutto il resto ####

otto <- settete |> 
  filter(jb != "Journal")
#### order by year ###

otto <- otto |> 
  mutate(year = as.numeric(year)) |> 
  arrange(desc(year))

otto$aut[1] <- paste("*", otto$aut[1])


baciami <- paste0(paste(otto$aut, 
                        paste("(", otto$year, ")", sep = ""),
                        paste("**", otto$title, "**", sep = ""), "-",
                        paste("*", otto$book, "*", sep = ""), "-", paste0(otto$vol,", "),
                        otto$ISSN, "- **doi:** ", otto$doi), collapse = " \\newline \\newline ") |> 
  (\(.) gsub(" , ", "", .))()

saveRDS(baciami, "input/other_publication.RDS")


##### Bibliometrics indicators

auth_info <- rscopus::author_retrieval(last_name = "Di Musciano", first_name = "Michele", 
                                 api_key = "dcc45b1f237b20ba44fd8b10ed7a38d6", verbose = F, view = "METRICS")

simpl_auth_info <- auth_info$content$`author-retrieval-response`[[1]]

sum_auth_info <- paste(
  paste("<b>**Document:**</b>", simpl_auth_info$coredata$`document-count`), 
  paste("<b>**Cited by:**</b>", simpl_auth_info$coredata$`citation-count`),
  paste("<b>**h-index:**</b>", simpl_auth_info$`h-index`), sep = "; ") |> 
  paste("(Scopus)")
  

saveRDS(sum_auth_info, "input/bblio_ind.RDS")


##### Conference #####

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

##### Poster #####

confarr_post <- read.csv(file = "input/Posters_NoSpecialCharacters.csv", header = T, sep = ",") |> 
  mutate(From = dmy(From)) |> 
  mutate(To = dmy(To)) |> 
  arrange(desc(To)) 

conflyout_post <- confarr_post$Conference |> 
  (\(.)gsub("th ", "^th^ ", ., fixed = T))() |> 
  (\(.)gsub("3rd ", "3^rd^ ", ., fixed = T))() |> 
  (\(.)gsub("2nd ", "2^nd^ ", ., fixed = T))() |>
  (\(.)gsub("1st ", "1^st^ ", ., fixed = T))() |> 
  paste(" (", confarr_post$Organization,")", sep = "") |> 
  paste(gsub("Di Musciano M.", " <b>**Di Musciano M.**</b>", confarr_post$Author), confarr_post$Title, 
        paste("<b>**", confarr_post$Role, "**</b>", sep = ""), 
        paste(confarr_post$Where, " (", confarr_post$From, " - ", confarr_post$To, ").", sep = ""), sep = " - ") |> 
  paste(collapse = " \\newline \\newline ") |> 
  (\(.)paste("- ", .))()

saveRDS(conflyout_post, "input/poster.RDS")

#### summary conferences ####

sum_post <- read.csv(file = "input/Conferences_NoSpecialCharacters.csv", header = T, sep = ",") |> 
  dplyr::select(Role, X, X.1) |> 
  pivot_longer(everything()) |> 
  filter(value != "") |> 
  pull(value) |> 
  c(read.csv(file = "input/Posters_NoSpecialCharacters.csv", header = T, sep = ",") |> 
  pull(Role))
  
sum_conf <- sum_post |> 
  as.data.frame() |>
  filter(sum_post != "Chair") |> 
  mutate(sum_post = case_when(
    sum_post == "Speaker" ~ "Oral Communications",
    TRUE ~ sum_post
  )) |> 
  mutate(sum_post = factor(sum_post, levels = c("Oral Communications", "Invited speaker", "Poster", "Member of the Scientific Committee", "Member of the Organizing Committee"))) |>
  count(sum_post)
  
confsumm_lyout <- paste(paste("<b>**", sum_conf$sum_post, "**</b>", sep = ""), " = ", sum_conf$n) |> 
  paste(collapse = " \\newline") |> 
  (\(.)paste("- ", .))()

saveRDS(confsumm_lyout, "input/sum_conf.RDS")

#### summary conferences ITA ####

sum_conf_ITA <- sum_post |> 
  as.data.frame() |>
  filter(sum_post != "Chair") |> 
  mutate(sum_post = case_when(
    sum_post == "Speaker" ~ "Comunicazioni orali",
    sum_post == "Member of the Scientific Committee" ~ "Membro del comitato scientifico", 
    sum_post == "Member of the Organizing Committee" ~ "Membro del comitato organizzatore",
    TRUE ~ sum_post
  )) |> 
  mutate(sum_post = factor(sum_post, levels = c("Comunicazioni orali", "Invited speaker", "Poster", "Membro del comitato scientifico", "Membro del comitato organizzatore"))) |>
  count(sum_post)

confsumm_lyout_ITA <- paste(paste("<b>**", sum_conf_ITA$sum_post, "**</b>", sep = ""), " = ", sum_conf_ITA$n) |> 
  paste(collapse = " \\newline") |> 
  (\(.)paste("- ", .))()

saveRDS(confsumm_lyout_ITA, "input/sum_conf_ITA.RDS")


##### le top dodici ###

ltopd <- read.csv("input/letopdodici.csv")


dod <- ltopd |> 
  dplyr::select(-year, doi = DOI) |> 
  left_join(sei) |> 
  left_join(myscopus |>
              select(doi = `prism:doi`, citedb = `citedby-count`) |>
              mutate(doi = tolower(doi))) |> 
  mutate(citedb = case_when(
    is.na(citedb) ~ "0",
    TRUE ~ citedb
  )) |> 
  mutate(citedb = as.numeric(citedb)) |> 
  add_column(cnt = 1:12)


banco <- paste0(
  paste(
    paste(dod$cnt, ") ", sep = ""),
    dod$aut,
    paste("(", dod$year, ")", sep = ""),
    paste("**", dod$title, "**", sep = ""), "-",
    paste("*", dod$journ, "*", sep = ""), "-", 
    paste0(dod$vol,", "),
    dod$ISSN, 
    "- **doi:** ", dod$doi, 
    "- **Role:** ", dod$AuthContr, " author",
    "- **Quartile:** ", dod$Quartile, 
    "- **IF:** ", dod$IF, 
    "- **Cited by:** ", dod$citedb, " (scopus)"),
  collapse = "\\newline  \n") |>
  (\(.) gsub(" , ", "", .))()

saveRDS(banco, "input/dodici_pub.RDS")

### highly cited paper