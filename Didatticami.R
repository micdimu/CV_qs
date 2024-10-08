library(tidyverse)
library(data.table)

did_eng <- read.csv("input/Didattica_eng.csv")

did_eng_out <- did_eng |> 
  (\(.) paste("Course lecturer: ", "**", .$name, "** ",
      "(", did_eng$cfu, "CFU - ", .$hour , "h), ",
      "**SSD:** ", .$SSD, " ",
      "**Accademic year:** ", .$Ays, ", ",
      "**Semester:** ", .$semester, ", ",
      "**Degree program:** ", .$degreeprogram, " ",
      "**Istitution:** ", .$where,
      sep = ""))() |> 
  paste(collapse = " \\newline \\newline ") |> 
  (\(.)paste("- ", .))()

saveRDS(did_eng_out, "input/did_eng.RDS")


did_ita <- read.csv("input/Didattica_ita.csv")

did_ita_out <- did_ita |> 
  (\(.) paste("Corso: ", "**", .$name, "** ",
              "(", did_eng$cfu, "CFU - ", .$hour , "h), ",
              "**SSD:** ", .$SSD, " ",
              "**Anno Accademico:** ", .$Ays, ", ",
              "**Semestre:** ", .$semester, ", ",
              "**Corso di studi:** ", .$degreeprogram, " ",
              "**Istituzione:** ", .$where,
              sep = ""))() |> 
  paste(collapse = " \\newline \\newline ") |> 
  (\(.)paste("- ", .))()

saveRDS(did_ita_out, "input/did_ita.RDS")


# docenza per la lezione svolta all’interno del corso di
# guide escursionistiche del Parco Nazionale della Maiella riguardante il ruolo delle
# aree protette negli scenari di cambiamenti climatici per la conservazione della
# biodiversità. 

#### old but gold 
# 
# -   Course lecturer: **Applied Botany** (3CFU - 30h) Master degree in Environmental Biology and Ecosystem Management. **SSD:** BIOS-01/C (ex BIO/03). <b>**Accademic Years:**</b> 2022-2023, 2023-2024, <b>**Semester:**</b> II.
# 
# -   Course lecturer: **Evolutionary Botany** (3CFU - 30h) Bachelor degree in Biology. **SSD:** BIOS-01/C (ex BIO/03). <b>**Accademic Year:**</b> 2023-2024, <b>**Semester:**</b> II.
# 
# -   Course lecturer: **Ecological Modelling** (3CFU - 30h) Master degree in Environmental Biology and Ecosystem Management. **SSD:** BIOS-01/C (ex BIO/03). <b>**Accademic Year:**</b> 2023-2024, <b>**Semester:**</b> I.
# 
# -   Course lecturer: **Management of Biological and Environmental Data with R Studio** (1CFU - 10h) Bachelor degree in Environmental Science and Technology, optional course (AFO). **SSD:** BIOS-01/C (ex BIO/03). <b>**Accademic Year:**</b> 2023-2024, <b>**Semester:**</b> I.
# 
# -   Course lecturer: **Systematic Botany** (3CFU - 30h) Bachelor degree in Biology. **SSD:** BIOS-01/C (ex BIO/03). <b>**Accademic Year:**</b> 2022-2023, <b>**Semester:**</b> II.
# 
# -   Course lecturer: **Management of Biological and Environmental Data with R Studio** (3CFU - 30h) Bachelor degree in Environmental Science and Technology, optional course (AFO). **SSD:** BIOS-01/C (ex BIO/03). <b>**Accademic Year:**</b> 2022-2023, <b>**Semester:**</b> I.
# 
# -   Course lecturer: **Applied Botany** (3CFU - 24h) Master degree in Environmental Biology and Ecosystem Management. **SSD:** BIOS-01/C (ex BIO/03). <b>**Accademic Years:**</b> 2021-2022 <b>**Semester:**</b> II.
# 
# -   Course lecturer: **Introduction to R** (1CFU - 8h) Bachelor degree in Environmental Science and Technology, optional course (AFO). <b>**Accademic Year:**</b> 2021-2022, <b>**Semester:**</b> II.
# 
# -   Course lecturer: **Introduction to R and management of environmental data** (1CFU - 8h) Master degree in Environmental Biology and Ecosystem Management, optional course (AFO). <b>**Accademic Year:**</b> 2019-2020, <b>**Semester:**</b> I.
# 
# -   Course lecturer: **Introduction to R** (1CFU - 8h) Master degree in Environmental Biology and Ecosystem Management, optional course (AFO). <b>**Accademic Year:**</b> 2018-2019, <b>**Semester:**</b> I.
# 
