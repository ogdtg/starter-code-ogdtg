print(getwd())
file.exists(paste0(getwd(),"/pattern/pattern.ipynb"))
file.exists("pattern/pattern.ipynb")
list.files()

library(stringr)
#library(tgAPI)
library(dplyr)
library(httr)
library(jsonlite)

res <- httr::GET("https://data.tg.ch/api/explore/v2.0/catalog/exports/json") 
catalog <- jsonlite::fromJSON(rawToChar(res$content), flatten = TRUE)

catalog_mod <- catalog %>%
  filter(str_detect(dataset_id,"^[a-z]+-[a-z]+-\\d+$"))



#fileName <- "/__w/starter-code-ogdtg/starter-code-ogdtg/pattern/pattern.ipynb"
fileName <- "pattern/pattern.ipynb"

prep_data <- readChar(fileName, file.info(fileName)$size)
id <- catalog_mod$dataset_id[5]
res <- httr::GET(glue::glue("https://data.tg.ch/api/explore/v2.0/catalog/datasets/{id}"))

metadata <- res$content %>%
  rawToChar() %>%
  jsonlite::fromJSON()


fields <- metadata$dataset$fields$name

if (length(fields)>0) {
  license <- metadata$dataset$metas$default$license
  title <- metadata$dataset$metas$default$title
  modified <- metadata$dataset$metas$default$modified
  keywords <- paste(metadata$dataset$metas$default$keyword, collapse= ", ")
  description <- metadata$dataset$metas$default$description %>% str_replace_all('\\"',"\\'")
  fields <- metadata$dataset$fields
  created <- metadata$dataset$metas$dcat$issued

  prep_data_mod <- stringr::str_replace_all(prep_data,c("_DATANAME_"=title,
                                                        "_CREATED_"=created,
                                                        "_MODIFIED_"= modified,
                                                        "_LICENSE_"=license,
                                                        "_DESCRIPTION_"=description,
                                                        "_KEYWORDS_"=keywords,
                                                        "_IDENTIFIER_"=id,
                                                        "pattern/pattern" = paste0("ogdtg@",id))
                                                )
  prep_data_mod <- stringr::str_remove_all(prep_data_mod,"\\r")
  con <- file(paste0("ogdtg@",id,".ipynb"), open = "wt", encoding = "UTF-8")


  sink(con)
  cat(prep_data_mod)
  sink()
  close(con)
}
