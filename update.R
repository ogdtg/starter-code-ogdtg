# Load Packages

library(stringr)
library(jsonlite)
library(httr)
library(dplyr)

##################################################################################################################
# Define Functions

#' Get the OGD catalog
#'
#' @return A dataframe with meta information on the OGD data catalog of the Cantone of Thurgau
#' @description Function to get meta information on the current catalog of datasets available on data.tg.ch
#' @export
#'
get_catalog <- function() {
  res = httr::GET("https://data.tg.ch/api/v2/catalog/exports/json")
  if (res$status_code != 200) {
    stop("The API returned an error. Please check your internet connection or visit data.tg.ch for more information")
  }
  result=jsonlite::fromJSON(rawToChar(res$content), flatten = TRUE)
  return(result)
}




#' Prepare Notebooks and Markdown files for starter-code
#'
#' @param type either `pynb`,`rnb`,`rmd` or `rmdgeo`
#' @param catalog_id index in the catalog
#' @param catalog metadata catalog (`get_catalog`)
#'
#' @return
#' @export
#'
#' @examples
prepare_starter <- function(type,catalog_id, catalog){

  if(type == "pynb"){
    fileName <- "pattern/py_pattern.ipynb"
    new_dir <- "03_Py_Notebooks/"
    ext <- ".ipynb"

  }else if (type == "rmd") {
    new_dir = "02_R_Markdowns/01_CSV/"
    fileName <- "pattern/rmd_pattern.Rmd"
    ext <- ".Rmd"


  }else if (type== "rnb") {
    new_dir <- "01_R_Notebooks/"
    fileName <- "pattern/rnb_pattern.ipynb"
    ext <- ".ipynb"


  }else if (type == "rmdgeo") {
    fileName <- "pattern/rmdgeo_pattern.Rmd"
    new_dir <- "02_R_Markdowns/02_Geodata/"
    ext <- ".Rmd"


  }else (
    stop("Type not valid")
  )

  fields <- catalog$fields[catalog_id][[1]]$name

  if (length(fields)>0 | type=="rmdgeo") {



    id <- catalog$dataset_id[catalog_id]



    license <-
      ifelse(
        is.null(catalog$metas.default.license[catalog_id]) ||
          is.na(catalog$metas.default.license[catalog_id]),
        "",
        catalog$metas.default.license[catalog_id]
      )
    title <-
      ifelse(
        is.null(catalog$metas.default.title[catalog_id]) ||
          is.na(catalog$metas.default.title[catalog_id]),
        "",
        catalog$metas.default.title[catalog_id]
      )
    modified <-
      ifelse(
        is.null(catalog$metas.default.modified[catalog_id]) ||
          is.na(catalog$metas.default.modified[catalog_id]),
        "",
        lubridate::ymd_hms(catalog$metas.default.modified[catalog_id]) %>%
          format("%Y-%m-%d %H:%M:%S")
      )
    keywords <-
      ifelse(
        is.null(catalog$metas.default.keyword[catalog_id]) ||
          is.na(catalog$metas.default.keyword[catalog_id]),
        "",
        paste(unlist(catalog$metas.default.keyword[catalog_id]), collapse = ", ")
      )
    description <-
      ifelse(
        is.null(catalog$metas.default.description[catalog_id]) ||
          is.na(catalog$metas.default.description[catalog_id]),
        "",
        stringr::str_replace_all(catalog$metas.default.description[catalog_id], '\\"', "\'")
      )
    created <-
      ifelse(
        is.null(catalog$metas.dcat.issued[catalog_id]) ||
          is.na(catalog$metas.dcat.issued[catalog_id]),
        "",
        lubridate::ymd(catalog$metas.dcat.issued[catalog_id]) %>%
          format("%Y-%m-%d")
      )


    prep_data <- readChar(fileName, file.info(fileName)$size)



    prep_data_mod <- stringr::str_replace_all(prep_data,c("_DATANAME_"=title,
                                                          "_CREATED_"=created,
                                                          "_MODIFIED_"= modified,
                                                          "_LICENSE_"=license,
                                                          "_DESCRIPTION_"=description,
                                                          "_KEYWORDS_"=keywords,
                                                          "_IDENTIFIER_"=id,
                                                          "pattern/py_pattern" = paste0(new_dir,type,"_ogdtg@",id),
                                                          "pattern/rnb_pattern" = paste0(new_dir,type,"_ogdtg@",id))
    )

    if (type %in% c("rmd","rmdgeo")) {
      prep_data_mod <- stringr::str_replace_all(prep_data_mod,c("geo_rmd_pattern"= title,
                                                "rmd_pattern" = title))
    }

    prep_data_mod <- stringr::str_remove_all(prep_data_mod,"\\r")


    con <- file(paste0(new_dir,type,"_ogdtg@",id,ext), open = "wt", encoding = "UTF-8")


    sink(con)
    cat(prep_data_mod)
    sink()
    close(con)
    message(paste0(id," saved as ",new_dir,type,"_ogdtg@",id,ext))

    return(paste0(new_dir,type,"_ogdtg@",id,ext))

  }
}


#' Add Table rows to README
#'
#' @param id dataset_id
#' @param title dataset title
#' @param py_colab Python Colab File Name
#' @param r_colab R Colab File Name
#' @param r_markdown Rmarkdown File Name
#'
#' @return one Table row for the README
#' @export
#'
edit_readme <- function(id,title,py_colab =NULL,r_colab = NULL, r_markdown){

  py_colab_string = "![Py:Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/ogdtg/starter-code-ogdtg/blob/main/"
  r_colab_string = "![R:Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/ogdtg/starter-code-ogdtg/blob/main/"
  py_github <- "[Python Github](https://github.com/ogdtg/starter-code-ogdtg/blob/main/"
  r_github = "[R Github](https://github.com/ogdtg/starter-code-ogdtg/blob/main/"
  r_markdown_string = "[RMarkdown](https://github.com/ogdtg/starter-code-ogdtg/blob/main/"


  rep_py_colab_string = paste0(py_colab_string,py_colab)
  rep_r_colab_string = paste0(r_colab_string,r_colab)
  rep_py_github = paste0(py_github,py_colab)
  rep_r_github = paste0(r_github,py_colab)
  rep_r_markdown_string = paste0(r_markdown_string,r_markdown)

  if (is.null(r_colab)){
    rep_r_github = ""
    rep_r_colab_string = ""
  }
  if (is.null(py_colab)){
    rep_py_github = ""
    rep_py_colab_string = ""
  }



  readme_row <- '| _IDENTIFIER_ | [_DATANAME_](https://data.tg.ch/explore/dataset/_IDENTIFIER_/information/) |[![Py:Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/ogdtg/starter-code-ogdtg/blob/main/)| [![R:Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/ogdtg/starter-code-ogdtg/blob/main/) | [Python Github](https://github.com/ogdtg/starter-code-ogdtg/blob/main/) | [R Github](https://github.com/ogdtg/starter-code-ogdtg/blob/main/) | [RMarkdown](https://github.com/ogdtg/starter-code-ogdtg/blob/main/) |'

  readme_row_mod <- stringr::str_replace_all(readme_row,c("_DATANAME_"=substr(title,1,200),
                                                        "_IDENTIFIER_"=id,
                                                        "!\\[Py:Open In Colab\\]\\(https://colab.research.google.com/assets/colab-badge.svg\\)\\]\\(https://colab.research.google.com/github/ogdtg/starter-code-ogdtg/blob/main/" = rep_py_colab_string,
                                                        "!\\[R:Open In Colab\\]\\(https://colab.research.google.com/assets/colab-badge.svg\\)\\]\\(https://colab.research.google.com/github/ogdtg/starter-code-ogdtg/blob/main/" = rep_r_colab_string,
                                                        "\\[Python Github\\]\\(https://github.com/ogdtg/starter-code-ogdtg/blob/main/" = rep_py_github,
                                                        "\\[R Github\\]\\(https://github.com/ogdtg/starter-code-ogdtg/blob/main/" = rep_r_github,
                                                        "\\[RMarkdown\\]\\(https://github.com/ogdtg/starter-code-ogdtg/blob/main/" = rep_r_markdown_string)
  )
  readme_row_mod <- stringr::str_replace_all(readme_row_mod,c("\\| \\) \\|"="\\| \\|",
                                                          "\\| \\[\\) \\|"="\\| \\|",
                                                          "\\[\\)" = " ",
                                                          "\\| \\) \\|"="\\| \\|"
                                             ))
  return(readme_row_mod)
}

##########################################################################################################################



# whole catalog
catalog <- get_catalog()

# Catalog with CSV Data
catalog_csv <- catalog %>%
  filter(str_detect(dataset_id,"^[a-z]+-[a-z]+-\\d+$"))

# catalog with geodata
catalog_geo <- catalog %>%
  filter(!str_detect(dataset_id,"^[a-z]+-[a-z]+-\\d+$"))


# CSV Data Init
readme_list_csv <- list()

for (i in 1:nrow(catalog_csv)) {
  id = catalog_csv$dataset_id[i]
  title = catalog_csv$metas.default.title[i]


  py_colab = prepare_starter(type = "pynb",i,catalog_csv)
  r_markdown = prepare_starter(type = "rmd",i,catalog_csv)
  r_colab = prepare_starter(type = "rnb",i,catalog_csv)

  readme_string <- edit_readme(
    id = id,
    title = title,
    py_colab = py_colab,
    r_colab = r_colab,
    r_markdown =r_markdown
  )
  readme_list_csv[[i]] <- readme_string
}


# Geo Data Init
readme_list_geo <- list()

for (i in 1:nrow(catalog_geo)) {
  id = catalog_geo$dataset_id[i]
  title = catalog_geo$metas.default.title[i]


  r_markdown = prepare_starter(type = "rmdgeo",i,catalog_geo)

  readme_string <- edit_readme(
    id = id,
    title = title,
    r_markdown =r_markdown
  )
  readme_list_geo[[i]] <- readme_string
}



fileName <- "pattern/rm_pat.md"

current_time <- format(as.POSIXct(Sys.time()+ 2*60*60,tz = "GMT+2") ,"%Y-%m-%d %H:%M:%S")

readme <- readChar(fileName, file.info(fileName)$size)

readme <- str_replace_all(readme,"Latest update: \\*\\*.+?\\*\\*",
                          glue::glue("Latest update: \\*\\*{current_time}\\*\\*"))

readme <- str_replace_all(readme,"^.+?Taking inspiration from the",
                          ":bulb: Taking inspiration from the")

readme <- str_replace_all(readme,"\n.+?The Jupyter Notebooks can be opened directly in Google Colab.+?\n",
                          "\n :rocket: The Jupyter Notebooks can be opened directly in Google Colab. :rocket: \n")

readme <- stringr::str_remove_all(readme,"\\r")


geo_readme <- readme_list_geo %>% unlist() %>% paste(collapse = "\n")
csv_readme <- readme_list_csv %>% unlist() %>% paste(collapse = "\n")


full_readme <- paste(readme,csv_readme,"\n## Geodata\n",geo_readme,collapse = "\n")


con <- file("README.md", open = "wt", encoding = "UTF-8")


sink(con)
cat(full_readme)
sink()
close(con)
