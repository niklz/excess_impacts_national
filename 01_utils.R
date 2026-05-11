# provides all available information from the Organsational Data Services API
# based on provided health code
ods_info <- function(health_org_code) {
  
  url <- paste0(
    "https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations/",
    health_org_code
  )
  
  httpResponse <- httr::GET(url, httr::accept_json())
  ods_information <- jsonlite::fromJSON(
    httr::content(
      httpResponse, 
      "text", 
      encoding="UTF-8"
    )
  )
  return(ods_information)
}

ods_lookup <- function(health_org_code, table1, table2, filter_category) {
  if (is.na(health_org_code)) return(NA)
  
  ods_information <- ods_info(health_org_code)
  
  
  lkp <- purrr::pluck(
    ods_information,
    "Organisation",
    table1,
    table2
  )
  
  if (!is.null(lkp)) {
    if (filter_category == "Active") {
      lkp <- lkp |> 
        filter(
          Status == "Active"
        )
    } else if (filter_category == "Successor") {
      lkp <- lkp |> 
        filter(
          Type == "Successor"
        )
    }
    
    if (nrow(lkp) > 0) {
      lkp <- lkp |> 
        tibble() |> 
        unnest(cols = Target) |> 
        unnest(cols = OrgId) |> 
        pull(extension)
    } else {
      lkp <- NA
    }
    
  } else {
    lkp <- NA
  }
  
  return(lkp)
}


# identifies active parent organisations for health org code provided
health_org_lookup <- function(health_org_code) {
  lkp <- ods_lookup(
    health_org_code,
    table1 = "Rels",
    table2 = "Rel",
    filter_category = "Active"
  )
  
  return(lkp)
}