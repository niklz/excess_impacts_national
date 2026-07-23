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

org_recodes <- c(
  "MIRIAM MINOR EMERGENCY" = "Miriam Minor Emergency",
  "EAST BERKS PRIMARY CARE OOH(WAM)" = "East Berks Primary Care OOH (WAM)",
  "SOUTH BIRMINGHAM GP WALK IN CENTRE" = "South Birmingham GP Walk In Centre",
  "PHL LYMINGTON UTC" = "PHL Lymington UTC",
  "KENT COMMUNITY HEALTH NHS FOUNDATION TRUST" = "Kent Community Health NHS Foundation Trust",
  "ERDINGTON GP HEALTH & WELLBEING WIC" = "Erdington GP Health & Wellbeing WIC",
  "MARKET HARBOROUGH MED.CTR" = "Market Harborough Medical Centre",
  "QUEEN VICTORIA HOSPITAL NHS FOUNDATION TRUST" = "Queen Victoria Hospital NHS Foundation Trust",
  "SOUTHERN HEALTH NHS FOUNDATION TRUST" = "Southern Health NHS Foundation Trust",
  "TETBURY HOSPITAL TRUST LTD" = "Tetbury Hospital Trust Ltd",
  "SOUTHPORT AND ORMSKIRK HOSPITAL NHS TRUST" = "Southport and Ormskirk Hospital NHS Trust",
  "BADGER LTD" = "Badger Ltd",
  "WHITSTABLE MEDICAL PRACTICE" = "Whitstable Medical Practice",
  "SOUTH WEST YORKSHIRE PARTNERSHIP NHS FOUNDATION TRUST" = "South West Yorkshire Partnership NHS Foundation Trust",
  "BRIGHTON STATION HEALTH CENTRE" = "Brighton Station Health Centre",
  "NOTTINGHAM CITYCARE PARTNERSHIP" = "Nottingham CityCare Partnership",
  "LATHAM HOUSE MEDICAL PRACTICE" = "Latham House Medical Practice",
  "ROYAL CORNWALL HOSPITALS NHS TRUST" = "Royal Cornwall Hospitals NHS Trust",
  "SANDWELL AND WEST BIRMINGHAM HOSPITALS NHS TRUST" = "Sandwell and West Birmingham Hospitals NHS Trust",
  "HERTFORDSHIRE COMMUNITY NHS TRUST" = "Hertfordshire Community NHS Trust",
  "PUTNOE MEDICAL CENTRE WALK IN CENTRE" = "Putnoe Medical Centre Walk In Centre",
  "DORSET HEALTHCARE UNIVERSITY NHS FOUNDATION TRUST" = "Dorset HealthCare University NHS Foundation Trust",
  "PAULTON MEMORIAL HOSPITAL" = "Paulton Memorial Hospital",
  "WYE VALLEY NHS TRUST" = "Wye Valley NHS Trust",
  "ST GEORGE'S UNIVERSITY HOSPITALS NHS FOUNDATION TRUST" = "St George's University Hospitals NHS Foundation Trust",
  "MEDWAY NHS FOUNDATION TRUST" = "Medway NHS Foundation Trust",
  "WIRRAL COMMUNITY HEALTH AND CARE NHS FOUNDATION TRUST" = "Wirral Community Health and Care NHS Foundation Trust",
  "SLEAFORD MEDICAL GROUP" = "Sleaford Medical Group",
  "SKELMERSDALE WALK IN CENTRE" = "Skelmersdale Walk In Centre",
  "CHESTERFIELD ROYAL HOSPITAL NHS FOUNDATION TRUST" = "Chesterfield Royal Hospital NHS Foundation Trust",
  "LIVERPOOL HEART AND CHEST HOSPITAL NHS FOUNDATION TRUST" = "Liverpool Heart and Chest Hospital NHS Foundation Trust",
  "ASSURA READING LLP" = "Assura Reading LLP",
  "WORKINGTON HEALTH LIMITED" = "Workington Health Limited",
  "CALDERDALE AND HUDDERSFIELD NHS FOUNDATION TRUST" = "Calderdale and Huddersfield NHS Foundation Trust",
  "GATESHEAD HEALTH NHS FOUNDATION TRUST" = "Gateshead Health NHS Foundation Trust",
  "SUMMERFIELD GP SURG & URGENT CARE CENTRE" = "Summerfield GP Surg & Urgent Care Centre",
  "OXFORD HEALTH NHS FOUNDATION TRUST" = "Oxford Health NHS Foundation Trust",
  "FIRST COMMUNITY HEALTH AND CARE CIC" = "First Community Health and Care CIC",
  "THE PINN UNREGISTERED WIC" = "The Pinn Unregistered WIC",
  "PRACTICE PLUS GROUP HOSPITAL - SOUTHAMPTON" = "Practice Plus Group Hospital - Southampton",
  "SHEFFIELD CHILDREN'S NHS FOUNDATION TRUST" = "Sheffield Children's NHS Foundation Trust",
  "OKEHAMPTON MEDICAL CENTRE" = "Okehampton Medical Centre",
  "EASTBOURNE STATION HEALTH CENTRE" = "Eastbourne Station Health Centre",
  "HASTINGS MED P & WALKIN" = "Hastings Med P & Walk-In",
  "PORTSMOUTH HOSPITALS UNIVERSITY NATIONAL HEALTH SERVICE TRUST" = "Portsmouth Hospitals University NHS Trust",
  "HOMERTON UNIVERSITY HOSPITAL NHS FOUNDATION TRUST" = "Homerton University Hospital NHS Foundation Trust",
  "ASSURA VERTIS URGENT CARE CENTRES (BIRMINGHAM)" = "Assura Vertis Urgent Care Centres (Birmingham)",
  "UNIVERSITY HOSPITAL SOUTHAMPTON NHS FOUNDATION TRUST" = "University Hospital Southampton NHS Foundation Trust",
  "SALISBURY NHS FOUNDATION TRUST" = "Salisbury NHS Foundation Trust",
  "YEOVIL DISTRICT HOSPITAL NHS FOUNDATION TRUST" = "Yeovil District Hospital NHS Foundation Trust",
  "LOCAL CARE DIRECT" = "Local Care Direct",
  "NORTHERN DEVON HEALTHCARE NHS TRUST" = "Northern Devon Healthcare NHS Trust",
  "ST.GEORGE'S CENTRE" = "St George's Centre",
  "UNIVERSITY COLLEGE LONDON HOSPITALS NHS FOUNDATION TRUST" = "University College London Hospitals NHS Foundation Trust",
  "SHROPSHIRE COMMUNITY HEALTH NHS TRUST" = "Shropshire Community Health NHS Trust",
  "WEST HERTFORDSHIRE HOSPITALS NHS TRUST" = "West Hertfordshire Hospitals NHS Trust",
  "EXMOUTH MINOR INJURY UNIT" = "Exmouth Minor Injury Unit",
  "HUMBER TEACHING NHS FOUNDATION TRUST" = "Humber Teaching NHS Foundation Trust",
  "URGENT CARE CENTRE (QMS)" = "Urgent Care Centre (QMS)",
  "SURREY AND SUSSEX HEALTHCARE NHS TRUST" = "Surrey and Sussex Healthcare NHS Trust",
  "WOKING WALK IN CENTRE" = "Woking Walk In Centre",
  "DERBYSHIRE COMMUNITY HEALTH SERVICES NHS FOUNDATION TRUST" = "Derbyshire Community Health Services NHS Foundation Trust",
  "THE DUDLEY GROUP NHS FOUNDATION TRUST" = "The Dudley Group NHS Foundation Trust",
  "BLACKPOOL TEACHING HOSPITALS NHS FOUNDATION TRUST" = "Blackpool Teaching Hospitals NHS Foundation Trust",
  "ASHFORD WALK-IN-CENTRE" = "Ashford Walk-In Centre",
  "EPSOM AND ST HELIER UNIVERSITY HOSPITALS NHS TRUST" = "Epsom and St Helier University Hospitals NHS Trust",
  "CENTRAL LONDON COMMUNITY HEALTHCARE NHS TRUST" = "Central London Community Healthcare NHS Trust",
  "NORTHAMPTON GENERAL HOSPITAL NHS TRUST" = "Northampton General Hospital NHS Trust",
  "SOUTH TYNESIDE AND SUNDERLAND NHS FOUNDATION TRUST" = "South Tyneside and Sunderland NHS Foundation Trust",
  "BUCKINGHAMSHIRE HEALTHCARE NHS TRUST" = "Buckinghamshire Healthcare NHS Trust",
  "MID YORKSHIRE HOSPITALS NHS TRUST" = "Mid Yorkshire Hospitals NHS Trust",
  "CAMBRIDGE UNIVERSITY HOSPITALS NHS FOUNDATION TRUST" = "Cambridge University Hospitals NHS Foundation Trust",
  "KETTERING GENERAL HOSPITAL NHS FOUNDATION TRUST" = "Kettering General Hospital NHS Foundation Trust",
  "BIRMINGHAM WOMEN'S AND CHILDREN'S NHS FOUNDATION TRUST" = "Birmingham Women's and Children's NHS Foundation Trust",
  "SHERWOOD FOREST HOSPITALS NHS FOUNDATION TRUST" = "Sherwood Forest Hospitals NHS Foundation Trust",
  "THE ROTHERHAM NHS FOUNDATION TRUST" = "The Rotherham NHS Foundation Trust",
  "OAKHAM MEDICAL PRACTICE" = "Oakham Medical Practice",
  "SOUTH WESTERN AMBULANCE SERVICE NHS FOUNDATION TRUST" = "South Western Ambulance Service NHS Foundation Trust",
  "UNIVERSITY HOSPITALS OF NORTH MIDLANDS NHS TRUST" = "University Hospitals of North Midlands NHS Trust",
  "WORCESTERSHIRE ACUTE HOSPITALS NHS TRUST" = "Worcestershire Acute Hospitals NHS Trust",
  "WILTSHIRE HEALTH & CARE" = "Wiltshire Health & Care",
  "NORTH EAST LONDON NHS FOUNDATION TRUST" = "North East London NHS Foundation Trust",
  "WALSALL HEALTHCARE NHS TRUST" = "Walsall Healthcare NHS Trust",
  "THE PRINCESS ALEXANDRA HOSPITAL NHS TRUST" = "The Princess Alexandra Hospital NHS Trust",
  "NORTH WEST ANGLIA NHS FOUNDATION TRUST" = "North West Anglia NHS Foundation Trust",
  "NORTHUMBRIA HEALTHCARE NHS FOUNDATION TRUST" = "Northumbria Healthcare NHS Foundation Trust",
  "DONCASTER AND BASSETLAW TEACHING HOSPITALS NHS FOUNDATION TRUST" = "Doncaster and Bassetlaw Teaching Hospitals NHS Foundation Trust",
  "BARKING, HAVERING AND REDBRIDGE UNIVERSITY HOSPITALS NHS TRUST" = "Barking, Havering and Redbridge University Hospitals NHS Trust",
  "NORTHERN LINCOLNSHIRE AND GOOLE NHS FOUNDATION TRUST" = "Northern Lincolnshire and Goole NHS Foundation Trust",
  "NORTH BRISTOL NHS TRUST" = "North Bristol NHS Trust",
  "HOUNSLOW AND RICHMOND COMMUNITY HEALTHCARE NHS TRUST" = "Hounslow and Richmond Community Healthcare NHS Trust",
  "ROYAL UNITED HOSPITALS BATH NHS FOUNDATION TRUST" = "Royal United Hospitals Bath NHS Foundation Trust",
  "GREAT WESTERN HOSPITALS NHS FOUNDATION TRUST" = "Great Western Hospitals NHS Foundation Trust",
  "SOUTH WARWICKSHIRE NHS FOUNDATION TRUST" = "South Warwickshire NHS Foundation Trust",
  "SIRONA CARE & HEALTH" = "Sirona Care & Health",
  "LEWISHAM AND GREENWICH NHS TRUST" = "Lewisham and Greenwich NHS Trust",
  "CORBY URGENT CARE CENTRE" = "Corby Urgent Care Centre",
  "NOTTINGHAM UNIVERSITY HOSPITALS NHS TRUST" = "Nottingham University Hospitals NHS Trust",
  "THE ROBERT JONES AND AGNES HUNT ORTHOPAEDIC HOSPITAL NHS FOUNDATION TRUST" = "The Robert Jones and Agnes Hunt Orthopaedic Hospital NHS Foundation Trust",
  "CHELSEA AND WESTMINSTER HOSPITAL NHS FOUNDATION TRUST" = "Chelsea and Westminster Hospital NHS Foundation Trust",
  "BARNSLEY HOSPITAL NHS FOUNDATION TRUST" = "Barnsley Hospital NHS Foundation Trust",
  "UNIVERSITY HOSPITALS COVENTRY AND WARWICKSHIRE NHS TRUST" = "University Hospitals Coventry and Warwickshire NHS Trust",
  "WEST SUFFOLK NHS FOUNDATION TRUST" = "West Suffolk NHS Foundation Trust",
  "TAMESIDE AND GLOSSOP INTEGRATED CARE NHS FOUNDATION TRUST" = "Tameside and Glossop Integrated Care NHS Foundation Trust",
  "ISLE OF WIGHT NHS TRUST" = "Isle of Wight NHS Trust",
  "THE HILLINGDON HOSPITALS NHS FOUNDATION TRUST" = "The Hillingdon Hospitals NHS Foundation Trust",
  "THE QUEEN ELIZABETH HOSPITAL, KING'S LYNN, NHS FOUNDATION TRUST" = "The Queen Elizabeth Hospital, King's Lynn, NHS Foundation Trust",
  "LINCOLNSHIRE COMMUNITY HEALTH SERVICES NHS TRUST" = "Lincolnshire Community Health Services NHS Trust",
  "EAST CHESHIRE NHS TRUST" = "East Cheshire NHS Trust",
  "MAIDSTONE AND TUNBRIDGE WELLS NHS TRUST" = "Maidstone and Tunbridge Wells NHS Trust",
  "MANCHESTER UNIVERSITY NHS FOUNDATION TRUST" = "Manchester University NHS Foundation Trust",
  "UNIVERSITY HOSPITALS BIRMINGHAM NHS FOUNDATION TRUST" = "University Hospitals Birmingham NHS Foundation Trust",
  "HULL UNIVERSITY TEACHING HOSPITALS NHS TRUST" = "Hull University Teaching Hospitals NHS Trust",
  "WRIGHTINGTON, WIGAN AND LEIGH NHS FOUNDATION TRUST" = "Wrightington, Wigan and Leigh NHS Foundation Trust",
  "HARROGATE AND DISTRICT NHS FOUNDATION TRUST" = "Harrogate and District NHS Foundation Trust",
  "DHU HEALTH CARE C.I.C" = "DHU Health Care C.I.C",
  "LEEDS TEACHING HOSPITALS NHS TRUST" = "Leeds Teaching Hospitals NHS Trust",
  "BECKENHAM BEACON UCC" = "Beckenham Beacon UCC",
  "UNITED LINCOLNSHIRE HOSPITALS NHS TRUST" = "United Lincolnshire Hospitals NHS Trust",
  "HAMPSHIRE HOSPITALS NHS FOUNDATION TRUST" = "Hampshire Hospitals NHS Foundation Trust",
  "NORTH TEES AND HARTLEPOOL NHS FOUNDATION TRUST" = "North Tees and Hartlepool NHS Foundation Trust",
  "UNIVERSITY HOSPITALS OF DERBY AND BURTON NHS FOUNDATION TRUST" = "University Hospitals of Derby and Burton NHS Foundation Trust",
  "UNIVERSITY HOSPITALS OF LEICESTER NHS TRUST" = "University Hospitals of Leicester NHS Trust",
  "GLOUCESTERSHIRE HEALTH AND CARE NHS FOUNDATION TRUST" = "Gloucestershire Health and Care NHS Foundation Trust",
  "MOORFIELDS EYE HOSPITAL NHS FOUNDATION TRUST" = "Moorfields Eye Hospital NHS Foundation Trust",
  "BEDFORDSHIRE HOSPITALS NHS FOUNDATION TRUST" = "Bedfordshire Hospitals NHS Foundation Trust",
  "THE NEWCASTLE UPON TYNE HOSPITALS NHS FOUNDATION TRUST" = "The Newcastle Upon Tyne Hospitals NHS Foundation Trust",
  "MERSEY CARE NHS FOUNDATION TRUST" = "Mersey Care NHS Foundation Trust",
  "MILTON KEYNES UNIVERSITY HOSPITAL NHS FOUNDATION TRUST" = "Milton Keynes University Hospital NHS Foundation Trust",
  "UNIVERSITY HOSPITALS PLYMOUTH NHS TRUST" = "University Hospitals Plymouth NHS Trust",
  "BRADFORD TEACHING HOSPITALS NHS FOUNDATION TRUST" = "Bradford Teaching Hospitals NHS Foundation Trust",
  "LIVERPOOL UNIVERSITY HOSPITALS NHS FOUNDATION TRUST" = "Liverpool University Hospitals NHS Foundation Trust",
  "ALDER HEY CHILDREN'S NHS FOUNDATION TRUST" = "Alder Hey Children's NHS Foundation Trust",
  "MARKET HARBOROUGH" = "Market Harborough",
  "ROSSENDALE MINOR INJURIES UNIT" = "Rossendale Minor Injuries Unit",
  "BARTS HEALTH NHS TRUST" = "Barts Health NHS Trust",
  "COUNTESS OF CHESTER HOSPITAL NHS FOUNDATION TRUST" = "Countess of Chester Hospital NHS Foundation Trust",
  "EAST AND NORTH HERTFORDSHIRE NHS TRUST" = "East and North Hertfordshire NHS Trust",
  "ROYAL NATIONAL ORTHOPAEDIC HOSPITAL NHS TRUST" = "Royal National Orthopaedic Hospital NHS Trust",
  "LIVERPOOL WOMEN'S NHS FOUNDATION TRUST" = "Liverpool Women's NHS Foundation Trust",
  "NORFOLK AND NORWICH UNIVERSITY HOSPITALS NHS FOUNDATION TRUST" = "Norfolk and Norwich University Hospitals NHS Foundation Trust",
  "LONDON NORTH WEST UNIVERSITY HEALTHCARE NHS TRUST" = "London North West University Healthcare NHS Trust",
  "HASLEMERE MINOR INJURIES UNIT" = "Haslemere Minor Injuries Unit",
  "THE CHRISTIE NHS FOUNDATION TRUST" = "The Christie NHS Foundation Trust",
  "NORTH WEST BOROUGHS HEALTHCARE NHS FOUNDATION TRUST" = "North West Boroughs Healthcare NHS Foundation Trust",
  "MELTON MOWBRAY" = "Melton Mowbray",
  "OADBY" = "Oadby",
  "UNIVERSITY HOSPITALS OF MORECAMBE BAY NHS FOUNDATION TRUST" = "University Hospitals of Morecambe Bay NHS Foundation Trust",
  "COUNTY DURHAM AND DARLINGTON NHS FOUNDATION TRUST" = "County Durham and Darlington NHS Foundation Trust",
  "EAST LANCASHIRE HOSPITALS NHS TRUST" = "East Lancashire Hospitals NHS Trust",
  "ROYAL SURREY COUNTY HOSPITAL NHS FOUNDATION TRUST" = "Royal Surrey County Hospital NHS Foundation Trust",
  "ROYAL BERKSHIRE NHS FOUNDATION TRUST" = "Royal Berkshire NHS Foundation Trust",
  "ROYAL DEVON AND EXETER NHS FOUNDATION TRUST" = "Royal Devon and Exeter NHS Foundation Trust",
  "GUY'S AND ST THOMAS' NHS FOUNDATION TRUST" = "Guy's and St Thomas' NHS Foundation Trust",
  "UNIVERSITY HOSPITALS DORSET NHS FOUNDATION TRUST" = "University Hospitals Dorset NHS Foundation Trust",
  "OAKHAM" = "Oakham",
  "NORTH MIDDLESEX UNIVERSITY HOSPITAL NHS TRUST" = "North Middlesex University Hospital NHS Trust",
  "ASHFORD AND ST PETER'S HOSPITALS NHS FOUNDATION TRUST" = "Ashford and St Peter's Hospitals NHS Foundation Trust",
  "OXFORD UNIVERSITY HOSPITALS NHS FOUNDATION TRUST" = "Oxford University Hospitals NHS Foundation Trust",
  "YORK TEACHING HOSPITAL NHS FOUNDATION TRUST" = "York Teaching Hospital NHS Foundation Trust",
  "MID AND SOUTH ESSEX NHS FOUNDATION TRUST" = "Mid and South Essex NHS Foundation Trust",
  "MID CHESHIRE HOSPITALS NHS FOUNDATION TRUST" = "Mid Cheshire Hospitals NHS Foundation Trust",
  "SUSSEX COMMUNITY NHS FOUNDATION TRUST" = "Sussex Community NHS Foundation Trust",
  "BRIGHTON AND SUSSEX UNIVERSITY HOSPITALS NHS TRUST" = "Brighton and Sussex University Hospitals NHS Trust",
  "LOUGHBOROUGH URGENT CARE CENTRE" = "Loughborough Urgent Care Centre",
  "ST HELENS AND KNOWSLEY TEACHING HOSPITALS NHS TRUST" = "St Helens and Knowsley Teaching Hospitals NHS Trust",
  "BOLTON NHS FOUNDATION TRUST" = "Bolton NHS Foundation Trust",
  "SOUTH TEES HOSPITALS NHS FOUNDATION TRUST" = "South Tees Hospitals NHS Foundation Trust",
  "IMPERIAL COLLEGE HEALTHCARE NHS TRUST" = "Imperial College Healthcare NHS Trust",
  "LLR EA - THE MERLYN VAZ HEALTH & SOCIAL CARE CENTRE" = "LLR EA - The Merlyn Vaz Health & Social Care Centre",
  "EAST SUFFOLK AND NORTH ESSEX NHS FOUNDATION TRUST" = "East Suffolk and North Essex NHS Foundation Trust",
  "BRIDGEWATER COMMUNITY HEALTHCARE NHS FOUNDATION TRUST" = "Bridgewater Community Healthcare NHS Foundation Trust",
  "CROYDON HEALTH SERVICES NHS TRUST" = "Croydon Health Services NHS Trust",
  "NORTH CUMBRIA INTEGRATED CARE NHS FOUNDATION TRUST" = "North Cumbria Integrated Care NHS Foundation Trust",
  "UNIVERSITY HOSPITALS BRISTOL AND WESTON NHS FOUNDATION TRUST" = "University Hospitals Bristol and Weston NHS Foundation Trust",
  "BRACKNELL URGENT CARE CENTRE WIC" = "Bracknell Urgent Care Centre WIC",
  "THE ROYAL WOLVERHAMPTON NHS TRUST" = "The Royal Wolverhampton NHS Trust",
  "KINGSTON HOSPITAL NHS FOUNDATION TRUST" = "Kingston Hospital NHS Foundation Trust",
  "SALFORD ROYAL NHS FOUNDATION TRUST" = "Salford Royal NHS Foundation Trust",
  "WESTERN SUSSEX HOSPITALS NHS FOUNDATION TRUST" = "Western Sussex Hospitals NHS Foundation Trust",
  "BERKSHIRE HEALTHCARE NHS FOUNDATION TRUST" = "Berkshire Healthcare NHS Foundation Trust",
  "DARTFORD AND GRAVESHAM NHS TRUST" = "Dartford and Gravesham NHS Trust",
  "EAST KENT HOSPITALS UNIVERSITY NHS FOUNDATION TRUST" = "East Kent Hospitals University NHS Foundation Trust",
  "WHITTINGTON HEALTH NHS TRUST" = "Whittington Health NHS Trust",
  "STOCKPORT NHS FOUNDATION TRUST" = "Stockport NHS Foundation Trust",
  "AIREDALE NHS FOUNDATION TRUST" = "Airedale NHS Foundation Trust",
  "TORBAY AND SOUTH DEVON NHS FOUNDATION TRUST" = "Torbay and South Devon NHS Foundation Trust",
  "SOMERSET NHS FOUNDATION TRUST" = "Somerset NHS Foundation Trust",
  "GLOUCESTERSHIRE HOSPITALS NHS FOUNDATION TRUST" = "Gloucestershire Hospitals NHS Foundation Trust",
  "EAST SUSSEX HEALTHCARE NHS TRUST" = "East Sussex Healthcare NHS Trust",
  "THE WALTON CENTRE NHS FOUNDATION TRUST" = "The Walton Centre NHS Foundation Trust",
  "COVENTRY AND WARWICKSHIRE PARTNERSHIP NHS TRUST" = "Coventry and Warwickshire Partnership NHS Trust",
  "KING'S COLLEGE HOSPITAL NHS FOUNDATION TRUST" = "King's College Hospital NHS Foundation Trust",
  "CORNWALL PARTNERSHIP NHS FOUNDATION TRUST" = "Cornwall Partnership NHS Foundation Trust",
  "JAMES PAGET UNIVERSITY HOSPITALS NHS FOUNDATION TRUST" = "James Paget University Hospitals NHS Foundation Trust",
  "BIRMINGHAM WIC" = "Birmingham WIC",
  "EAST RIDING COMMUNITY HOSPITAL" = "East Riding Community Hospital",
  "GOOLE & DISTRICT HOSPITAL" = "Goole & District Hospital",
  "THE WILBERFORCE HEALTH CENTRE" = "The Wilberforce Health Centre",
  "BRANSHOLME HEALTH CENTRE" = "Bransholme Health Centre",
  "THE SHREWSBURY AND TELFORD HOSPITAL NHS TRUST" = "The Shrewsbury and Telford Hospital NHS Trust",
  "FRIMLEY HEALTH NHS FOUNDATION TRUST" = "Frimley Health NHS Foundation Trust",
  "WARRINGTON AND HALTON TEACHING HOSPITALS NHS FOUNDATION TRUST" = "Warrington and Halton Teaching Hospitals NHS Foundation Trust",
  "PENNINE ACUTE HOSPITALS NHS TRUST" = "Pennine Acute Hospitals NHS Trust",
  "WIRRAL UNIVERSITY TEACHING HOSPITAL NHS FOUNDATION TRUST" = "Wirral University Teaching Hospital NHS Foundation Trust",
  "ROYAL FREE LONDON NHS FOUNDATION TRUST" = "Royal Free London NHS Foundation Trust",
  "PRACTICE PLUS GROUP SURGICAL CENTRE - ST MARYS PORTSMOUTH" = "Practice Plus Group Surgical Centre - St Marys Portsmouth",
  "SHEFFIELD TEACHING HOSPITALS NHS FOUNDATION TRUST" = "Sheffield Teaching Hospitals NHS Foundation Trust",
  "HERTS URGENT CARE (ASCOTS LANE)" = "Herts Urgent Care (Ascots Lane)",
  "GEORGE ELIOT HOSPITAL NHS TRUST" = "George Eliot Hospital NHS Trust",
  "LANCASHIRE TEACHING HOSPITALS NHS FOUNDATION TRUST" = "Lancashire Teaching Hospitals NHS Foundation Trust",
  "DORSET COUNTY HOSPITAL NHS FOUNDATION TRUST" = "Dorset County Hospital NHS Foundation Trust",
  "TOTAL" = "Total",
  "CLACTON HOSPITAL" = "Clacton Hospital",
  "FRYATT HOSPITAL" = "Fryatt Hospital",
  "HHCIC EAST WIC" = "HHCIC East WIC",
  "ROYAL BROMPTON & HAREFIELD NHS FOUNDATION TRUST" = "Royal Brompton & Harefield NHS Foundation Trust",
  "CALDER COMMUNITY PRACTICE" = "Calder Community Practice",
  "SOUTHAMPTON NHS TREATMENT CENTRE" = "Southampton NHS Treatment Centre",
  "ST MARY'S NHS TREATMENT CENTRE" = "St Mary's NHS Treatment Centre",
  "POOLE HOSPITAL NHS FOUNDATION TRUST" = "Poole Hospital NHS Foundation Trust",
  "THE ROYAL BOURNEMOUTH AND CHRISTCHURCH HOSPITALS NHS FOUNDATION TRUST" = "The Royal Bournemouth and Christchurch Hospitals NHS Foundation Trust",
  "SHREWSBURY AND TELFORD HOSPITAL NHS TRUST" = "Shrewsbury and Telford Hospital NHS Trust",
  "PARK COMMUNITY PRACTICE" = "Park Community Practice",
  "HAROLD WOOD WIC" = "Harold Wood WIC",
  "PORTSMOUTH HOSPITALS NHS TRUST" = "Portsmouth Hospitals NHS Trust",
  "KINGS PARK SURGERY" = "Kings Park Surgery",
  "YATE WEST GATE CENTRE" = "Yate West Gate Centre",
  "THE JUNCTION HC - UNREGISTERED PATIENTS" = "The Junction HC - Unregistered Patients",
  "NORWICH PRACTICES LTD" = "Norwich Practices Ltd",
  "SUMMERFIELD URGENT CARE CENTRE" = "Summerfield Urgent Care Centre",
  "YORK AND SCARBOROUGH TEACHING HOSPITALS NHS FOUNDATION TRUST" = "York and Scarborough Teaching Hospitals NHS Foundation Trust",
  "NORTHERN CARE ALLIANCE NHS FOUNDATION TRUST" = "Northern Care Alliance NHS Foundation Trust",
  "UNIVERSITY HOSPITALS SUSSEX NHS FOUNDATION TRUST" = "University Hospitals Sussex NHS Foundation Trust",
  "WEST HERTFORDSHIRE TEACHING HOSPITALS NHS TRUST" = "West Hertfordshire Teaching Hospitals NHS Trust",
  "STATION PLAZA HEALTH CENTRE" = "Station Plaza Health Centre",
  "ROYAL DEVON UNIVERSITY HEALTHCARE NHS FOUNDATION TRUST" = "Royal Devon University Healthcare NHS Foundation Trust",
  "ROSSENDALE MIU & OOH" = "Rossendale MIU & OOH",
  "SOUTH WARWICKSHIRE UNIVERSITY NHS FOUNDATION TRUST" = "South Warwickshire University NHS Foundation Trust",
  "HOMERTON HEALTHCARE NHS FOUNDATION TRUST" = "Homerton Healthcare NHS Foundation Trust",
  "MID YORKSHIRE TEACHING NHS TRUST" = "Mid Yorkshire Teaching NHS Trust",
  "PUTNOE WALK IN CENTRE" = "Putnoe Walk In Centre",
  "MERSEY AND WEST LANCASHIRE TEACHING HOSPITALS NHS TRUST" = "Mersey and West Lancashire Teaching Hospitals NHS Trust",
  "READING URGENT CARE CENTRE" = "Reading Urgent Care Centre",
  "BARKING HOSPITAL UTC" = "Barking Hospital UTC",
  "HAROLD WOOD POLYCLINIC UTC" = "Harold Wood Polyclinic UTC",
  "KINGSTON AND RICHMOND NHS FOUNDATION TRUST" = "Kingston and Richmond NHS Foundation Trust",
  "HAMPSHIRE AND ISLE OF WIGHT HEALTHCARE NHS FOUNDATION TRUST" = "Hampshire and Isle of Wight Healthcare NHS Foundation Trust",
  "BRIDLINGTON HOSPITAL" = "Bridlington Hospital",
  "NEMS UTC" = "NEMS UTC",
  "UNITED LINCOLNSHIRE TEACHING HOSPITALS NHS TRUST" = "United Lincolnshire Teaching Hospitals NHS Trust",
  "HERNE BAY HEALTH CARE LTD" = "Herne Bay Health Care Ltd",
  "HASTINGS GP LED HEALTH CENTRE" = "Hastings GP Led Health Centre",
  "ENDERBY LEISURE CENTRE" = "Enderby Leisure Centre",
  "LUTTERWORTH HOSPITAL" = "Lutterworth Hospital",
  "TROWBRIDGE MINOR INJURIES UNIT" = "Trowbridge Minor Injuries Unit",
  "CHIPPENHAM COMMUNITY HOSPITAL" = "Chippenham Community Hospital",
  "EAST AND NORTH HERTFORDSHIRE TEACHING NHS TRUST" = "East and North Hertfordshire Teaching NHS Trust",
  "WRIGHTINGTON, WIGAN AND LEIGH TEACHING HOSPITALS NHS FOUNDATION TRUST" = "Wrightington, Wigan and Leigh Teaching Hospitals NHS Foundation Trust",
  "FAVERSHAM MEDICAL PRACTICE MIU" = "Faversham Medical Practice MIU",
  "ROYAL SURREY NHS FOUNDATION TRUST" = "Royal Surrey NHS Foundation Trust",
  "PORTSMOUTH HOSPITALS UNIVERSITY NHS TRUST" = "Portsmouth Hospitals University NHS Trust",
  "NEMS CBS (UTC)" = "NEMS CBS (UTC)",
  "NORTH CHESHIRE AND MERSEY NHS FOUNDATION TRUST" = "North Cheshire and Mersey NHS Foundation Trust"
)

icb_recodes <- icb_recodes <- c(
  "HAMPSHIRE AND ISLE OF WIGHT ICB" = "Hampshire and Isle of Wight ICB",
  "KENT AND MEDWAY ICB" = "Kent and Medway ICB",
  "SURREY AND SUSSEX ICB" = "Surrey and Sussex ICB",
  "GLOUCESTERSHIRE ICB" = "Gloucestershire ICB",
  "CHESHIRE AND MERSEYSIDE ICB" = "Cheshire and Merseyside ICB",
  "BIRMINGHAM AND SOLIHULL ICB" = "Birmingham and Solihull ICB",
  "WEST YORKSHIRE ICB" = "West Yorkshire ICB",
  "NOTTINGHAM AND NOTTINGHAMSHIRE ICB" = "Nottingham and Nottinghamshire ICB",
  "CORNWALL AND THE ISLES OF SCILLY ICB" = "Cornwall and the Isles of Scilly ICB",
  "BLACK COUNTRY ICB" = "Black Country ICB",
  "CENTRAL EAST ICB" = "Central East ICB",
  "DORSET ICB" = "Dorset ICB",
  "BATH AND NORTH EAST SOMERSET, SWINDON AND WILTSHIRE ICB" = "Bath and North East Somerset, Swindon and Wiltshire ICB",
  "HEREFORDSHIRE AND WORCESTERSHIRE ICB" = "Herefordshire and Worcestershire ICB",
  "SOUTH WEST LONDON ICB" = "South West London ICB",
  "LANCASHIRE AND SOUTH CUMBRIA ICB" = "Lancashire and South Cumbria ICB",
  "DERBY AND DERBYSHIRE ICB" = "Derby and Derbyshire ICB",
  "THAMES VALLEY ICB" = "Thames Valley ICB",
  "NORTH EAST AND NORTH CUMBRIA ICB" = "North East and North Cumbria ICB",
  "SOUTH YORKSHIRE ICB" = "South Yorkshire ICB",
  "NORTH EAST LONDON ICB" = "North East London ICB",
  "SOMERSET ICB" = "Somerset ICB",
  "WEST AND NORTH LONDON ICB" = "West and North London ICB",
  "SHROPSHIRE, TELFORD AND WREKIN ICB" = "Shropshire, Telford and Wrekin ICB",
  "DEVON ICB" = "Devon ICB",
  "HUMBER AND NORTH YORKSHIRE ICB" = "Humber and North Yorkshire ICB",
  "NORTHAMPTONSHIRE ICB" = "Northamptonshire ICB",
  "STAFFORDSHIRE AND STOKE-ON-TRENT ICB" = "Staffordshire and Stoke-on-Trent ICB",
  "ESSEX ICB" = "Essex ICB",
  "BRISTOL, NORTH SOMERSET AND SOUTH GLOUCESTERSHIRE ICB" = "Bristol, North Somerset and South Gloucestershire ICB",
  "COVENTRY AND WARWICKSHIRE ICB" = "Coventry and Warwickshire ICB",
  "SOUTH EAST LONDON ICB" = "South East London ICB",
  "NORFOLK AND SUFFOLK ICB" = "Norfolk and Suffolk ICB",
  "GREATER MANCHESTER ICB" = "Greater Manchester ICB",
  "LINCOLNSHIRE ICB" = "Lincolnshire ICB",
  "LEICESTER, LEICESTERSHIRE AND RUTLAND ICB" = "Leicester, Leicestershire and Rutland ICB"
)

region_recodes <- c(
  "NHS ENGLAND NORTH WEST" = "NHS England North West",
  "NHS ENGLAND SOUTH EAST" = "NHS England South East",
  "NHS ENGLAND MIDLANDS" = "NHS England Midlands",
  "NHS ENGLAND SOUTH WEST" = "NHS England South West",
  "NHS ENGLAND NORTH EAST AND YORKSHIRE" = "NHS England North East and Yorkshire",
  "NHS ENGLAND EAST OF ENGLAND" = "NHS England East of England",
  "NHS ENGLAND LONDON" = "NHS England London",
  "TOTAL" = "Total"
)
ods_icb_extract <- function(ods_data) {

  rels <- ods_data$Organisation$Rels$Rel
  
  # Filter for Active relationships that match ICB link types
  # Trusts use RE5 (Geography), GPs use RE4 (Commissioning)
  icb_row <- rels[rels$Status == "Active" & rels$id %in% c("RE4", "RE5"), ]
  
  # If there are multiple, the one with Target.PrimaryRoleId.id == "RO261" 
  # is almost certainly the ICB
  icb_code <- icb_row$Target$OrgId$extension[icb_row$Target$PrimaryRole$id == "RO261"]
  
  return(icb_code[1]) # Return the first match
}



make_mort_funnel_plot <- function(df, 
                                 rate_breaks = c(1/400, 1/200, 1/100, 1/50), 
                                 line_breaks = c("95%" = 1.96, "99.7%" = 3, "1 in 1000" = 3.29),
                                 over_dispertion = 3,
                                 type_label = "") {
  
  # --- 1. Background Ribbons ---
  breaks <- sort(unique(c(0, rate_breaks, Inf)))
  ribbon_df <- data.frame(
    ymin = breaks[-length(breaks)],
    ymax = breaks[-1],
    fill = colorRampPalette(c("#2ecc71", "#f1c40f", "#e74c3c"))(length(breaks)-1)
  )

  # --- 2. Clean and Filter ---
  plot_df <- df %>%
    dplyr::mutate(
      excess_mort = as.numeric(excess_mort),
      tot_ae_adm = as.numeric(tot_ae_adm),
      org = as.character(org) 
    ) %>%
    dplyr::filter(!is.na(excess_mort), !is.na(tot_ae_adm), !is.na(org)) %>%
    dplyr::filter(tot_ae_adm > 0, excess_mort <= tot_ae_adm)
  
  if (nrow(plot_df) == 0) return(ggplot2::ggplot() + ggplot2::theme_void())

  # --- 3. Calculate Statistics ---
  mu <- sum(plot_df$excess_mort) / sum(plot_df$tot_ae_adm)
  current_rate <- plot_df$excess_mort / plot_df$tot_ae_adm
  z_scores <- (current_rate - mu) / sqrt(mu * (1 - mu) / plot_df$tot_ae_adm)

  # --- 4. Generate Dynamic Funnel Lines ---
  x_min <- min(plot_df$tot_ae_adm)
  x_max <- max(plot_df$tot_ae_adm)
  
  # Create a sequence for the lines
  line_data_base <- data.frame(tot_ae_adm = seq(x_min, x_max, length.out = 500)) %>%
    dplyr::mutate(
      logit_mu = log(mu / (1 - mu)),
      logit_se = sqrt(over_dispertion) * sqrt(1 / (tot_ae_adm * mu * (1 - mu)))
    )

  # Use map_df to create a "Long" format data frame of all requested lines
  line_breaks <- c("National average" = 0, line_breaks)
funnel_lines <- purrr::map_df(names(line_breaks), function(label) {
    z <- line_breaks[label]
    line_data_base %>%
      dplyr::mutate(
        upper = 1 / (1 + exp(-(logit_mu + z * logit_se))),
        # Only draw the lower line if it's a 'major' threshold (e.g., z > 2.5)
        lower = if(z > 2.5) 1 / (1 + exp(-(logit_mu - z * logit_se))) else NA,
        label = label
      )
})

  y_limit <- max(max(current_rate) * 1.2, 0.02)
  
  # --- 5. Plot ---
  ggplot2::ggplot(plot_df, ggplot2::aes(x = tot_ae_adm, y = excess_mort / tot_ae_adm)) +
    # Shading
    ggplot2::geom_rect(data = ribbon_df, inherit.aes = FALSE,
                       ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = fill),
                       alpha = 0.15) +
    ggplot2::scale_fill_identity() +
    # Dynamic Funnel Lines
    ggplot2::geom_line(data = funnel_lines, ggplot2::aes(y = upper, group = label), 
                       color = "black", alpha = 0.3) +
    ggplot2::geom_line(data = funnel_lines, ggplot2::aes(y = lower, group = label), 
                       color = "black", alpha = 0.3) +
    # Labels for lines (placed at the far right)
    ggplot2::geom_text(data = funnel_lines %>% dplyr::filter(tot_ae_adm == x_max),
                       ggplot2::aes(y = upper, label = label), 
                       hjust = 1.1, vjust = -0.5, size = 2.5, alpha = 0.6) +
    # Points
    ggplot2::geom_point(color = "steelblue", alpha = 0.7) +
    # Formatting
    ggplot2::scale_y_continuous(breaks = rate_breaks, labels = rate_labeller, expand = c(0, 0)) +
    ggplot2::coord_cartesian(ylim = c(0, y_limit)) +
    ggplot2::labs(
      title = "Risk-Adjusted Excess Mortality Funnel",
      subtitle = paste0("Avg Rate: ", rate_labeller(mu), " | Phi (Overdispersion): ", round(over_dispertion, 2)),
      x = "Total Type-1 A&E Admissions", 
      y = "Risk Rate (Expected Excess Deaths)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none", panel.grid.minor = ggplot2::element_blank())
}

rate_labeller <- function(x) {
  # Small epsilon check for zero
  ifelse(x < 1e-10, "0", paste0("1 in ", round(1 / x)))
}

per_k_labeller <- function(x) {
  # Small epsilon check for zero
  ifelse(x < 1e-10, "0", paste0(round(1000*x), " per mille\n", "(1 in ", round(1 / x), ")"))
}

# Helper to bin the data into "1 in X" categories
round_denom <- function(val, round = 25) {
  if (is.na(val) || val == 0) return("0")
  
  # Calculate denominator and round to nearest round
  denom <- 1 / val
  rounded_denom <- round(denom / round) * round
  
  return(rounded_denom)
}
