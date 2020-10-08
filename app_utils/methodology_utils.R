pacman::p_load(htmlTable, shiny)

#' Function to add a nice HTML'd data source table on the methodology tab
#'
#' @return: HTML for the data source table on the methodology tab
data_source_tbl_html <- function() {
  ds_tbl <- data.frame(
    Statistic = c(
      "Maternal Deaths", "Live Births", "Woman-Years Lived", "Delivery Hospitals",
      "Percent Uninsured", "Percent Covered by Medicaid", "% Difference in NTSV Cesarean Rate from NJ Statewide Goal",
      "Practitioner Midwives Per Capita", "OB/GYN Physicians Per Capita", "Percent Poverty"
    ),
    Definition = c(
      'Death of a woman while pregnant or within...[365]...days of termination 
      of pregnancy, irrespective of the duration and site of the pregnancy, from 
      any cause related to or aggravated by the pregnancy or its management but not 
      from accidental or incidental causes" (WHO Maternal Mortality Site).',
      '"A live birth is defined as the complete expulsion or extraction from its mother 
      of a product of conception, irrespective of the duration of pregnancy, which, after 
      such separation, breathes or shows any evidence of life, such as beating of the heart, 
      pulsation of the umbilical cord, or definite movement of voluntary muscles." 
      (NJ SHAD Birth Data Technical Notes).',
      "The number of years lived by a woman within the time period measured and risk of 
      dying from maternal mortality.",
      "The geographic location of all delivery hospitals in the state of NJ in 2016.",
      "The total number of individuals without any insurance (public or private) divided by 
      the total number of individuals in that county.",
      "The total number of individuals covered by Medicaid divided by 
      the total number of individuals in that county.",
      '"The low-risk c-section rate is the count of infants delivered by c-section divided
      by the count of all live nulliparous (first birth), full term (37 completed weeks or
      more, based on the obstetric estimate), singleton (one fetus), vertex (head first)
      births" (NJ SHAD Data Notes) multiplied by 100 to display the percentage of NTSV cesarean
      sections.',
      "The total number of practitioner midwives in that county as determined by the 
      National Provider Identifier (NPI) from the Centers for Medicaid and Medicare (CMS)
      divided by the population of women ages 15+ multiplied by 100K to display the
      number of midwives per 100K people.",
      "The total number of OB/GYN Physicians practicing patient care in that county as
      determined by the American Medical Association (AMA) Master File divided by the
      population of women ages 15+ multiplied by 100K to display the number of
      OB/GYN physicians per 100K people.",
      "Following the Office of Management and Budget's (OMB) Statistical Policy Directive 14, 
      the Census Bureau uses a set of money income thresholds that vary by family size and 
      composition to determine who is in poverty. If a family's total income is less than the 
      family's threshold, then that family and every individual in it is considered in poverty.
      Details are available <a href='https://www.census.gov/topics/income-poverty/poverty/guidance/poverty-measures.html'>
      here</a>."
    ),
    `Data Source` = c(
      "<a href='https://www-doh.state.nj.us/doh-shad/query/selection/mort/MortSelection.html'>NJ State Health Assessment Data (SHAD) Mortality Data</a>",
      "<a href='https://www-doh.state.nj.us/doh-shad/query/selection/birth/BirthSelection.html'>NJ SHAD Birth Data</a>",
      "<a href='https://www.census.gov/programs-surveys/acs/data.html'>American Community Survey (ACS)</a>",
      "<a href='https://njogis-newjersey.opendata.arcgis.com/datasets/f15a09b7df4e408ea31ea2623dfd4e0b_8/data'>Geographic Location Shape Files</a> /
                      <a href='https://nj.gov/health/maternal/morbidity/mhh_reportcard/all_hospitals.shtml'>NJ Delivery Hospitals</a>", "...", "...",
      "<a href='https://www-doh.state.nj.us/doh-shad/query/builder/birth/MODLowRiskCesarean/MOD.html'>NJ SHAD Birth Data NTSV Cesarean Delivery Rate</a> /
                      <a href='https://www.healthypeople.gov/2020/topics-objectives/objective/mich-71'>NJ NTSV Goal</a>",
      "<a href='https://data.hrsa.gov/topics/health-workforce/ahrf'>Area Health Resource File (AHRF)/CMS NPI</a>",
      "<a href='https://data.hrsa.gov/topics/health-workforce/ahrf'>AHRF/AMA Physician Master File</a>",
      "<a href='https://data.hrsa.gov/topics/health-workforce/ahrf'>AHRF</a> / 
                      <a href='https://www.census.gov/programs-surveys/saipe/data.html'>Census Small Area Income and Poverty Estimates (SAIPE)</a>"
    ),
    Rationale = c(
      "NJ SHAD was chosen as the data source for maternal deaths because it provided 
                   the most detailed information available compared to the alternatives (e.g. CDC Wonder).",
      "NJ SHAD was chosen as the data source for live births because it provided 
                   the most detailed information available for the population of interest.",
      "ACS was chosen as the data source for woman-years lived because it provided 
                   the most detailed information available for the population of interest.",
      "The New Jersey Maternal Data Center was chosen as the data source for delivery hospitals
                  because it provided the most detailed information available for the population of interest.",
      "ACS was chosen as the data source for percent uninsured because it provided 
                   the most detailed information available for the population of interest.",
      "ACS was chosen as the data source for percent covered by medicaid because it provided 
                   the most detailed information available for the population of interest.",
      "NJ SHAD was chosen as the data source for NTSV cesarean section rates for consistency, as birth
                   data was originally pulled from this data source.",
      "AHRF was chosen as the data source for practitioner midwives because it provided 
                   the most detailed information available for the population of interest.",
      "AHRF was chosen as the data source for OB/GYN physicians because it provided 
                   the most detailed information available for the population of interest.",
      "This variable was readily available in the AHRF file and represented an important 
                   social determinant of health."
    ),
    Notes = c(
      "Maternal deaths were obtained by querying all deaths with the following NCHS Causes of Death: 
      Other complications of pregnancy, childbirth and the puerperium (ICD10 O10-O99) and 
      Pregnancy with abortive outcome (ICD10 O00-O08). While maternal deaths are usually 
      defined as those occurring within 42 days of termination of the pregnancy and late maternal 
      deaths are those occurring within 365 days, we use the term maternal deaths to refer to both 
      as the NJ SHAD groups them together under these causes of death.",
      "",
      "Woman-years lived was obtained using ACS Summary Table B01001 from the 1-Year Estimates.",
      "We cross referenced the hospital shape file with the delivery hospitals listed at nj.gov to obtain a
      subset of hospitals that perform deliveries. Delivery hospitals in 2016 are shown regardless of the selected
      year as 2016 was the most recent data we had access to. Additionally, the location and amount of delivery
      hospitals were not expected to change much throughout the years.",
      "Percent uninsured was obtained using ACS Summary Table B27001 from the 1-Year Estimates. Ages 6-54 were
      chosen because they were most inclusive age range available for this Census variable that incorporated our
      target demographic.",
      "Percent covered by Medicaid was obtained using ACS Summary Table B27007 from the 1-Year Estimates. Ages
      6-54 were chosen because they were most inclusive age range available for this Census variable that
      incorporated our target demographic.",
      "Difference in NTSV Cesarean Rate from NJ Statewide Goal was obtained by subtracting the most current
      NTSV State Goal of 24.7% (from 2020) from the county's NTSV Cesarean Rate. The 2020 NTSV State goal of
      24.7% was used throughout all years.",
      "Practitioner Midwives was obtained using the 2018-2019 AHRF County File.",
      "OB/GYN Physicians was obtained using the 2018-2019 AHRF County File. As this data only exists
      for 2010, 2015, and 2017, the most current data will be displayed without looking ahead. I.e. 
      2010 data is displayed for years 2010-2014, 2015 data is displayed for 2015-2016, and 2017 for 2017.",
      "Percent Poverty was obtained using the 2018-2019 AHRF County File."
    )
  )

  names(ds_tbl)[3] <- "Data Source"

  htmlTable(ds_tbl,
    align = paste(rep("l", ncol(ds_tbl)), collapse = ""),
    rnames = rep("", nrow(ds_tbl)),
    css.cell = "padding: 10px",
    useViewer = T
  ) %>%
    HTML()
}
