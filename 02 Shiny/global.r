require(readr)
require(lubridate)
require(dplyr)
require(data.world)

online0 = TRUE

if(online0) {
  globals = query(
    data.world(propsfile = "www/.data.world"),
      dataset="lordlemon/s-17-edv-final-project", type="sql",
      query="select StateName,PopulationBelowPovertyLVL, WhitePopulation,BlackPopulation,LatinoHispanic,WhitePopulationBelowPovertyLVL,BlackPopulationBelowPovertyLVL,LatinoHispanicBelowPovertyLVL from PovertyUSAStates"
       # %>% View()
  ) 
} 
#globals$Order_Date <- lubridate::year(globals$Order_Date)