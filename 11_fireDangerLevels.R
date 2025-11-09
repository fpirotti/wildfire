library(readxl)
library(writexl)
library(dplyr)
library(jsonlite)
library(cffdrs)

classes <- c("very low", "low", "Medium", "High", "Very High")
thresholds <- list(
  alp = list(
    fwi = c(3, 7, 11, 16),
    ffmc = c(70, 84, 87, 89)
  ),
  med = list(
    fwi = c(3, 8, 14, 22),
    ffmc = c(70, 84, 88, 90)
  )
)

data <- readxl::read_excel("data/FVGfireDangerLevels/outfwi.xlsx")
data$tipo <- ifelse(grepl("alpi*", data$nome,ignore.case = T), "alp", "med")
data$level.fwi <- factor(apply(data, 1,  function(x){
  classes[[ (sum(thresholds[[ x[["tipo"]] ]][["fwi"]] < as.numeric(x[["FWI"]]) ) +1)]]
}), levels = classes)

# plot(table(data$level.fwi, data$MON, data$nome))



si_at_it <- data |> filter(nome=="Alpi Giulie")
si_it <- data |> filter(nome=="Carso")

writexl::write_xlsx(list("TOT"=data, "ItaSlovBorder_Carso"= si_it,
                    "ItaAustriaSlovBorder_AlpGiulie"= si_at_it) ,
                    "fireDangerITA.xlsx"
)




