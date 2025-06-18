## Rob Glaubius
## Avenir Health
## 2021-02-08
## aim-regtest-const : global constants used in aim-regtest

## var.levels provides factor variable levels according to a consistent
## ordering. These must be updated as we extract more indicators or indicator
## strata
var.levels = list(
  area = c("Global", "Regional", "National", "PJNZ"),
  ind  = c("Population", "Births", "Deaths", "HIV population", "New HIV infections", "AIDS deaths", "ART population", "ART coverage", "PMTCT need", "PMTCT population", "PMTCT coverage", "MTCT rate"),
  age  = c("All", "0-14", "15-49", "50+"),
  sex  = c("Both", "Male", "Female"))
