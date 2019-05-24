# Validate GeneNetwork species name
check_species <- function(species) {
  match.arg(tolower(species),
    c(
      "mouse",
      "rat",
      "arabidopsis",
      "human",
      "barley",
      "drosophila",
      "macaque monkey",
      "soybean",
      "tomato",
      "poplar"
    )
  )
}
