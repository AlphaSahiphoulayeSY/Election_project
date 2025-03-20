from_numbers_to_letters <- function(val){
	if (val == "113") {return("Cent-treize")}
	else if (val == "256") {return("Deux cent cinquante-six")}
	else if (val == "342") {return("Trois cent quarante-deux")}
	else if (val == "489") {return("Quatre cent quatre-vingt-neuf")}
	else if (val == "530") {return("Cinq cent trente")}
	else if (val == "602") {return("Six-cent-deux")}
	else if (val == "770") {return("Sept cent soixante-dix")}
	else if (val == "822") {return("Huit cent vingt-deux")}
	else if (val == "960") {return("Neuf cent soixante")}
	else if (val == "1006") {return("Mille six")}
}

substring_after_two_point <- function(s){
	result <- sub(".*:", "", s)
	return (result)
}

substring_after_region <- function(s){
	result <- sub("Région...", "", s)
	return (result)
}

delete_white_spaces <- function(x){
	result <- gsub("[[:space:]]", "", x)
	return (result)
}

extract_numbers <- function(texte){
	# Extract numbers using regular expressions
	numbers <- gregexpr("[0-9]+", texte)
	result <- regmatches(texte, numbers)
	# Convert to numeric
	numeric_result <- as.numeric(unlist(result))
	return (numeric_result)
}
