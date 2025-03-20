process_election_results <- function(csv_file){
	data <- read.csv(csv_file, skip=1)
	
	# Values for final dataframe
	# Convert all characters to uppercase
	REGION <- toupper(substring_after_region(colnames(data)))
	DEPARTEMENT <- toupper(delete_white_spaces(substring_after_two_point(data[1,])))
	SOUS_PREFECTURE <- toupper(delete_white_spaces(substring_after_two_point(data[2,])))
	LIEU_DE_VOTE <- toupper(delete_white_spaces(substring_after_two_point(data[3,])))
	BUREAU_DE_VOTE <- toupper(delete_white_spaces(substring_after_two_point(data[4,])))

	
	# Values for final dataframe
	NB_ELECTEURS_INSCRITS <- extract_numbers(data[6,])
	NB_VOTANTS <- extract_numbers(data[7,])
	NB_SUFFRAGES_NULS <- extract_numbers(data[8,])
	NB_SUFFRAGES_EXPRIMES <- NB_VOTANTS - NB_SUFFRAGES_NULS
	
	# Values for final dataframe
	AKOTO_YAO_KOUADIO_FELIX <- extract_numbers(data[12,])[1]
	ANAKY_KOBENA_INNOCENT_AUGUSTIN <- extract_numbers(data[13,])[1]
	BEDIE_KONAN_AIME_HENRI <- extract_numbers(data[14,])[1]
	DOLO_ADAMA <- extract_numbers(data[15,])[1]
    ENOH_AKA_NDOUBA <- extract_numbers(data[16,])[1]
    GBAGBO_LAURENT <- extract_numbers(data[17,])[1]
    GNAMIEN_KONAN <- extract_numbers(data[18,])[1]
    KONAN_KOUADIO_SIMEON <- extract_numbers(data[19,])[1]
    LOHOUES_ANNE_JACQUELINE_EPOUSE_OBLE <- extract_numbers(data[20,])[1]
    MABRI_TOIKEUSE_ALBERT <- extract_numbers(data[21,])[1]
    OUATTARA_ALASSANE <- extract_numbers(data[22,])[1]
    TAGOUA_NYNSEMON_PASCAL <- extract_numbers(data[23,])[1]
    TOHOU_HENRI <- extract_numbers(data[24,])[1]
    WODIE_ROMAIN_FRANCIS <- extract_numbers(data[25,])[1]
	
	df_res <- data.frame(REGION, DEPARTEMENT, SOUS_PREFECTURE, LIEU_DE_VOTE, BUREAU_DE_VOTE, NB_ELECTEURS_INSCRITS, NB_VOTANTS, NB_SUFFRAGES_NULS, NB_SUFFRAGES_EXPRIMES, AKOTO_YAO_KOUADIO_FELIX, ANAKY_KOBENA_INNOCENT_AUGUSTIN,
	BEDIE_KONAN_AIME_HENRI, DOLO_ADAMA, ENOH_AKA_NDOUBA, GBAGBO_LAURENT, GNAMIEN_KONAN, KONAN_KOUADIO_SIMEON, LOHOUES_ANNE_JACQUELINE_EPOUSE_OBLE, MABRI_TOIKEUSE_ALBERT, OUATTARA_ALASSANE, TAGOUA_NYNSEMON_PASCAL,
	TOHOU_HENRI, WODIE_ROMAIN_FRANCIS)
	
	# Rename dataframe column names
	colnames(df_res)[colnames(df_res) == 'SOUS_PREFECTURE'] <- 'SOUS-PREFECTURE'
	colnames(df_res)[colnames(df_res) == 'LIEU_DE_VOTE'] <- 'LIEU DE VOTE'
	colnames(df_res)[colnames(df_res) == 'BUREAU_DE_VOTE'] <- 'BUREAU DE VOTE'
	colnames(df_res)[colnames(df_res) == 'NB_ELECTEURS_INSCRITS'] <- 'INSCRITS'
	colnames(df_res)[colnames(df_res) == 'NB_VOTANTS'] <- 'VOTANTS'
	colnames(df_res)[colnames(df_res) == 'NB_SUFFRAGES_NULS'] <- 'BULLETINS NULS'
	colnames(df_res)[colnames(df_res) == 'NB_SUFFRAGES_EXPRIMES'] <- 'SUFFRAGES EXPRIMES'
	
	colnames(df_res)[colnames(df_res) == 'AKOTO_YAO_KOUADIO_FELIX'] <- 'AKOTO YAO'
	colnames(df_res)[colnames(df_res) == 'ANAKY_KOBENA_INNOCENT_AUGUSTIN'] <- 'ANAKY KOBENA'
	colnames(df_res)[colnames(df_res) == 'BEDIE_KONAN_AIME_HENRI'] <- 'BEDIE KONAN'
	colnames(df_res)[colnames(df_res) == 'DOLO_ADAMA'] <- 'DOLO ADAMA'
	colnames(df_res)[colnames(df_res) == 'ENOH_AKA_NDOUBA'] <- 'ENOH AKA'
	colnames(df_res)[colnames(df_res) == 'GBAGBO_LAURENT'] <- 'GBAGBO LAURENT'
	colnames(df_res)[colnames(df_res) == 'GNAMIEN_KONAN'] <- 'GNAMIEN KONAN'
	colnames(df_res)[colnames(df_res) == 'KONAN_KOUADIO_SIMEON'] <- 'KONAN KOUADIO'
	colnames(df_res)[colnames(df_res) == 'LOHOUES_ANNE_JACQUELINE_EPOUSE_OBLE'] <- 'LOHOUES ANNE'
	colnames(df_res)[colnames(df_res) == 'MABRI_TOIKEUSE_ALBERT'] <- 'MABRI TOIKEUSE'
	colnames(df_res)[colnames(df_res) == 'OUATTARA_ALASSANE'] <- 'OUATTARA ALASSANE'
	colnames(df_res)[colnames(df_res) == 'TAGOUA_NYNSEMON_PASCAL'] <- 'TAGOUA PASCAL'
	colnames(df_res)[colnames(df_res) == 'TOHOU_HENRI'] <- 'TOHOU HENRI'
	colnames(df_res)[colnames(df_res) == 'WODIE_ROMAIN_FRANCIS'] <- 'WODIE FRANCIS'
	
	return (df_res)
}

# Start of election data processing
print(paste0("Start of election data processing : ", Sys.time()))

# List all csv files in the folder
input_path <- "C:/Users/gs8894/OneDrive - Cooperactions/Documents/Election_project/input_file/"
all_csv_files <- list.files(path = input_path)

frame <- do.call(rbind, lapply(all_csv_files, function(x) process_election_results(paste(input_path,x,sep = "/"))))
nb_lines_frame <- dim(frame)[1]

output_filename <- "Compilation_resultats_elections.csv"
output_path <- "C:/Users/gs8894/OneDrive - Cooperactions/Documents/Election_project/output_file"
concat_path <- paste(output_path,output_filename,sep = "/")

# Save results in the final csv file
save_df <- write.csv2(frame, file=concat_path, row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")

# End of election data processing
print(paste0("End of election data processing : ", Sys.time()))
print(paste0("Results compilation : ", nb_lines_frame, " polling stations"))
