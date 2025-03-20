generate_election_data <- function(){
	items_region <- c("Région : AGNEBY-TIASSA", "Région : BAFING", "Région : BAGOUE", "Région : BELIER", "Région : BERE", "Région : BOUNKANI", "Région : CAVALLY", "Région : FOLON", "Région : GBEKE", "Région : GBOKLE")
	REGION <- sample(items_region, 1)
	
	items_departement <- c("Département : Agboville", "Département : Sikensi", "Département : Taabo", "Département : Tiassalé", "Département : Koro", "Département : Ouaninou", "Département : Touba", "Département : Boundiali", "Département : Kouto", "Département : Tingrela")
	DEPARTEMENT <- sample(items_departement, 1)
	
	items_commune <- c("Sous-préfecture ou Commune : Aboude", "Sous-préfecture ou Commune  : Ananguie", "Sous-préfecture ou Commune  : Attobrou", "Sous-préfecture ou Commune  : Azaguie", "Sous-préfecture ou Commune  : Gomon", "Sous-préfecture ou Commune  : Grand-Morie", "Sous-préfecture ou Commune  : Booko", "Sous-préfecture ou Commune  : Borotou", "Sous-préfecture ou Commune  : Saboudougou", "Sous-préfecture ou Commune  : Guinteguela")
	COMMUNE <- sample(items_commune, 1)
	
	items_lieu_de_vote <- c("Lieu de vote : EPP VACERISSO", "Lieu de vote : EPP GBETEMA", "Lieu de vote : EPP BONZO", "Lieu de vote : EPP KAAKO", "Lieu de vote : EPP FENAN", "Lieu de vote : EPP TOUBAKO-KAMASSELA", "Lieu de vote : PLACE PUBLIQUE GATASSO", "Lieu de vote : EPP DIOGO", "Lieu de vote : LYCEE MARIATOU KONE", "Lieu de vote : COLLEGE MODERNE DE BOUNDIALI")
	LIEU_DE_VOTE <- sample(items_lieu_de_vote, 1)
	
	items_bureau_de_vote <- c("Bureau de vote n° : 1", "Bureau de vote n° : 2", "Bureau de vote n° : 3", "Bureau de vote n° : 4", "Bureau de vote n° : 5", "Bureau de vote n° : 6", "Bureau de vote n° : 7", "Bureau de vote n° : 8", "Bureau de vote n° : 9", "Bureau de vote n° : 10")
	BUREAU_DE_VOTE <- sample(items_bureau_de_vote, 1)
	
	items_nb_electeurs_inscrits <- c("113","256","342","489","530","602","770","822","960","1006")
	NB_ELECTEURS_INSCRITS <- sample(items_nb_electeurs_inscrits, 1)
	
	NB_VOTANTS_df_line_8 <- as.integer(NB_ELECTEURS_INSCRITS)-25
	
	df_line_6 <- c("Résultats du comptage En chiffres En lettres")
	df_line_7 <- paste("A = Nombre d'électeurs inscrits", NB_ELECTEURS_INSCRITS, from_numbers_to_letters(NB_ELECTEURS_INSCRITS))
	df_line_8 <- paste("B = Nombre de votants", NB_VOTANTS_df_line_8)
	df_line_9 <- c("C = Nombre de suffrages nuls 04 quatre")
	df_line_10 <- c("D = B C = Nombre de suffrages exprimés ")
	
	df_line_11 <- c("Candidats à l'élection du Président Nombre de suffrages etf activement obtenus en : Représentants des candidats Emargement")
	df_line_12 <- c("de la République chiffres lettres pourcentage (préciser M., Mlle ou Mme)")
	
	df_line_13 <- c("AKOTO YAO Kouadio Félix 00 bero 00%")
	df_line_14 <- c("ANAKY Kobena Innocent Augustin 00 bero 00%")
	df_line_15 <- paste("BEDIE Konan Aime Henri ", round((75.80*NB_VOTANTS_df_line_8)/100), "75,80%")
	df_line_16 <- c("DOLO Adama 00 zero 0%")
	df_line_17 <- c("ENOH Aka N'Douba 00 zero 00%")
	df_line_18 <- paste("GBAGBO Laurent ", round((21.60*NB_VOTANTS_df_line_8)/100), "21,60%")
	df_line_19 <- c("GNAMIEN Konan 00 zero 00%")
	df_line_20 <- c("KONAN Kouadio Simeon 00 zero 00%")
	df_line_21 <- c("LOHOUES Anne Jacqueline epouse OBLE 00 zero 00%")
	df_line_22 <- c("MABRI Toikeuse Albert 00 zero on 00% NANOU BROU JUSTIN Niphosty")
	df_line_23 <- paste("OUATTARA Alassane ", round((3.20*NB_VOTANTS_df_line_8)/100), "03,20%", "ADEHE KOUTOVA MARC Ho")
	df_line_24 <- c("TAGOUA Nynsemon Pascal 00 zero 00%")
	df_line_25 <- c("TOHOU Henri 00 zero 00%")
	df_line_26 <- c("WODIE Romain Francis 00 zero 00% ESSOA EHILE Brown")
	
	df_line_27 <- c("Président(e) 1er Secrétaire 2e Secrétaire")
	df_line_28 <- c("BOUSSOU KOCRA Bernabe SOUGALO SYLLA AMON ASSI HENR 21-THERRY")
	
	
	df <- rbind(REGION, DEPARTEMENT, COMMUNE, LIEU_DE_VOTE, BUREAU_DE_VOTE, df_line_6, df_line_7, df_line_8, df_line_9, df_line_10, df_line_11, df_line_12, df_line_13, df_line_14, df_line_15,df_line_16,
	df_line_17, df_line_18, df_line_19, df_line_20, df_line_21, df_line_22, df_line_23, df_line_24, df_line_25, df_line_26, df_line_27, df_line_28)
	
	LV <- delete_white_spaces(substring_after_two_point(LIEU_DE_VOTE))
	DPT <- delete_white_spaces(substring_after_two_point(DEPARTEMENT))
	RGN <- delete_white_spaces(substring_after_two_point(REGION))
	HOUR <- paste(format(Sys.Date(), "%Y_%m_%d"),format(Sys.time(), "%H_%M_%S"),sep = "_")
	
	output_file <- tolower(paste(LV,DPT,RGN,HOUR,sep = "_"))
	output_filename <- paste(output_file,"csv",sep = ".")
	
	output_path <- paste("C:/Users/gs8894/OneDrive - Cooperactions/Documents/Election_project/input_file",output_filename,sep = "/")
	
	save_df <- write.csv2(df, file=output_path, row.names = FALSE, fileEncoding = "UTF-8")
	print(paste0("Sending polling station data to the server : ", output_file))
}

run_function_n_times <- function(n){
	for (i in 1:n) {
		Sys.sleep(1)  # Pause for 1 second in each iteration
		generate_election_data()
	}
}

run_function_n_times(1)
