#' @title Lancement du modèle hydraulique Mascaret
#' @description ENG : Run hydraulic model Mascaret                                                     
#' 
#' FR : Cette fonction appartient au script 03-PrepHydrauMascaret.R
#' 
#' - Définition des paramètres
#' - Création et écriture des fichiers d'entrée du modèle Mascaret 
#' - Création et écriture du fichier .cas : copie des profils en travers, débits en amont, condition aval, pas d'espace DX...
#' - Estimation des débits à chaque profil en travers, calculs hydrauliques.
#' 
#' Bief_STA : stocke les caractéristiques de tous les profils en travers
#'  
#' Cette fonction stocke les résultats dans la variable Bief_results. 
#' 
#' @param rep_res la fonction s'applique sur le dossier de résultats / results directory
#' @param name_run la fonction s'applique sur la variable nom qui comprend le débit + le bief + le BV étudié / run name
#' @param Bief_MNT_Iter la fonction s'applique sur le tableau de données avec l'altitude pour chaque profil en travers 
#' / dataframe with elevation data for each section
#' @param Bief_STA la fonction s'applique sur le tableau de données avec les caractéristiques de chaque profil en travers 
#' / dataframe with info for each section
#' @return la fonction retourne un tableau de données avec les résultats hydrauliques à chaque profil en travers 
#' / dataframe with results for each section

run_hydrau_mascaret = function(rep_res, name_run, Bief_MNT_Iter, Bief_STA)
  # LPo : préparation des fichiers pour les calculs flutor
{

  ## some parameters and constant values for flutor                   
  # LPo : paramètres (strickler, condition aval et DX)
  Strickler = as.vector(mascaret_strickler[i_coeff,])                                                      
  
  
  # prendre derniere ligne charge resultats flutor ou hecras
  CAVAL = liste_caval_all_bv %>% filter (NOM == paste0(num_bv, "_", num_reach))
  DX = mascaret_dx
  # LPo : voir CARTINO_PARAM.R
  
  ## create some input files .geo .hyd                                               
  # LPo : création des fichiers d'entrée
  
  dir_prj = file.path(rep_resu, rep_res); dir.create(dir_prj, recursive=T, showWarnings=F)        
  # LPo : rep_resu = "01-RUN/01-CEREMA/02-OUT/garonette3/Bv01" et ".../BV02" s'il y en a plusieurs etc
  
  name = name_run
  
  # LPo : Création des fichiers d'entrée
  
  geom_name = file.path(dir_prj, paste0(name, ".geo"))# LPo : création du fichier .geo
  # hyd_name  = file.path(dir_prj, paste0(name, ".hyd"))    # LPo : création du fichier .hyd
  file.create(geom_name)
  #file.create(c(geom_name, hyd_name)
  
  
  ## write first line
  #write("DEBORD", file=hyd_name)                                                     
  # LPo : write permet la création et l'écriture d'une colonne "DEBORD" (débordement)
  
  ## get the distance of each cross sections along the stream
  dist = Bief_STA$DIST                                                               
  # LPo : Bief_STA$DIST stocke la distance cumulée à chaque XS
  
  ## loop over the cross sections                                                    
  # LPo : boucle pour chaque XS
  
  Nprofils = nrow(Bief_STA)                                                                
  # LPo : nombre de profils en travers = nrow(=nombre de lignes) du tableau Bief_STA
  
  for(j in 1:Nprofils)
  {
    ## write the distance and the elevation data    
    # LPo : écriture dans une matrice ou un tableau de données des distances cumulées + altitudes pour chaque XS
    
    write(paste("PROFIL Bief_1", j, dist[j]), 
          file=geom_name, append=T)
    Profil_MNT = Bief_MNT_Iter %>% filter(NSection==j)
    write(paste(Profil_MNT$POSI, round(Profil_MNT$Z,digits=2), 'B'),                       
          file=geom_name, append=T)
    # LPo : digits = 2 : nombre de décimales
    
    ## write section number, strickler minor, strickler major, head loss                   
    # LPo : écriture dans une matrice ou un tableau de données
    
    #write(paste(j, Strickler[1], Strickler[2], 0), file=hyd_name, append=T)                
    # LPo : append = true signifie les données sont annexées
  }
  
  
  name_cas_mascaret = paste0(name_riv, name, "_mascaret")
  
  
  
  
  
  NApport = length(Bief_STA$Q)
  Apport = as.matrix(ncol=1, nrows=NApport, Bief_STA$Q - Bief_STA$Q[1])
  Apport = cbind(Apport, 0*Apport, 0*Apport)
  for (i in 2:NApport)
  {
    if (Apport[i,1] - Apport[i,2] > 0)
    {
      Apport[i,2] = max(Apport[1:i, 1] - sum(Apport[1:i-1, 2]))
    }
    Apport[i,3]=i
  }
  nb = which(Apport[,2]!=0)
  #nb_loi = length(nb)
  
  Apports_non_nuls = cbind("PROFIL" = Apport[nb,3], "DEBIT" = Apport[nb,2])
  
  ## write discharge inflows                                                         
  # LPo : écriture dans le fichier .cas des débits
  
  #write(length(nb), 
  #file=nomcas, append=T)
  # write(paste(Apport[nb,3], round(Apport[nb,2], digits=2)),
  #       file=nomcas, append=T)
  
  
  # Création des fichiers .loi 
  
  
  # Loi 0
  loi_0_name = file.path(dir_prj, paste0(name_riv, "_", num_bv, "_", num_reach, "_0.loi"))
  file.create(loi_0_name)
  
  ligne_1_fichier_loi_0 = paste0(num_bv, "_", num_reach, "_L1_QAMONT")
  
  write(ligne_1_fichier_loi_0, loi_0_name)
  write("Temps (s) Debit", loi_0_name)
  write("         S", loi_0_name)
  write(paste0(" 0.0 ,", Bief_STA$Q[1]), loi_0_name)
  write(paste0(" 10.0 ,", Bief_STA$Q[1]), loi_0_name)
  
  
  # Loi 1
  loi_1_name = file.path(dir_prj, paste0(name_riv, "_", num_bv, "_", num_reach, "_1.loi"))
  file.create(loi_1_name)
  
  i_loi = as.numeric(i_loi)
  
  ligne_1_fichier_loi_1 = paste0(num_bv, "_", num_reach, "_L1_CAVAL")
  write(ligne_1_fichier_loi_1, loi_1_name)
  write("Temps (s) Cote", loi_1_name)
  write("         S", loi_1_name)
  write(paste0(" 0.0 ,", CAVAL), loi_1_name)
  write(paste0(" 10.0 ,", CAVAL), loi_1_name)
  
  
  
  # Loi 3 : débit du bief étudié
  # Loi 4 et plus : débit apporté par chaque affluent du bief étudié
  
  
  for (i_apport in 1:nrow(length(Apports_non_nuls)))
  {
    valeur_apport = Apports_non_nuls[i_apport,2]
    
    loi_name = file.path(dir_prj, paste0(name_riv, "_", num_bv, "_", num_reach, "_", i_apport+2, ".loi"))
    file.create(loi_name)
    
    i_apport = as.numeric(i_apport)
    
    ligne_1_fichiers_loi = paste0(num_bv, "_",num_reach, "_L", i_apport+2, Apports_non_nuls[i_apport,1])
    
    write(ligne_1_fichiers_loi, loi_name)
    write("Temps (s) Debit", loi_name)
    write("         S", loi_name)
    write(paste0(" 0.0 ,", round(Apport[valeur_apport,2], digits=2)), loi_name)
    write(paste0(" 10.0 ,", round(Apport[valeur_apport,2], digits=2)), loi_name)
    
    
    # resultats flutor (Napport+2)
    
  }
  
}








## create run file mascaret .xcas
# LPo : création du fichier .xcas


file.copy(from = 
            to = dir_prj)

nomcas = file.path(dir_prj, paste0(name_cas_mascaret, ".xcas"))

write(paste0(name, ".geo"), file=nomcas)
#write(paste0(name, ".hyd"), file=nomcas, append=T)

## write the number of sections                                                    
# LPo : écriture dans le fichier .cas du nombre de XS


write(Nprofils, file=nomcas, append=T)
write(1, file=nomcas, append=T)

## write upstream discharge
# LPo : écriture dans le fichier .cas des débits en amont 

write(Bief_STA$Q[1], file=nomcas, append=T)

## write downstream boundary condition                                             
# LPo : écriture dans le fichier .cas de la condition aval

write(CAVAL, file=nomcas, append=T)

## write space increment for numerical computation - to be improved                
# LPo : écriture dans le fichier .cas du pas d'espace DX (- à améliorer)

write(DX, file=nomcas, append=T)

## estimate discharge inflows at each section                                      
# LPo : estimation des débits à chaque XS



## run executable                                                                  
# LPo : lancement de l'executable
file.copy(from = file.path(dir_exe, exe_flutor), to=dir_prj)
tmp_wd = getwd(); setwd(dir_prj)
system(exe_flutor, show.output.on.console=F); setwd(tmp_wd)

## get results
Bief_results = read.table(file.path(dir_prj, "RESULT1.txt"), skip=9, nrows=Nprofils)            
# LPo : stockage des résultats dans le fichier .txt

file.rename(file.path(dir_prj, "RESULT1.txt"), file.path(dir_prj, paste0(name, "_res.txt")))

## format results                                                                               
# LPo : stockage des résultats dans la variable Bief_results
Bief_results %<>% dplyr::select(SL_m=V4, CHAR_m=V17, Q_m3_s=V16, K=V10, H=V5, ZREF_m=V3, S1_m2=V7, VMIN_m_s=V9, FR=V14)
# LPo : rajout de colonnes - initialement on avait : 
# LPo : Bief_results %<>% dplyr::select(SL_m=V4, CHAR_m=V17, Q_m3_s=V16, K=V10, H=V5)
Bief_results$NSection = Bief_STA$NSection
Bief_results$NBief = Bief_STA$NBief

Bief_results$NBasVers = num_bv
# LPo : rajout de NBasVers

return(Bief_results)





# Début de code pour Mascaret : 
# à transformer en une fonction qui récupère la condition aval dans le fichier résultats 
# après avoir fait tourné une première fois FLUTOR 
# (= la charge du dernier profil en travers du bief (en m))




# if (opt_mod == "mascaret")
# {
#   
#   liste_dir_rep_area = dir.exists(rep_area)
#   
#   liste_rep_resu = grep(pattern = "Bv", x = liste_dir_rep_area)
#   
#   liste_caval_all_bv = list()
#   
#   for (i_bv_flutor in 1:nrow(liste_rep_resu))
#   {
#     
#     liste_results_flutor = file.exists(file.path(rep_resu, rep_flutor))
#     liste_res_flutor = grep(pattern = "_res.txt", x=liste_results_flutor)
#     
#     liste_caval_res=list()
#     
#     for (i_res in 1:nrow(liste_res_flutor))
#     {
#       res_flutor = liste_res_flutor[i_res]
#       
#       tab_res_flutor = read.table(file.path(paste0(rep_resu, rep_flutor), res_flutor), skip=9)
#       last_rows_res = slice_tail(tab_res_flutor, n=8)
#       caval_res = last_rows_res[1,16]
#       
#       nom_res_flutor = str(res_flutor, vec.length = 10)
#       
#       tab_caval_res = cbind("CAVAL" = caval_res, "NOM" = nom_res_flutor)
#       
#       liste_caval_res[[i_res]]= tab_caval_res[,]
#       
#     }
#     
#     tab_all_caval_res = do.call(rbind,liste_caval_res)
#     liste_caval_all_bv[[i_bv_flutor]]= tab_all_caval_res[,]
#     
#   }
#   tab_caval_all_bv = do.call(rbind, liste_caval_all_bv)
# }
