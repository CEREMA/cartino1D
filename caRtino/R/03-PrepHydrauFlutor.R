#' @title Lancement du modèle hydraulique Flutor
#' @description ENG : Run hydraulic model flutor                                                        
#' 
#' FR : Cette fonction appartient au script 03-PrepHydrauFlutor.R
#' 
#' - Définition des paramètres
#' - Création et écriture des fichiers d'entrée du modèle Flutor : .geo, .hyd 
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

run_hydrau_flutor = function(rep_res, name_run, Bief_MNT_Iter, Bief_STA, i_iter)
  # LPo : préparation des fichiers pour les calculs flutor
{
  ## some parameters and constant values for flutor
  StricklerF = as.vector(Strick)
  CAVAL = flutor_caval
  DX = flutor_dx
  # LPo : Paramètres : Strickler, condition aval et DX
  # LPo : voir CARTINO_PARAM.R
  
  
  ## create some input files .geo .hyd                                               
  # LPo : Création des fichiers .geo et .hyd propres à flutor
  dir_prj = file.path(rep_resu, rep_res); 
  dir.create(dir_prj, recursive=T, showWarnings=F)           
  # LPo : rep_resu = "01-RUN/01-CEREMA/02-OUT/garonette3/Bv01" et ".../BV02" s'il y en a plusieurs etc
  
  name = name_run
  geom_name = file.path(dir_prj, paste0(name, ".geo"))    
  hyd_name  = file.path(dir_prj, paste0(name, ".hyd"))    
  file.create(c(geom_name, hyd_name))
  # LPo : Création du fichier .geo
  # LPo : Création du fichier .hyd
  
  
  ## write first line
  write("DEBORD", file=hyd_name)                                                     
  # LPo : write permet la création et l'écriture d'une colonne "DEBORD" (débordement) dans le .hyd
  
  ## get the distance of each cross sections along the stream
  dist = Bief_STA$DIST                                                               
  # LPo : Bief_STA$DIST donc maintenant dist contiennent la distance cumulée à chaque profil
  
  ## loop over the cross sections                                                    
  # LPo : Boucle pour chaque profil
  Nprofils = nrow(Bief_STA)                                                                
  # LPo : Nombre de profils en travers (nrow(Bief_STA) = nombre de lignes du tableau Bief_STA)
  
  for(j in 1:Nprofils)
  {
    ## write the distance and the elevation data    
    # LPo : Ecriture dans le fichier .geo des distances cumulées 
    # LPo : et des altitudes pour chaque profils
    write(paste("PROFIL Bief_1", j, dist[j]), 
          file=geom_name, append=T)
    Profil_MNT = Bief_MNT_Iter %>% filter(NSection==j)
    write(paste(Profil_MNT$POSI, round(Profil_MNT$Z,digits=2), 'B'),                       
          file=geom_name, append=T)
    # LPo : digits = 2 : nombre de décimales
    
    
    ## write section number, strickler minor, strickler major, head loss                   
    # LPo : Ecriture dans .hyd
    write(paste(j, Strick[1], Strick[2], 0), file=hyd_name, append=T)                
    # LPo : append = true signifie les données sont annexées (?)
  }
  
  
  ## create run file flutor.cas
  # LPo : Création du fichier .cas
  nomcas = file.path(dir_prj, "Flutor.cas")
  write(paste0(name, ".geo"), file=nomcas)
  write(paste0(name, ".hyd"), file=nomcas, append=T)
  
  ## write the number of sections                                                    
  # LPo : Ecriture dans le fichier .cas du nombre de profils
  write(Nprofils, file=nomcas, append=T)
  write(1, file=nomcas, append=T)
  
  ## write upstream discharge
  # LPo : Ecriture dans le fichier .cas du débit amont (au profil n°1) 
  write(Bief_STA$Q[1], file=nomcas, append=T)
  
  ## write downstream boundary condition                                             
  # LPo : Ecriture dans le fichier .cas de la condition aval
  write(CAVAL, file=nomcas, append=T)
  
  ## write space increment for numerical computation - to be improved                
  # LPo : Ecriture dans le fichier .cas du pas d'espace DX 
  write(DX, file=nomcas, append=T)
  
  ## estimate discharge inflows at each section                                      
  # LPo : Estimation des débits apportés à chaque profil (utile pour les confluences)
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
  
  
  ## write discharge inflows                                                         
  # LPo : Ecriture dans le fichier .cas des débits apportés aux confluences
  write(length(nb), 
        file=nomcas, append=T)
  write(paste(Apport[nb,3], round(Apport[nb,2], digits=2)),
        file=nomcas, append=T)
  
  
  
  ## run executable                                                                  
  # LPo : lancement de l'executable
  file.copy(from = file.path(dir_exe, exe_flutor), to=dir_prj)
  tmp_wd = getwd()
  txtfile=file.path(paste0(tmp_wd, "/", dir_prj, "/RESULT1.txt"))
  file.remove(txtfile, showWarnings = F)
  
  setwd(dir_prj)
  system(exe_flutor, show.output.on.console=T); setwd(tmp_wd)
  #system(shQuote(exe_flutor), show.output.on.console=T); setwd(tmp_wd)
  
  if (file.exists(txtfile)==F) 
  {
    Bief_results=NULL
  }else{
    ## get results
    Bief_results = read.table(txtfile, skip=9, nrows=Nprofils)            
    # LPo : Bief_results : lecture du fichier .txt qui contient les résultats
    # LPo : skip = 9 : Ne tient pas compte des 9 premières lignes
    
    file.rename(file.path(dir_prj, "RESULT1.txt"), file.path(dir_prj, paste0(name, "_res.txt")))
    # LPo : On renomme le fichier .txt pour pouvoir passer au tour suivant
    
    
    ## format results                                      
    Bief_results %<>% dplyr::select(SL_m=V4, CHAR_m=V17, Q_m3_s=V16, K=V10, H=V5, ZREF_m=V3, S1_m2=V7, VMIN_m_s=V9, FR=V14)
    # LPo : Sélection des résultats qui nous intéressent uniquement 
    # LPo : Rajout de colonnes
    # LPo : LIGNE D'ORIGINE :
    # LPo : Bief_results %<>% dplyr::select(SL_m=V4, CHAR_m=V17, Q_m3_s=V16, K=V10, H=V5)
    Bief_results$NSection = Bief_STA$NSection
    Bief_results$NBief = Bief_STA$NBief
    
    Bief_results$NBasVers = num_bv
    # LPo : Rajout de la colonne NBasVers
  }
  return(Bief_results)
}

