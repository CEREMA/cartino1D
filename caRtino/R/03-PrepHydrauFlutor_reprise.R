#' @title Lancement du modèle hydraulique Flutor
#' @description ENG : Run hydraulic model flutor                                                        
#' 
#' FR : Cette fonction appartient au script 03-PrepHydrauFlutor_reprise.R
#' 
#' - Définition des paramètres
#' - Création et écriture des fichiers d'entrée du modèle Flutor : .geo, .hyd 
#' - Création et écriture du fichier .cas (= le fichier paramètres pour flutor) : 
#' - copie des profils en travers, débits en amont, condition aval, pas d'espace DX...
#' - Estimation des débits à chaque profil en travers, calculs hydrauliques
#' 
#' Bief_Q_reprise et Bief_MNT_reprise : contiennent les caractéristiques de tous les profils en travers
#'  
#' Cette fonction retourne les résultats des calculs hydrauliques dans la variable all_results_reprise. 
#' 
#' @param rep_modele_reprise la fonction s'applique sur le dossier de résultats / results directory
#' @param Bief_MNT_reprise la fonction s'applique sur le tableau de données avec l'altitude 
#' pour chaque point de chaque profil en travers / dataframe with elevation data for each section
#' @param Bief_Q_reprise la fonction s'applique sur le tableau de données avec les caractéristiques 
#' de chaque profil en travers (débit) / dataframe with info for each section (discharge)
#' @return la fonction retourne un tableau de données avec les résultats hydrauliques 
#' à chaque profil en travers / dataframe with results for each section

run_hydrau_flutor_reprise = function(rep_modele_reprise,
                                     name_reach_reprise,
                                     raster_debit_reprise, 
                                     flutor_caval, 
                                     flutor_dx, 
                                     coeff,
                                     final_sections_fusion,
                                     Bief_Q_reprise, 
                                     Bief_MNT_reprise)
 
{
  ## some parameters and constant values for flutor                   
  # LPo : Paramètres et valeurs constantes pour flutor
  # LPo : (strickler, condition aval et DX)
  Strickler = as.vector(coeff) 
  CAVAL = flutor_caval
  DX = flutor_dx
  # LPo : voir CARTINO_PARAM.R

    
    geom_name = file.path(rep_modele_reprise, paste0(name_reach_reprise, ".geo"))
    hyd_name  = file.path(rep_modele_reprise, paste0(name_reach_reprise, ".hyd"))
    
    file.create(geom_name)
    file.create(hyd_name)
    # LPo : création du fichier .geo et du fichier .hyd
    
    
    ## write first line
    write("DEBORD", file=hyd_name)                                                     
    # LPo : write permet l'écriture de la colonne "DEBORD" (débordement)
    
    ## get the distance of each cross sections along the stream
    dist = final_sections_fusion$DIST                                                               
    # LPo : final_sections_fusion$DIST stocke la distance cumulée à chaque section
    
    
    ## loop over the cross sections                                                    
    # LPo : boucle pour chaque XS
    Nprofils = nrow(Bief_Q_reprise %>% filter(NBief == num_reach_reprise & NBasVers == num_bv_reprise))                                                                
    # LPo : nombre de profils en travers = nrow(=nombre de lignes) du tableau Bief_Q_reprise
    # LPo : pour chaque bief et chaque BV
    
    
    for(j in 1:Nprofils)
    {
      ## write the distance and the elevation data    
      write(paste("PROFIL Bief_1", j, round(dist[j], digits=2)), file=geom_name, append=T)
      # LPo : écriture dans le fichier .geo de la distance cumulée à chaque section
      
      
      Profil_MNT_reprise = Bief_MNT_reprise %>% filter(NSection == j & NBief == num_reach_reprise & NBasVers == num_bv_reprise)
      write(paste(Profil_MNT_reprise$POSI, round(as.numeric(Profil_MNT_reprise$Z),digits=2), 'B'),                       
            file=geom_name, append=T)
      # LPo : écriture dans le fichier .hyd des altitudes à chaque point de chaque section (POSI)
      # LPo : digits = 2 : nombre de décimales
      # LPo : Initialement on avait :
      # write(paste(Profil_MNT_reprise$POSI, round(Profil_MNT_reprise$Z,digits=2), 'B'),                       
      #       file=geom_name, append=T)
      
      
      ## write section number, strickler minor, strickler major, head loss  
      # LPo : écriture dans le fichier .hyd des Strickler lit mineur et lit majeur pour chaque section
      write(paste(j, Strickler[1], Strickler[2], 0), file=hyd_name, append=T)                
      # LPo : append = true ?
      
      j=j+1
    }
    
    ## create run file flutor.cas
    # LPo : création du fichier flutor.cas (un fichier .cas par bief)
    nomcas = file.path(rep_modele_reprise, paste0("Flutor.cas"))
    write(paste0(name_reach_reprise, ".geo"), file=nomcas)
    write(paste0(name_reach_reprise, ".hyd"), file=nomcas, append=T)
    
    ## write the number of sections                                                    
    # LPo : écriture dans le fichier .cas du nombre de XS
    write(Nprofils, file=nomcas, append=T)
    write(1, file=nomcas, append=T)
    
    ## write upstream discharge
    # LPo : écriture dans le fichier .cas des débits en amont 
    write(Bief_Q_reprise$Q_m3_s[1], file=nomcas, append=T)
    
    ## write downstream boundary condition                                             
    # LPo : écriture dans le fichier .cas de la condition aval
    write(CAVAL, file=nomcas, append=T)
    
    ## write space increment for numerical computation - to be improved                
    # LPo : écriture dans le fichier .cas du pas d'espace DX - à améliorer?
    write(DX, file=nomcas, append=T)
    
    
    ## estimate discharge inflows at each section                                      
    # LPo : estimation des débits à chaque section
    NApport = length(Bief_Q_reprise$Q_m3_s)
    Apport = as.matrix(ncol=1, nrows=NApport, Bief_Q_reprise$Q_m3_s - Bief_Q_reprise$Q_m3_s[1])
    Apport = cbind(Apport, 0*Apport, 0*Apport)
    for (i in 2:NApport)
    {
      if (Apport[i,1] - Apport[i,2] > 0)
      {
        Apport[i,2] = max(Apport[1:i, 1] - sum(Apport[1:i-1, 2]))
      }
      Apport[i,3]=i
    }
    nb_reprise = which(Apport[,2]!=0)

    ## write discharge inflows                                                         
    # LPo : écriture dans le fichier .cas des débits
    write(length(nb_reprise), 
          file=nomcas, append=T)
    write(paste(Apport[nb_reprise,3], round(Apport[nb_reprise,2], digits=2)),
          file=nomcas, append=T)
    
    ## run executable                                                                  
    # LPo : lancement de l'executable
    file.copy(from = file.path(dir_exe, exe_flutor), to=rep_modele_reprise)
    tmp_wd = getwd(); setwd(rep_modele_reprise)
    system(exe_flutor, show.output.on.console=T); setwd(tmp_wd)
    
  
    ## get results
    file.rename(file.path(rep_modele_reprise, "RESULT1.txt"), file.path(rep_modele_reprise, paste0(name_reach_reprise, "_res.txt")))
    # LPo : on renomme le fichier résultat en un autre .txt (_res.txt) comme ça on conserve les résultats pour chaque bief
    
    Bi_results_reprise = read.table(file.path(rep_modele_reprise, paste0(name_reach_reprise, "_res.txt")), skip=9, nrows=Nprofils)            
    # LPo : lecture des résultats contenus dans le fichier _res.txt, résultats stockés dans la variable Bi_results_reprise
    # LPo : skip permet de ne pas lire les 9 premières lignes du fichier _res.txt 
    # LPo : car les résultats sont écrits à partir de la 10ème ligne dans le fichier _res.txt
    
    ## format results                                                                               
    Bi_results_reprise %<>% dplyr::select(Z_m=V4, CHAR_m=V17, Q_m3_s=V16, K=V10, H=V5, ZREF_m=V3, S1_m2=V7, VMIN_m_s=V9, FR=V14, LM=V22)
    # LPo : on ne sélectionne que les colonnes qui nous intéresse
    # LPo : rajout de colonnes - initialement on avait : 
    # LPo : Bief_results %<>% dplyr::select(SL_m=V4, CHAR_m=V17, Q_m3_s=V16, K=V10, H=V5)
    # LPO : V4 = côte = ZM = SL_m
    
    # LPo : Filtre par bief et par bv pour avoir le même nombres de lignes entre Bi_results_reprise et Bief_Q_reprise
    Bief_Q_reprise_filter = Bief_Q_reprise %>% filter(NBief == num_reach_reprise & NBasVers == num_bv_reprise)
    
    Bi_results_reprise$NSection = Bief_Q_reprise_filter$NSection 
    # LPo : rajout de la colonne NSection
    Bi_results_reprise$NBief = Bief_Q_reprise_filter$NBief 
    # LPo : rajout de la colonne NBief
    Bi_results_reprise$NBasVers = Bief_Q_reprise_filter$NBasVers
    # LPo : rajout de la colonne NBasVers


  return(Bi_results_reprise)
}

