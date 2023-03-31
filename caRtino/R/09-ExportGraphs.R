#' Graph water level along the stream
#' @noRd
graph_water_dist = function(Bief_results, nom_calc, name_res)                                
# LPo : appelée dans le script 05-CheckResults.R
{
  tmp =
  Bief_results %>%
  dplyr::select(DIST, SL_m, CHAR_m, Z_REF) %>%
  tidyr::gather(VAR, VAL, -DIST)
  g =
    ggplot() +
    geom_line(data=tmp, aes(x=DIST, y=VAL, colour=VAR, group=VAR)) +
    geom_point(data=tmp[tmp$VAR=="Z_REF", ], aes(x=DIST, VAL), size=0.5, colour="blue") +
    labs(x="Position sur le lineaire (m)", y="Elevation (m)", title = nom_calc) +
    scale_colour_manual(name="",
                        labels=c("Charge", "Surface Libre", "Fond du lit"),
                        values=c("brown", "purple", "blue")) +
    theme_bw(base_size=16) +
    theme(legend.position="top")
  ggsave(paste0(name_res, "_level.png"), width=20, height=10, units="cm", dpi=600)

  tmp =
    Bief_results %>%
    dplyr::select(DIST, Q_m3_s) %>%
    tidyr::gather(VAR, VAL, -DIST)
  g =
    ggplot() +
    geom_line(data=tmp, aes(x=DIST, y=VAL, colour=VAR, group=VAR)) +
    geom_point(data=tmp[tmp$VAR=="Z_REF", ], aes(x=DIST, VAL), size=0.5, colour="blue") +
    labs(x="Position sur le lineaire (m)", y="Discharge (m3/s)", title = nom_calc) +
    scale_colour_manual(name="",
                        labels=c("Débit"),
                        values=c("brown")) +
    theme_bw(base_size=16) +
    theme(legend.position="top")
  ggsave(paste0(name_res, "_discharge.png"), width=20, height=10, units="cm", dpi=600)
}

#' Graph streamnet
#' @noRd
  graph_streamnet = function(new_Lines, junctions)
  {
  ggplot(data=new_Lines) + 
    geom_sf(data=new_Lines, aes(colour=factor(ID_BV))) +
    geom_sf(data=junctions) + facet_grid(ID~.) +
    theme_bw() + theme(legend.position="bottom") +
    theme(plot.title = element_text(hjust = 0.5))
  }

#' Graph elevation data
#' @noRd
graph_z = function(xs_sta_pt)
{
  
  ggplot(data=xs_sta_pt) +
    geom_line(aes(POSI, Z, group=NSection, colour=NSection)) +
    theme_bw() + theme(legend.position="bottom")
}

#' Graph streamnet and intersection of cross sections
#' @noRd
graph_profil = function(trans_bief_xy, trans_pt_intersects)                             
# LPo : trans_bief_xy est la variable de la fonction create_sfLines dans le script 01-PreHydrau.R
{

  ggplot() +
    geom_path(data=df_bief, aes(X, Y, group=group, colour=id)) +
    geom_line(data=trans_bief_xy, aes(X, Y, group=NSection), colour="orange") +
    geom_point(data=trans_bief_xy, aes(X, Y), colour="orange") +
    geom_point(data=trans_pt_intersects, aes(x, y), size=3, colour="darkred") +
    coord_equal() +
    theme_bw() + theme(legend.position="bottom")
}

