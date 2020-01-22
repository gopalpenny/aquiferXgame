# plotting_functions.R

# 5 plotting functions
# 5a-i plot_treaty_heatmap -- included zRange, zMinSwiss, zMaxFrench, q_vals, u_vals, d_vals
# 5a-ii plot_treaty_lines lines -- included zRange, zMinSwiss, zMaxFrench, q_vals, u_vals, d_vals
# 5b scenario_eval_and_plot -- evaluate and plot scenarios
# 5c ggsub -- gsub, with original string in first position for piping

# 5 plotting functions
# 5a-i plot_treaty_heatmap -- included zRange, zMinSwiss, zMaxFrench, q_vals, u_vals, d_vals
plot_treaty_heatmap <- function(treaty_cases,treaty_cases_interp,x_var,y_var,xl,yl,gt,z_sfc=NULL) {
  # x_var and y_var only used for identifying plotting variables. xl and yl used for labels
  treaty_cases_heatmap <- treaty_cases %>% rename_("x"=x_var[1],"y"=y_var[1])
  treaty_cases_heatmap_interp <- treaty_cases_interp %>% rename_("x"=x_var[1],"y"=y_var[1])
  p_zr <- ggplot(treaty_cases_heatmap_interp) +
    geom_raster(aes(x,y,fill=zRange)) + xlab(xl) + ylab(yl) +
    # geom_raster(data=treaty_cases_heatmap%>% filter(zRange<0),aes(x,y),fill="black",alpha=0.25) + xlab(xl) + ylab(yl) +
    scale_fill_gradient2(low="red",high="blue",midpoint=0)  +
    ggtitle(gt)+
    coord_cartesian(xlim=c(min(treaty_cases_heatmap$x),max(treaty_cases_heatmap$x)),
                    ylim=c(min(treaty_cases_heatmap$y),max(treaty_cases_heatmap$y))) +
    theme(legend.position="right",legend.title=element_text(size=8),
          title=element_text(size=8),axis.title=element_text(size=14)) #colors=colorspace::diverge_hcl(n=5,c = 70,l=c(40,100),h=c(30,180),power=.3))
  if (!is.null(z_sfc)) {
    z_sfc_plot <- z_sfc %>% rename_("x"=x_var[1],"y"=y_var[1])
    p_zr <- p_zr + geom_line(data=z_sfc_plot,aes(x,y,linetype=sfc)) +
      scale_linetype("")
  }
  p_fr <- ggplot(treaty_cases_heatmap) +
    geom_raster(aes(x,y,fill=zMaxFrench)) + xlab(xl) + ylab(y_var[1]) +
    scale_fill_gradient2(low="red",high="blue",midpoint=0) +
    theme(legend.position="right",legend.title=element_text(size=8)) #colors=colorspace::diverge_hcl(n=5,c = 70,l=c(40,100),h=c(30,180),power=.3))
  p_sw <- ggplot(treaty_cases_heatmap) +
    geom_raster(aes(x,y,fill=zMinSwiss)) + xlab(xl) + ylab(y_var[1]) +
    scale_fill_gradient2(low="blue",high="red",midpoint=0) +
    theme(legend.position="right",legend.title=element_text(size=8))

  game_levels <- c("xxhat","xxstar","xxdouble","xxhat_double")
  thm_vals <-  theme(legend.position="bottom",legend.title=element_blank())
  thm_vals2 <-  thm_vals %+replace% theme(axis.title.y=element_blank(),axis.title.x=element_blank())
  fill_scale <- scale_fill_gradientn(colors=colorspace::heat_hcl(n=15,h=c(0,360),c=c(90,50),l=c(20,80)))
  p_qs <- ggplot(treaty_cases_heatmap %>% gather(var,val,starts_with("qs",ignore.case = FALSE)) %>% select(x,y,var,val) %>%
                   mutate(var=factor(var,levels=gsub("xx","qs",game_levels)))) +
    geom_raster(aes(x,y,fill=val)) + facet_wrap(~var,ncol=1) + xlab(xl) + ylab(yl) + thm_vals + fill_scale
  p_qf <- ggplot(treaty_cases_heatmap %>% gather(var,val,starts_with("qf",ignore.case = FALSE)) %>% select(x,y,var,val) %>%
                   mutate(var=factor(var,levels=gsub("xx","qf",game_levels)))) +
    geom_raster(aes(x,y,fill=val)) + facet_wrap(~var,ncol=1) + xlab(xl) + ylab(yl) + thm_vals + fill_scale
  p_Us <- ggplot(treaty_cases_heatmap %>% gather(var,val,starts_with("Us",ignore.case = FALSE)) %>% select(x,y,var,val) %>%
                   mutate(var=factor(var,levels=gsub("xx","Us_",game_levels)))) +
    geom_raster(aes(x,y,fill=val)) + facet_wrap(~var,ncol=1) + thm_vals2 + fill_scale
  p_Uf <- ggplot(treaty_cases_heatmap %>% gather(var,val,starts_with("Uf",ignore.case = FALSE))  %>% select(x,y,var,val) %>%
                   mutate(var=factor(var,levels=gsub("xx","Uf_",game_levels)))) +
    geom_raster(aes(x,y,fill=val)) + facet_wrap(~var,ncol=1) + thm_vals2 + fill_scale

  depth <- treaty_cases_heatmap %>% gather(var,depth,starts_with("ds",ignore.case = FALSE),starts_with("df",ignore.case = FALSE)) %>% select(var,depth) %>% pull(depth)
  if (min(depth) > 0) {
    # depth_scale <- scale_fill_gradientn(colors=colorspace::heat_hcl(n=10,h=c(180,250),c=c(60,90),l=c(75,25)))
    depth_scale <- fill_scale
  } else {
    depth_scale <- scale_fill_gradient2(low="darkred",high="darkgreen",midpoint=0)
  }
  p_ds <- ggplot(treaty_cases_heatmap %>% gather(var,val,starts_with("ds",ignore.case = FALSE)) %>% select(x,y,var,val) %>%
                   mutate(var=factor(var,levels=gsub("xx","ds_",game_levels)))) +
    geom_raster(aes(x,y,fill=val)) + facet_wrap(~var,ncol=1) + thm_vals2 + depth_scale
  p_df <- ggplot(treaty_cases_heatmap %>% gather(var,val,starts_with("df",ignore.case = FALSE))  %>% select(x,y,var,val) %>%
                   mutate(var=factor(var,levels=gsub("xx","df_",game_levels)))) +
    geom_raster(aes(x,y,fill=val)) + facet_wrap(~var,ncol=1) + thm_vals2+depth_scale
  gridExtra::grid.arrange(p_zr,p_sw,p_fr,p_qs,p_qf,p_Us,p_Uf,p_ds,p_df,
                          layout_matrix=cbind(matrix(c(1,1,2,1,1,3,rep(1:6+3,each=3)),nrow=3)),
                          heights=c(0.34,0.4,0.26),widths=c(1/6,1/6,rep((1-1/3)/6,6)))
}

# 5a-ii plot_treaty_lines lines -- included zRange, zMinSwiss, zMaxFrench, q_vals, u_vals, d_vals
plot_treaty_lines <- function(treaty_cases,y_level,x_var,y_var,xl,yl,gt,z_sfc=NULL) {
  treaty_cases_lines <- treaty_cases %>% rename_("x"=x_var[1],"y"=y_var[1]) %>%
    mutate(y_diff=abs(y-y_level),min_y_diff=y_diff==min(y_diff)) %>%
    filter(min_y_diff)
  p_zr <- ggplot(treaty_cases_lines) +
    geom_line(aes(x,zRange)) + xlab(xl) + ggtitle(gt) +
    theme(legend.position="right",legend.title=element_text(size=8),
          title=element_text(size=8),axis.title=element_text(size=14)) #colors=colorspace::diverge_hcl(n=5,c = 70,l=c(40,100),h=c(30,180),power=.3))
  p_zi <- ggplot(treaty_cases_lines %>% gather(z_var,z_val,zMaxFrench,zMinSwiss)) +
    geom_segment(data=treaty_cases_lines %>% filter(zRange > 0),
                 aes(x=x,xend=x,y=zMinSwiss,yend=zMaxFrench),alpha=0.5) +
    geom_line(aes(x,y=z_val,color=z_var)) + xlab(xl) #+#+ #ylab(yl) +
  # theme(legend.position=c(0.9,0.1))

  game_levels <- c("xxhat","xxstar","xxdouble","xxhat_double")
  thm_vals <-  theme(legend.position="bottom",legend.title=element_blank())
  p_q <- ggplot(treaty_cases_lines %>% gather(var,val,starts_with("qs",ignore.case = FALSE),starts_with("qf",ignore.case = FALSE)) %>% select(x,y,var,val) %>%
                  mutate(q_var=gsub("q[sf]","q_",var),
                         player=gsub("q([sf]).*","\\1",var),
                         q_var=factor(q_var,levels=gsub("xx","q_",game_levels)))) +
    geom_line(aes(x,y=val,color=q_var,linetype=player),size=1.5,alpha=0.7)  + xlab(xl) + thm_vals +ylab("Pumping")
  p_U <- ggplot(treaty_cases_lines %>% gather(var,val,starts_with("Us",ignore.case = FALSE),starts_with("Uf",ignore.case = FALSE)) %>% select(x,y,var,val) %>%
                  mutate(u_var=gsub("U[sf]","U",var),
                         player=gsub("U([sf]).*","\\1",var),
                         u_var=factor(u_var,levels=gsub("xx","U_",game_levels)))) +
    geom_line(aes(x,y=val,color=u_var,linetype=player),size=1.5,alpha=0.7) + xlab(xl) + thm_vals +ylab("Utility")

  p_d <- ggplot(treaty_cases_lines %>% gather(var,val,starts_with("ds",ignore.case = FALSE),starts_with("df",ignore.case = FALSE)) %>% select(x,y,var,val) %>%
                  mutate(d_var=gsub("d[sf]","d",var),
                         player=gsub("d([sf]).*","\\1",var),
                         d_var=factor(d_var,levels=gsub("xx","d_",game_levels)))) +
    geom_line(aes(x,y=-val,color=d_var,linetype=player),size=1.5,alpha=0.7) + xlab(xl) + thm_vals + ylab("-Depth")

  p_blank <- grid::grid.rect(gp=grid::gpar(col="white"))
  gridExtra::grid.arrange(p_zr,p_zi,p_q,p_U,p_d,p_blank,
                          layout_matrix=rbind(c(1,1,3:5),c(2,2,6,6,6)),
                          heights=c(0.6,0.4)) #,widths=c(1/6,1/6,rep((1-1/3)/6,6)))
}

# 5b scenario_eval_and_plot -- evaluate and plot scenarios
scenario_eval_and_plot <- function(params_basic,params_df,x_var,y_var,root_init,eps_vals=0,
                                   scenario_name,xlabel,ylabel,output_dir_path,instr="hr",y_level=NULL,
                                   treaty_cases=NULL,z0_surface=NULL) {
  # this function evaluates a treaty scenario given parameter sets (params_df)
  # params basic is used only to generate title to ID fixed parameters
  # params_df is used to evaluate the treaty
  # x_var can be 1 or 2 variable names -- the x axis
  # y_var can be 1 or 2 variable names -- the y axis
  # plotting: y_var[1] vs x_var[1]
  # instr: h - plot heatmap, l - plot lines at y_val=y_level, i - interpolate, r - include root finding in plot
  cat("beginning",scenario_name,"...\n")
  cat("running evaluate_agreement_cases...\n")

  if (is.null(treaty_cases)) {
    treaty_cases <- evaluate_agreement_cases(params_df,"tpqud")
  }
  if (max(grepl("i",instr))) {
    cat("interpolating surface...\n")
    orig_cases <- treaty_cases[,c(x_var[1],y_var[1],"zRange")] %>% set_names(c("x","y","z"))
    new_locations <- with(orig_cases,tibble(xo=seq(min(x),max(x),length.out=100),
                                            yo=seq(min(y),max(y),length.out=100)))
    treaty_cases_interp_prep <- akima::interp(orig_cases$x,orig_cases$y,orig_cases$z,
                                              new_locations$xo,new_locations$yo)
    treaty_cases_interp_xyz <- bind_cols(expand(new_locations,yo,xo),
                                         z=as.vector(treaty_cases_interp_prep$z))

    z0_surface <- treaty_cases_interp_xyz %>% expand(nesting(xo,yo,z),eps=eps_vals) %>% # for every eps_val, duplicate x,y,z
      mutate(z_eps=z-eps,abs_z_eps=abs(z_eps)) %>% group_by(xo,eps) %>%
      mutate(zmin=min(z_eps),zmax=max(z_eps),min_abs_z=min(abs_z_eps),
             x_contains_0=zmax>0 & zmin < 0,
             y_is_min_z=abs_z_eps==min_abs_z) %>%
      filter(x_contains_0,y_is_min_z) %>% # filter for x contains 0, and y is minimum z
      summarize(y=mean(yo)) %>% group_by() %>%
      set_names(c(x_var[1],"eps",y_var[1])) %>% mutate(sfc=paste0("eps=",eps)) %>%
      select(-eps)
    treaty_cases_interp <- treaty_cases_interp_xyz %>% set_names(c(y_var[1],x_var[1],"zRange"))
  } else {
    treaty_cases_interp <- treaty_cases[,c(x_var[1],y_var[1],"zRange")]
  }

  if (is.null(z0_surface) & max(grepl("r",instr))) {
    cat("getting z0_surface...\n")
    z0_surface <- do.call(rbind,lapply(eps_vals,get_roots,
                                       params_df=params_df,treaty_cases=treaty_cases,
                                       x_var=x_var,y_var=y_var,
                                       root_init = root_init))
  }
  if (max(grepl("r",instr)==0) & !max(grepl("i",instr))) {
    cat("skipping z0_surface roots...\n")
    z0_surface <- data.frame(x=integer(0),y=integer(0),z=integer(0)) %>% set_names(c(x_var[1],y_var[1],"sfc"))
  }
  if (max(grepl("s",instr))) {
    output_filename <- paste0(gsub(" ","_",scenario_name),"_data.csv")
    cat("saving scenario to",output_filename,"...\n")
    write_csv(treaty_cases,file.path(output_dir_path,output_filename))
  }
  # plotting stuff
  vartext <- params_basic %>% gather(var,val) %>% mutate(vars=paste(var,val,sep="=")) %>%
    pull(vars) %>% paste(collapse = ", ") %>% strwrap(width=70) %>% paste0(collapse="\n")
  titletext <- paste0(scenario_name,"\n",vartext)
  if (max(grepl("h",instr))) {
    output_filename <- paste0(gsub(" ","_",scenario_name),"_heatmaps.png")
    # treaty_cases_plot <- treaty_cases %>% set_names(names(treaty_cases) %>% ggsub(x_var[1],"x") %>% ggsub(y_var[1],'y'))
    # z_sfc_plot <- z0_surface %>% set_names(names(z0_surface) %>% ggsub(x_var[1],"x") %>% ggsub(y_var[1],'y'))
    cat("plotting scenario to",output_filename,"...\n")
    png(file.path(output_dir_path,output_filename),width=w1,height=h1)
    plot_treaty_heatmap(treaty_cases,treaty_cases_interp,x_var,y_var,xl=xlabel,yl=ylabel,gt=titletext,z_sfc=z0_surface)
    dev.off()
  } else {
    cat("heatmap plots not requested.\n")
  }
  if (max(grepl("l",instr))) {
    output_filename <- paste0(gsub(" ","_",scenario_name),"_",paste(y_var,collapse="_"),"_equals_",y_level,".png")
    cat("plotting scenario to",output_filename,"...\n")
    png(file.path(output_dir_path,output_filename),width=w1,height=h1)
    plot_treaty_lines(treaty_cases,y_level=y_level,x_var=x_var,y_var=y_var,
                      xl=xlabel,yl=ylabel,gt=titletext,z_sfc=z_sfc_plot)
    dev.off()
  } else {
    cat("line plots not requested.\n")
  }
  cat("done.\n")
  return(list(treaty_cases=treaty_cases %>% mutate(treaty_signed=zRange>0),
              z0_surface=z0_surface))
}

# 5c ggsub -- gsub, with original string in first position for piping
ggsub <- function(str_orig,str_match,str_replace) {
  gsub(str_match,str_replace,str_orig)
}
