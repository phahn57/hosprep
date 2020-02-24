## function.R

## Exclusion list for mistakes in arzt1

arzt_exc <- c("Müller-Stephann","Cakmak","Kaiser","Prce","Einecke","Reiter")

### relevant OPS for departements

df_rel <- list(ORTH=c("5-820|5-821|5-822|5-823"))
df_rel$H_CHI <- c("5-056|5-812|5-842|5-847")
df_rel$S_CHI <- c("5-814.3|5-814.4|5-814.1")
df_rel$F_CHI <- c("5-781|5-808.b0|5-808.a5")
df_rel$K_CHI <- c("5-812.5|5-813.4|5-812.eh")
df_rel$Kinder <- c("5-789.1h|5-854")


abteil <- function(df, abtt) {
        df %>% filter(abt == abtt) %>% group_by(jahr, form, mon) %>% summarise(
                drg = sum(effgewicht),
                faelle = n(),
                sn = sum(snzeit, na.rm = TRUE),
                verweil = sum(vwd, na.rm = TRUE)
        ) %>% ungroup()
}

### use grepl because of kvhch and kv mart former kvhuech
ermaech <- function(df, kvvar) {
        if (kvvar=="all")
                df
        else
        df %>% filter(grepl(kvvar,kat))
}

plotter_abt <- function(dd,xname, yname, ybez) {
        x_var <- enquo(xname)
        y_var <- enquo(yname)
        ggplot(dd, aes(x = !!x_var, y = !!y_var, group=jahr, colour=jahr))+
                geom_point() + geom_line() + theme_bw() +
                guides(colour = FALSE, alpha = FALSE, size = FALSE) +
                ggtitle(ybez, "prev.year (black)") + labs(x = "month", y = ybez) +
                scale_x_continuous(breaks = brk) + scale_y_continuous(limits = c(0, NA))
}



plotter <- function(df,xname, yname, xax, yax,akt,vor) {
        x_var <- enquo(xname)
        y_var <- enquo(yname)
        ggplot(df, aes(x = !!x_var, y = !!y_var)) +
                geom_col(data = df[df$jahr == akt, ], alpha = 0.3) +
                geom_point(data = df[df$jahr == vor, ], color = "blue") +
                theme_bw() + guides(colour = FALSE, alpha = FALSE, size = FALSE) +
                ggtitle(yax, "prev.year (blue)") + labs(x = xax, y = yax)
}




## Funktion für ops- Suche
plotter_ops <- function(df,dep,ops,bm){
        x <- df %>% filter(abt == dep) %>% filter(abt == dep) %>% filter_at(vars(contains("ops")),any_vars(grepl(ops,.))) ##filter(grepl(ops, ops1)|grepl(ops,ops2))
        x <- x %>% group_by(jahr, mon) %>% count()
        xp <- x %>% filter(mon <= bm) %>% group_by(jahr) %>% summarise(ops_n = n()) ## %>% dcast(htep_prim~jahr)

        ggplot(x, aes(mon, n, group=jahr, colour=jahr))+
                geom_point() + geom_line() + theme_bw() +
                guides(colour = FALSE, alpha = FALSE, size = FALSE) +
                ggtitle(ops, "prev.year (black)") + labs(x = "month", y = ops) +
                scale_x_continuous(breaks = brk) + scale_y_continuous(limits = c(0, NA))
}

### plot DB3
plotter_db3 <- function(df,dep,sj)  {
        plot_df <- df %>% filter(variable==dep)
        ggplot(plot_df,aes(as.factor(pl_is),value))+geom_col(aes(fill=art))+
        theme_bw()+guides(colour=FALSE,alpha=FALSE,size=FALSE)+labs(title=sj,x="Art",y="Wert")+ facet_wrap(~class)+scale_fill_manual(values=cbPalette)
}



# make table for tabular
table_db3 <- function(df,dep) {
        plot_df <- df %>% filter(variable==dep) %>% select(-variable)
        ### alternative approach:
        plot_df <- pivot_wider(plot_df, names_from =c(pl_is), values_from=value)
        plot_df <- plot_df %>% mutate(Abw=ist-plan) %>% mutate(prozAbw=round((Abw/plan)*100,digits=2))
        plot_df
}


### Top 5 Eingriffe mit OP_Zeit pro Operateur

 plot_sn <- function(fall,abb){
         suche <- df_rel[[abb]]
         xx <- fall %>% filter(abt==abb,op=="J") %>% select(c("ops1","ops2","snzeit","arzt1","vor_schnitt"))
         yy <- xx %>% gather(key=variable,value=val,-c("snzeit","arzt1","vor_schnitt"))
         zz <- filter(yy,str_detect(val, suche)) %>% filter(!is.na(snzeit)) %>% filter(!(arzt1 %in% arzt_exc)) %>% filter(!is.na(arzt1))#%>% mutate(val=substr(val,1,5))
         ## filter non relevant ops
         ff <- zz %>% group_by(val) %>% count()  %>% filter(n>40)
         zz <- left_join(ff,zz) %>% filter(snzeit<300) %>% filter(vor_schnitt< 120)
         ggplot(zz,aes(arzt1,snzeit))+
                 geom_boxplot()+ coord_flip()+ facet_wrap(~val,ncol=4,scales="free")

 }


 plot_vor <- function(fall,abb){
         suche <- df_rel[[abb]]
         xx <- fall %>% filter(abt==abb,op=="J") %>% select(c("ops1","ops2","snzeit","arzt1","vor_schnitt"))
         yy <- xx %>% gather(key=variable,value=val,-c("snzeit","arzt1","vor_schnitt"))
         zz <- filter(yy,str_detect(val, suche)) %>% filter(!is.na(vor_schnitt)) %>% filter(vor_schnitt>0)  %>% filter(!(arzt1 %in% arzt_exc)) %>% filter(!is.na(arzt1)) #%>% mutate(val=substr(val,1,5))
         ## filter non relevant ops
         ff <- zz %>% group_by(val) %>% count()  %>% filter(n>40)
         zz <- left_join(ff,zz) %>% filter(snzeit<300) %>% filter(vor_schnitt< 120)
         ggplot(zz,aes(arzt1,vor_schnitt))+
                 geom_boxplot()+ coord_flip()+ facet_wrap(~val,ncol=4, scales="free")

 }


 plotter_zuweiser <- function(kv,rg){
         xx <- kv %>% group_by(mon,jahr,einw) %>% tally() %>% ungroup()  ## ersetze einw durch praxis
         xx <- xx %>% mutate(ymd=ymd(paste(jahr,mon,1,sep="-")))
         xx <- xx[,c(3:5)]
         yy <- spread(xx,ymd,value=n,fill=0)
         ## modify means last 12 month
         nc <- ncol(yy)
         nc12 <- nc-12 #nc12:nc == last 12 month
         yy <- mutate(yy, sm = rowMeans(yy[, c(nc12:nc)])) %>% arrange(desc(sm)) %>% filter(!is.na(einw))  ## einw
         ## filter für Reihenfolge
         yy <- yy[rg[1]:rg[2],]
         ## separate sm
         sm <- yy[,c(1,ncol(yy))]
         ## gather
         zz <- gather(yy[,-(ncol(yy))],dat,n,-einw)
         # zz <- melt(yy[,c(1:ncol(yy)-1)], id = c("einw"))  ## einw
         zz <- left_join(zz,sm)
         zz <- mutate(zz,subject=reorder(einw,sm))
         zz <- mutate(zz,dat=ymd(dat))
         ggplot(zz, aes(x = dat, y = n)) + geom_col() + geom_smooth(method="gam") + facet_wrap("subject")+ scale_x_date(date_labels="%y%b")
}

 plotter_faelle <- function(data,abtt,rg){
         xx <-  data  %>% filter(abt==abtt) %>% filter(form=="voll"|form=="ambulantes operieren")
         xx <- xx %>% group_by(mon,jahr,einw) %>% tally() %>% ungroup()  ## ersetze einw durch praxis
         xx <- xx %>% mutate(ymd=ymd(paste(jahr,mon,1,sep="-")))
         xx <- xx[,c(3:5)]
         yy <- spread(xx,ymd,value=n,fill=0)
         ## modify means last 12 month
         nc <- ncol(yy)
         nc12 <- nc-12 #nc12:nc == last 12 month
         yy <- mutate(yy, sm = rowMeans(yy[, c(nc12:nc)])) %>% arrange(desc(sm))#  %>% filter(!is.na(einw))  ## einw
         ## filter für Reihenfolge
         yy <- yy[rg[1]:rg[2],]
         ## separate sm
         sm <- yy[,c(1,ncol(yy))]
         ## gather
         zz <- gather(yy[,-(ncol(yy))],dat,n,-einw)
         # zz <- melt(yy[,c(1:ncol(yy)-1)], id = c("einw"))  ## einw
         zz <- left_join(zz,sm)
         zz <- mutate(zz,subject=reorder(einw,sm))
         zz <- mutate(zz,dat=ymd(dat))
         ggplot(zz, aes(x = dat, y = n)) + geom_col() + facet_wrap("subject")+ scale_x_date(date_labels="%y%b")
 }

 plotter_praxis <- function(kv,bmon,pra){
         xx <- kv %>% filter(mon<=bmon) %>% group_by(jahr,einw) %>% tally() %>% ungroup()  ## filter für aktuelle monthe)
         xx <- spread(xx,jahr,value=n,fill=0)
         ## join_with praxis
         yy <- left_join(pra,xx) %>% select(-no,-einw)
         ## gather
         zz <- gather(yy,dat,n,-prax)
         #zz <- left_join(zz,pra)
         zz <- zz %>% group_by(dat,prax) %>% summarise(total=sum(n,na.rm=TRUE))
         ggplot(zz,aes(x=dat,y=total))+geom_col()+facet_wrap(~prax)
 }

 plotter_map <- function(df){
         # df <- y_akt$map
         m <- leaflet(df) %>% setView(9.0807,49.2244, zoom=10) %>% addTiles()
         bins = c(1,10,30,60,90,120,150,180,200,Inf)
         pal <- colorBin("PiYG", domain=df$H_CHI_18, bins = bins)
         m %>% addPolygons(
                 fillColor = ~pal(H_CHI_18),
                 weight = 2,
                 opacity= 1,
                 color="white",
                 dashArray = "3",
                 fillOpacity = 0.7)
        # m %>%
         #        addLegend(pal=pal,values=~density, opacity=0.9, title=NULL)
         # m %>%
         #         highlightOptions(
         #                 weight = 5,
         #                 color="#666",
         #                 dashArray = "",
         #                 fillOpacity = 0.4,
         #                 bringToFront = TRUE)

         m
 }
