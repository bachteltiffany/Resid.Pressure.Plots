ggplot(NULL, aes(y=temp_med, x=depth_med)) +
  coord_flip(xlim = z.range, ylim = T.range) +
  scale_x_continuous(trans=reverselog_trans(10)) + #log
  geom_smooth(data=ctd_data, aes(y=temp, x=depth, linetype=source), color="white", span=.7, method='loess', se=FALSE) +
  geom_point(data=filter(zvt_data.m, order == "bero"), aes(x = temp_med, y = depth_med), position="identity",size=12,alpha=0.5, color = "orange") +
  geom_point(data=filter(zvt_data.m, order == "cydi"), aes(x = temp_med, y = depth_med), position="identity",size=12,alpha=0.5, color = "tomato")+
  geom_point(data=filter(zvt_data.m, order == "loba"), aes(x = temp_med, y = depth_med), position="identity",size=12,alpha=0.5, color = "blue")+
  #geom_point(data=zvt_data.m, position="identity",size=12,alpha=0.5) +
  #geom_point(data=zvt_data.m, position="identity",size=12,alpha=0.5) +
  theme(#legend.position='none',
    plot.background = element_rect(fill ="black"),
    panel.background = element_rect(fill = 'black'),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "white",size = 1,linetype ="solid"),axis.text.x = element_text(angle = 45,size=10, colour="white"),axis.text.y = element_text( color="white",size=10))
# hide legend