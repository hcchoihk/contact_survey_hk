library(grid)


colour_blue =  function(contactmatrix,max.value){
  colours_vec = c("white","steelblue", "navy")
  colours = colorRampPalette(colours_vec,bias=2.0)
  z = contactmatrix
  ncol=100
  allcol=colours(ncol)
  i=ceiling((z*ncol/max.value)^1)
  i=i*(i>0)+1*(i==0)
  cols=allcol[i]
}

colour_red =  function(contactmatrix,max.value){
  colours_vec = c("white","orange", "darkred")
  colours = colorRampPalette(colours_vec,bias=2.0) 
  z = contactmatrix
  ncol=100
  allcol=colours(ncol)
  i=ceiling((z*ncol/max.value)^1)
  i=i*(i>0)+1*(i==0)
  cols=allcol[i]
}


plotcontact_multi_plain = function(nrow, ncol, cols, group, a, INDEX, TITLE='', labelnames, at, rot_yticklabel=0){
  pushViewport(plotViewport(c(1.0,1,0.5,0.5),xscale=c(0,nrow*a),yscale=c(0,ncol*a)))
  grid.rect()
  if (missing(labelnames)){
	  labelnames = seq(0,ncol/2,by=1)*(a*2)
	  labelnames[ncol/2+1] = paste0(labelnames[ncol/2+1],'+')
  }
  if (missing(at)){
	at = list(
	at_xaxis = seq(0,nrow/2,by=1)*(a*2),
	at_yaxis = seq(0,ncol/2,by=1)*(a*2)
	)
  grid.xaxis(at=(0:nrow)*a,label=F,gp=gpar(fontsize=3))
  grid.yaxis(at=(0:ncol)*a,label=F,gp=gpar(fontsize=3))
  }
  grid.text(labelnames,x=unit(at$at_xaxis,"native"),y=unit(-1.2,'lines'), gp=gpar(fontsize=6))
  if (rot_yticklabel==0){
	grid.yaxis(at=at$at_yaxis,label=labelnames,gp=gpar(fontsize=6))
  } else{
	grid.text(labelnames,x=unit(-0.3,'lines'),y=unit(at$at_yaxis,"native"), hjust=1, rot=0, gp=gpar(fontsize=6))
  }
  
  x <- (1:ncol)/ncol
  y <- (1:nrow)/nrow
  right <- rep(x, nrow)
  top <- rep(y, each=ncol)
  grid.rect(x=right, y=top,width=1/ncol, 
            height=1/nrow,just=c("right", "top"),
            gp=gpar(col=cols,fill=cols))
  grid.text(paste0('(',INDEX,')'),unit(1,'lines'),unit(1,'npc')+unit(-0.8,'lines'),gp=gpar(fontsize=6.5))
  grid.text(paste0(TITLE),y = unit(1,'npc')+unit(0.8,'lines'),gp=gpar(fontsize=7))
  grid.rect(gp=gpar(col='black',fill=NA)) 
  popViewport()
} # plotcontact_multi_plain



plotcontact_multi_left = function(nrow, ncol, cols, group, a, INDEX, TITLE='', labelnames, at, rot_yticklabel=0){
  pushViewport(plotViewport(c(1.0,1,0.5,0.5),xscale=c(0,nrow*a),yscale=c(0,ncol*a)))
  grid.rect()
  if (missing(labelnames)){
	  labelnames = seq(0,ncol/2,by=1)*(a*2)
	  labelnames[ncol/2+1] = paste0(labelnames[ncol/2+1],'+')
  }
  if (missing(at)){
	at = list(
	at_xaxis = seq(0,nrow/2,by=1)*(a*2),
	at_yaxis = seq(0,ncol/2,by=1)*(a*2)
	)
  grid.xaxis(at=(0:nrow)*a,label=F,gp=gpar(fontsize=3))
  grid.yaxis(at=(0:ncol)*a,label=F,gp=gpar(fontsize=3))
  }
  grid.text(labelnames,x=unit(at$at_xaxis,"native"),y=unit(-1.2,'lines'), gp=gpar(fontsize=6))
  if (rot_yticklabel==0){
	grid.yaxis(at=at$at_yaxis,label=labelnames,gp=gpar(fontsize=6))
  } else{
	grid.text(labelnames,x=unit(-0.3,'lines'),y=unit(at$at_yaxis,"native"), rot=0, hjust=1, gp=gpar(fontsize=6))
  }
  grid.text('Age of contact',x=unit(-2.85,'lines'),rot=90,gp=gpar(fontsize=7))
  
  x <- (1:ncol)/ncol
  y <- (1:nrow)/nrow
  right <- rep(x, nrow)
  top <- rep(y, each=ncol)
  grid.rect(x=right, y=top,width=1/ncol, 
            height=1/nrow,just=c("right", "top"),
            gp=gpar(col=cols,fill=cols))
  grid.text(paste0('(',INDEX,')'),unit(1,'lines'),unit(1,'npc')+unit(-0.8,'lines'),gp=gpar(fontsize=6.5))
  grid.text(paste0(TITLE),y = unit(1,'npc')+unit(0.8,'lines'),gp=gpar(fontsize=7))
  grid.rect(gp=gpar(col='black',fill=NA)) 
  popViewport()
} # plotcontact_multi_left



plotcontact_multi_bottom = function(nrow, ncol, cols, group, a, INDEX, TITLE='', labelnames, at, rot_yticklabel=0){
  pushViewport(plotViewport(c(1.0,1,0.5,0.5),xscale=c(0,nrow*a),yscale=c(0,ncol*a)))
  grid.rect()
  if (missing(labelnames)){
	  labelnames = seq(0,ncol/2,by=1)*(a*2)
	  labelnames[ncol/2+1] = paste0(labelnames[ncol/2+1],'+')
  }
  if (missing(at)){
	at = list(
	at_xaxis = seq(0,nrow/2,by=1)*(a*2),
	at_yaxis = seq(0,ncol/2,by=1)*(a*2)
	)
  grid.xaxis(at=(0:nrow)*a,label=F,gp=gpar(fontsize=3))
  grid.yaxis(at=(0:ncol)*a,label=F,gp=gpar(fontsize=3))
  }
  grid.text(labelnames,x=unit(at$at_xaxis,"native"),y=unit(-1.2,'lines'), gp=gpar(fontsize=6))
if (rot_yticklabel==0){
	grid.yaxis(at=at$at_yaxis,label=labelnames,gp=gpar(fontsize=6))
  } else{
	grid.text(labelnames,x=unit(-0.3,'lines'),y=unit(at$at_yaxis,"native"), hjust=1, rot=0, gp=gpar(fontsize=6))
  }
  grid.text('Age of individuals',y=unit(-2.1,'lines'),gp=gpar(fontsize=7))
  
  x <- (1:ncol)/ncol
  y <- (1:nrow)/nrow
  right <- rep(x, nrow)
  top <- rep(y, each=ncol)
  grid.rect(x=right, y=top,width=1/ncol, 
            height=1/nrow,just=c("right", "top"),
            gp=gpar(col=cols,fill=cols))
  grid.text(paste0('(',INDEX,')'),unit(1,'lines'),unit(1,'npc')+unit(-0.8,'lines'),gp=gpar(fontsize=6.5))
  grid.text(paste0(TITLE),y = unit(1,'npc')+unit(0.8,'lines'),gp=gpar(fontsize=7))
  grid.rect(gp=gpar(col='black',fill=NA)) 
  popViewport()
} # plotcontact_multi_bottom

