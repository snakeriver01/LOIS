library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyRGL)
library(rgl)
library( GenKern )
library(googlesheets)
library(DT)
library(tseries)
library(forecast)
library(htmlwidgets); 
library(rglwidget) 
library(rsconnect)

KDSurface=function( x, y, era=200, varNames, gridSize ){
# global probability surface
kdSurface=KernSur(x=x, y=y, xgridsize=gridSize, ygridsize=gridSize, 
                                  xbandwidth=dpik(x),
                                  ybandwidth=dpik(y) )

xlim=range( kdSurface$xord )
ylim=range( kdSurface$yord )
zlim=range( as.vector( kdSurface$zden )   )
colorTime=matrix( "white", gridSize, gridSize ) 
colorAlpha=matrix( 1.0, gridSize, gridSize )
zz = kdSurface$zden   # this is a mx
fhat=as.vector( zz )

# limit quantiles to non-zero probs
q=quantile(  fhat, probs=c( 0.20,    # 1   transparent white
                                            0.45,      # 2 pureRed
			         0.65,     # 3   pureGreen
                                             0.90 )     # 4     pureBlue
                ); 
                                                # above 0.90  # 5 violet

colorTime[ (zz<=q[1])   ] = 'white'
colorAlpha[ (zz<=q[1]) ] = 0.75

colorTime[ ( zz>q[1] & zz<=q[2] ) ]='red'
colorAlpha[ ( zz>q[1] & zz<=q[2] ) ] = 1.0

colorTime[ ( zz>q[2] & zz<=q[3])  ]='green1'
colorAlpha[(  zz>q[2] & zz<=q[3]) ] = 1.0

colorTime[    zz>q[3] & zz<=q[4] ]='blue'
colorAlpha[  zz>q[3] & zz<=q[4] ] = 1.0

colorTime[ ( q[4]<=zz ) ]='magenta'
colorAlpha[ ( q[4]<=zz )    ] = 1.0

colorTime=matrix(colorTime, ncol=gridSize, nrow=gridSize )

open3d()
rgl.material( shininess=30.0 )

persp3d(  x=kdSurface$xord, y=kdSurface$yord,  z=kdSurface$zden,    
                  col=colorTime,
#                xlim=xlim, ylim=ylim, zlim=zlim, 
                ann=T, add=F, top=T, xlab="", ylab="", zlab="" ) 
title3d( xlab=varNames[1], ylab=varNames[2], zlab="Density Score" )

grid3d( side="z", at = NULL, col = "gray", lwd = 1, lty = 1, n = 5)

#return( colorTime )

return(kdSurface)

}			# end of function 


KDSurfaceColors=function( x, y, gridsize  ){

# global probability surface
kdSurface=KernSur(x=x, y=y, xgridsize=gridsize, ygridsize=gridsize, 
                                  xbandwidth=dpik(x),
                                  ybandwidth=dpik(y) )
colorTime=matrix( 'white', gridsize, gridsize )
colorAlpha=matrix( 1.0, gridsize, gridsize )

zz = kdSurface$zden
fhat=as.vector( zz )
# limit quantiles to non-zero probs
q=quantile(  fhat, probs=c( 0.40,         # 1   transparent white
                                            0.60,         # 2 pureRed
			                0.80,        # 3   pureGreen
                                            0.90 )       # 4     pureBlue
                                   # above 0.90     # 5 violet
                                   # brushed          # gold
) ;  # end quantile documented

colorTime[ (zz<=q[1]) ] = 'white'
colorAlpha[ (zz<=q[1])  ] = 0.75

colorTime[ ( zz>q[1] & zz<=q[2] ) ]='red'
colorAlpha[ ( zz>q[1] & zz<=q[2] ) ] = 1.0

colorTime[ ( zz>q[2] & zz<=q[3])  ]='green1'
colorAlpha[(  zz>q[2] & zz<=q[3]) ] = 1.0

colorTime[    zz>q[3] & zz<=q[4] ]='blue'
colorAlpha[  zz>q[3] & zz<=q[4]  ] = 1.0

colorTime[  ( q[4]<=zz ) ] = 'magenta'
colorAlpha[ ( q[4]<=zz ) ] = 1.0

return( list(colorTime=colorTime, colorAlpha=colorAlpha )  )

}			# end of function 


JLo.Pts <-function( response, pr1, pr2, degree=2, span=.25, 
varNames, response.name=response.name, colorTime=colorTime, gridSize=10, weights=1:length(response) ,  
 bw='yes' )
{ 

colorMaps = KDSurfaceColors( x=pr1, y=pr2, gridsize=gridSize )

lois<-loess( response ~ pr1+pr2 , degree=degree, span=span,
weights=weights, 
#    family='symmetric',   surface='direct',         
control=loess.control(surface="interpolate", trace.hat="approximate",  cell=.20 ) )

open3d()
# fancy with the best of intentions but bad results
#     #   material3d( texminfilter= "nearest.mipmap.linear" )
material3d( texminfilter= "linear" )
plot3d(  x=pr1, y=pr2, z=response, type="s", size=1.30, col=colorTime ,  
             xlab=varNames[1] , ylab=varNames[2], zlab=response.name,  
             xlim=range( pr1 ), ylim=range( pr2 ), zlim=range(response)  )                     
 grid3d(side="z", at = NULL, col = "black", lwd = 2, lty = 1, n = 5)
 # colors by pain.level
                                                                                                  
g1<-seq( min(pr1), max(pr1), length=gridSize )
g2<-seq( min( pr2 ), max(pr2), length=gridSize )

# allow predictions only when there is supporting data 
gridIron<-expand.grid(pr1=g1,pr2=g2)
dd.lo.pr<-predict( lois, newdata=gridIron, na.action = na.omit  )

persp3d( g1, g2, z=dd.lo.pr, col=colorMaps$colorTime,  #alpha=colorMaps$colorAlpha, 
zlim=range(response),
xlab = varNames[1], ylab = varNames[2], zlab = response.name,
main = 'Local Regression Stimulus Response Surface with Data Point Cloud',
sub=paste( 'approx degrees of freedom trace:',lois$trace.hat)  )

}  # end of function

PlotLois = function( x, response.name, lwd=1.25 )
{  # x is a matrix or data.frame with cols = response, fit, error, dateLine
# copyright Lloyd Lubet  2008  and 2015
# all rights reserved

e    =  x[ , 'error' ]
r     = x[ , 'response' ]
fit   = x[ , 'fit' ];
dateLine = x[ , 'dateLine' ]
L = length(r)
error = var( e )
#  ga: the fit vs actuals
ylim = range( c( fit, r ) )
ylim [1] = 0.85*ylim[1]   # to accommodate new rug feature
# number of labels on x-axis should not exceed 15

ss =1:L 
xsq = seq( 1, L, L/15 ); 
xn=row.names( x)[ xsq ]

plot( dateLine, r  , type= 'l', ylim=ylim, axes=F, col='red',  lwd=lwd, 
         xlab  = '', ylab  =  "fit and response", xlim=range(ss)    ) 
lines( dateLine, fit, col='purple2', lwd=1.1 )
points( dateLine, fit,  col= 'blue', pch='.', cex=1.4 )
# x axis
axis( side = 1, labels=xn, at=xsq, cex=0.25, las=3, xlim=range(ss),  cex.axis=0.65, cex.lab=0.75   )   

      ysq=seq( min(r), max(r), ( max(r)-min(r) ) / 10 ) 
      yn = format( ysq, digits=4 );

axis( side = 2, labels=yn, at=ysq, cex.axis=0.65 )    # y axis 
 
# put a rug for abs res
absRes=abs(e)
lines( dateLine, y=0.92*ylim[1]+absRes, t='h', col='green2' )  
 # this manuever drops the err hist down further
pp = paste( response.name,  " in (red) vs fit (blue)", ' measure of error: ', format( error, digits=4 ), sep=' ' );
title( main = pp )           
box();

}

sheet <- gs_title( 'tickerSymbols' )
sp500 = gs_read_csv(sheet)   
sheet <- gs_title( 'colorLegend.csv' )
colorLegend = gs_read_csv(sheet)   

shinyServer(   function(input, output, session ) {
# define sp500 symbol table for stock history lookup
output$sp500Table = DT::renderDataTable( sp500, selection='single' )

observeEvent( input$setResponseBtn, {
rs = input$sp500Table_rows_selected 
updateTextInput(session, inputId='responseSymbol', value = as.character(  sp500 [ rs, 1 ]  )  )
}
)

observeEvent( input$setExplBtn1 , {
rs = as.integer( input$sp500Table_rows_selected )
updateTextInput(session, inputId='eVarSymbol1', value = as.character( sp500[ rs, 1 ]  )   )
}
)

observeEvent( input$setExplBtn2 , {
rs = as.integer( input$sp500Table_rows_selected )
updateTextInput(session, inputId='eVarSymbol2', value = as.character( sp500[ rs, 1 ]  )   )
}
)

response = eventReactive( input$responseSymbol, {
get.hist.quote(instrument = input$responseSymbol, start="2014-01-04" , quote = "Close" , provider = "yahoo", compression = "d", retclass='zoo' ) 
}  # end code chunk
)   # end fun													

# this translates arcane financial symbols into company names 
# wrt the components of a simple local model
transTable = reactive( {
      df = data.frame( matrix( 0 , nrow=3, ncol=3 )   )
      rn  =  c( 'response', 'expl. var. 1', 'expl. var. 2' )
 
    ind = match( x=input$responseSymbol , table=unlist( sp500[ , 1  ] )  )
      df[ 1 , 1 ] = rn[1]
      df[ 1 , 2 ] = as.character(  input$responseSymbol  )
      df[ 1 , 3 ] = as.character(  sp500 [ ind[1], 'Security' ]  )
     
     ind = match( x=input$eVarSymbol1 , table=unlist( sp500[ , 1  ] )  )
     df[ 2 ,  1 ] = rn[2]
     df[ 2 ,  2 ] = as.character(  input$eVarSymbol1 )
     df[ 2  , 3 ] = as.character(  sp500 [ ind[1], 'Security' ]    )                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        

     ind = match( x=input$eVarSymbol2 , table=unlist( sp500[ , 1  ] )  )
     df[ 3 ,  1 ] = rn[3]
     df[ 3 ,  2 ] = as.character(  input$eVarSymbol2  )
     df[ 3  , 3 ] = as.character(  sp500 [ ind[1], 'Security' ]   )   
    
     names( df ) = c( 'variable description', 'symbol',  'security name' )                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
      df   
}
)
							
output$modelTable = renderTable( {  
        transTable()
 }  # end chunk
)  # end fun     

eVar1 = eventReactive( input$eVarSymbol1, {
 get.hist.quote(instrument = input$eVarSymbol1, start="2014-01-04" , quote = "Close" , provider = "yahoo", compression = "d", retclass='zoo' ) 
}  #end code chunk
)   # end fun

eVar2 = eventReactive( input$eVarSymbol2, {
 get.hist.quote(instrument = input$eVarSymbol2, start="2014-01-04" , quote = "Close" , provider = "yahoo", compression = "d", retclass='zoo' ) 
}  #end code chunk
)   # end fun


# update input text values init 
observe( {
updateNumericInput(session, inputId='explPred1', value = format( mean( eVar1() ), digits=3  )   )
updateNumericInput(session, inputId='explPred2', value = format( mean( eVar2() ), digits=3  )   )
}
)

output$colorLegend <- renderTable( colorLegend )

stocks = reactive( {
dateLine = 1:length( response()  )
stocks = cbind( response(), eVar1(), eVar2(), dateLine )
xTab = transTable()   # translate symbols to names
names( stocks ) = c( as.vector( xTab[ , 3 ] ), 'dateLine'  )
return(stocks)
    } # end chunk
  )  # end react fun

x.lo = eventReactive( input$bldModel , 
 {
stk = stocks()
rc=dim( stk )[1]                            
rn = row.names( stk )    
cn = names( stk )
eVar.labels = paste( cn[2:3], collapse="+" )
ff<-as.formula(  paste( cn[1], '~', eVar.labels , sep="" )  )
lois<-loess( ff , data=stk, degree=2, span=0.20 )
df = data.frame(  response(), fitted(lois), resid(lois), dateLine=1:rc   )  
names( df )=c( 'response', 'fit', 'error', 'dateLine' )
return( df )
}   # obs evt wrapper chunk end
)    # obs evt fun end

brushDF.expr =  eventReactive( input$plot_brush, ignoreNULL=TRUE, { 

    if( is.null(input$plot_brush) ) return(NULL)
    if( length(input$plot_brush) < 2 ) return()
    if( any(is.na(input$plot_brush) )  ) return()
df = x.lo() 
cn = names(df)
x = brushedPoints( df=df, brush=input$plot_brush, xvar = 'dateLine', yvar = cn[1] )
return(  as.matrix( x[ , cn, drop=TRUE ]  )   )
}   # end chunk
)   # end reactive fun  

output$brushTable =  renderPrint( expr = {
if( is.null(input$plot_brush) ) return(' ')
if( is.null(brushDF.expr() ) ) return(NULL)
brushDf = brushDF.expr()
if( is.null(brushDf) || any(is.na(brushDf)) || length(brushDf)<=1 )
     return(' ')
brushDf
}   # end print of brush df
)    # end renderTable fun

output$supportMapPts <- renderRglwidget (
{    # begin handler code chunk 
stk = stocks()
kdSurface = KDSurface( x=stk[ , 2 ], y=stk[ , 3 ], era=200 , varNames=names(stk)[2:3], gridSize=40 )
d1=input$explPred1
d2=input$explPred2 
#ep1 = as.vector( stk[,2] ); 
#ep2 = as.vector( stk[,3] ); 
zm = max( as.vector( kdSurface$zden ) )
selPt = c( d1, d2, zm )
selPt = rbind( selPt, selPt )   # cannot do with zoo data.frames
selPt[ 2, 3 ] = 0
points3d( selPt[1,], col='orange3', size=6 )
segments3d( selPt, col='red2', lwd=6 )
text3d( selPt[1, ], text = 'the point selected to drive the forecast' , cex=0.6, col='black' )
rglwidget(  )

}    # end code chunk
)    # end obs event

observeEvent( input$StopApp, ignoreNULL=TRUE,  handlerExpr=  
{    stopApp();
} # end chunk
)  # end obs evt fun


observeEvent( input$submitForecast, ignoreNULL=TRUE,  handlerExpr=  
{
dd = stocks()
rn = row.names( dd )    
cn = names( dd )
eVar.labels = paste( cn[2:3], collapse="+" )
ff<-as.formula(  paste( cn[1], '~', eVar.labels , sep="" )  )
lois<-loess( ff , data=dd, degree=2, span=0.20 )
pred.dd = data.frame( input$explPred1,  input$explPred2 )
names(pred.dd) = cn[2:3]
loPred=predict( lois, newdata = pred.dd  , se=T  )
res= resid( lois )
arimaObj = auto.arima( res, max.p=4, max.q=3 ) 
f = res-arimaObj$residuals
arimaPred.obj = predict( arimaObj, ahead=1, se.fit = T )
#build df for trans to ui
Pred = matrix( 0, nrow=3, ncol=2)
Pred =  as.data.frame( Pred )
Pred[ 1, 1 ] = loPred$fit
Pred[ 1, 2 ] = loPred$se.fit
Pred[ 2, 1 ] = arimaPred.obj$pred[1]
Pred[ 2, 2 ] = arimaPred.obj$se[1]
Pred[ 3, 1  ] = sum( Pred[ 1:2, 1 ] )
Pred=cbind( c( 'Lois', 'Arima', 'Estimated Response -->' ), Pred )
colnames(Pred) = c( 'regression type', 'forecast value', 'se' )
rownames(Pred) = c( 'Lois', 'Arima', 'Estimated Response -->' )
output$Prediction=renderTable( Pred  )
}  # end obs chunk
)  # end obs to update forecast input var value 1 control

output$viewPort2D <- renderPlot (
{  # start interactive plot render
#  r = list()
lois = x.lo()
stk=stocks()
cn=names(stk)
PlotLois( x=lois, response.name=cn[1], lwd=1.25 )
} # end code chunk render                       
)    # end function render loFit 2D

output$viewPort3D <- renderRglwidget (
{   # begin code chunk
# color code change for brushed rows of the data.frame or plot
# invoke expr for brushedPts
# put all the brush edits and dd here
brushed=TRUE
if( is.null( input$plot_brush ) ) brushed=F 
if( brushed==T ){
dd = brushDF.expr()
if( is.null(dd) ) brushed=F
if( !( is.matrix(dd) || is.data.frame(dd ) )   )   brushed=F
if(   any(  is.na(dd)  )   ) brushed=F
if( dim(dd)[1]<1 ) brushed=F
if( dim(dd)[2]<1 ) brushed=F
  }

stks = stocks()
colorTime = rep( 'gray2', dim(stks)[1]  )
if( brushed == T ) {    # update selected spheres 
     brushRows = dd[ , 'dateLine' ]
     colorTime[ brushRows ] = 'orange2'
}

JLo.Pts( response = stks[ , 1 ], 
        pr1= stks[ , 2 ], colorTime=colorTime, 
        pr2= stks[ , 3 ],        
        response.name=names(stks)[1], 

        varNames=names(stks)[2:3], span=0.25, degree=2, gridSize=40  )
# this is an important kicker 
rglwidget(  )   

  }  # end of chunk render rglWidget df 
)    # end function render rglWidget
 

output$errDensity <- renderPlot (
{  # start  plot render 
lois = x.lo()
e =lois[ , 'error' ]
dd=density( e )
plot( dd$x, dd$y, t='l', col='red', xlab='', ylab=''  )
title( main='error density from loess regression',  
         xlab='error', ylab='probability', 
sub=paste( 'Mean Sq Residual: ', format( var( e ) ,digits=4), sep=' ' )  );
abline( v=0, lwd=1.5 )
}  # end render chunk
)   # end render function

output$trustRegionsMap <- renderPlot (
{  # start  plot render 
stks = stocks()
pr1=stks[ ,2 ]
pr2=stks[ ,3 ]
op <- KernSur(x=pr1,y=pr2, xgridsize=100, ygridsize=100, correlation=0,
xbandwidth=dpik(pr1), ybandwidth=dpik(pr2)   )
cn=names(stks)
image(op$xords, op$yords, op$zden, col=c('white','red','green1','blue','violet'), 
#  will not take hex strings     c(pureWhite,pureRed,pureGreen,pureBlue,pureViolet), 
axes=TRUE, xlab=cn[ 2 ], ylab=cn[3]  );
title( main='Loess Support Regions color coded by prob. density' )
}  # end render plot chunk
)   # end render function

output$loSupport3D <- renderRglwidget (
{   # begin code chunk  
stks = stocks()
 KDSurface( x=stks[, 2 ], y=stks[,3 ], era=200, varNames=names(stks)[2:3], gridSize=40 )
 rglwidget(  )
  }  # end of chunk  lois3D xxx
)   # end render function


output$residSmoothing <- renderPlot (
{  # start interactive plot render    
lois = x.lo()

res = lois[ , 'error' ]
tt=1:length(res)
plot( tt, res, t='l', col='red', xlab='', ylab='' ) 
titleTxt=paste( 'time-series of residuals (red) with a', input$smoother, 'smooth (blue)', sep=' ' )

# now regress on the residuals , res 
# run selected smoother
if( input$smoother == 'Lois' ) { 
regressionObj<-loess( res~tt ,degree=2, span=0.20,          
control=loess.control(surface="interpolate", trace.hat="approximate",  cell=.20 ) )
f=fitted(regressionObj)
e=res-f
}
else
if( input$smoother == 'Spline' ) {
regressionObj<-smooth.spline( tt , res )
f=fitted(regressionObj)
e=res-f
}
else
if( input$smoother == 'Arima' ) {
     regressionObj = auto.arima( res, max.p=4, max.q=3 ) 
      e = regressionObj$residuals
      e = as.vector( e )
      f = res-e
  
} 

lines( tt, f, col='blue', lwd=1.2 );
title( main=titleTxt,  
         xlab='time', ylab='residuals', 
sub=paste( 'Mean Sq Residual: ', format( var(e), digits=4 ), sep=' ' )  );     

} # end code chunk render                       
)    # end function render  residSmoothing


output$stepwiseRegression <- renderPlot (
{  # start interactive plot render 
lois = x.lo()
res  = lois[ , 'error' ] 
response = lois[ , 'response' ]
tt=1:length(res)
plot( tt,  response, t='l', col='red', xlab='', ylab='' ) 

# now regress on the residuals
# run selected smoother
if( input$smoother == 'Lois' ) { 
regressionObj<-loess( res~tt ,degree=2, span=0.20,          
control=loess.control(surface="interpolate", trace.hat="approximate",  cell=.20 ) )
f=fitted(regressionObj)
e=res-f
}
if( input$smoother == 'Spline' ) {
regressionObj<-smooth.spline( tt , res )
f=fitted(regressionObj)
e=res-f
}
if( input$smoother == 'Arima' ) {
     regressionObj = auto.arima( res, max.p=3, max.q=3 ) 
     f = res-regressionObj$residuals
     e = res-f
} 
 
lines( tt, lois[ , 'fit' ]+f, col='blue', lwd=1.2 );
titleTxt=
paste( 'time-series of response (red) vs loess + ', input$smoother, ' smoother on the residuals (in blue)', sep=' ' )
title( main=titleTxt, xlab='time', ylab='residuals', 
         sub=paste( 'Lois Arima Reduced Mean Sq Residual: ', 
         format( var( as.vector( e )  ) , digits=4 ), sep=' ' )  );     
} # end code chunk render                       
)    # end function render chained reg

observeEvent( input$closeIntro, ignoreNULL=TRUE,  handlerExpr=
{
updateCollapse(session, id='collapsePanels' , open = 'Variable Selection for Model', close = "< Introduction, a good place to start >", style = 'danger' )
}  # end chunk
)   # end obs evt

observeEvent( input$selEVars, ignoreNULL=TRUE,  handlerExpr=
{
updateCollapse(session, id='workHorse' , open = "< Model Accuracy >", close = '< Select Explanatory Variables >',  style = 'danger' )
HTML("intro text <a href='#< Trust Regions >'Go to Trust/a> intro text " )
}  # end chunk
)   # end obs evt

observeEvent( input$closeAccuracy, ignoreNULL=TRUE,  handlerExpr=
{
updateCollapse(session, id='workHorse' , open = '< Trust Regions >', close = "< Model Accuracy >", style = 'danger' )
}  # end chunk
)   # end obs evt

observeEvent( input$closeTrustPanel, ignoreNULL=TRUE,  handlerExpr=
{
updateCollapse(session, id='workHorse', 
                          open = '< Step Wise Regressions >', 
                          close = "< Trust Regions >", style = 'danger' )
}  # end chunk
)   # end obs evt

observeEvent( input$closeStepwisePanel, ignoreNULL=TRUE,  handlerExpr=
{
updateCollapse(session, id='workHorse' , open = "< Forecast >", close = "< Step Wise Regressions >", style = 'danger' )
}  # end chunk
)   # end obs evt

observeEvent( input$closeForecastPanel, ignoreNULL=TRUE,  handlerExpr=
{
updateCollapse(session, id='collapsePanels' , open = 'Variable Selection for Model', close = "< Forecast >", style = 'danger' )
updateRadioButtons(session, inputId='bldModel', label = NULL, choices = NULL,
selected = 0, inline = FALSE)
}  # end chunk
)   # end obs evt

}  # end server code
)   # end shiny server fun
