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

shinyUI(

fluidPage( 

tags$style(type='text/css', ".panel-heading { color: blue; font-weight: bold; font-size: 14px; line-height: 14px;} .panel-title { color: blue; font-weight: bold; font-size: 14px; line-height: 14px; }")
,  # ,
tags$style(type='text/css', ".action-button { color: red; border: green; } .button-title { color: blue; font-weight: bold; font-size: 12px; line-height: 12px; }")
,  # ,
tags$style(type='text/css', ".selectize-input { font-size: 10px; line-height: 10px;} .selectize-dropdown { font-size: 10px; line-height: 10px; }")
,  # ,""
tags$style(type='text/css', ".control-label { color: green2; font-size: 12px; line-height: 12px;}" )

,  # ,"control-label"

mainPanel( 
bsCollapse(  id='collapsePanels', multiple=TRUE,  
                    open="< Introduction, a good place to start >", 
bsCollapsePanel(   "< Introduction, a good place to start >", 
h6('Lois is an interactive completely visualized version of loess. And loess is a mouthful, a local, non-linear, non-parametric robust regression and more... it comes with R.')
,  # ,  
h6('Your Mission: Create a predictive model with Lois. This demo will constrain your selection to only 2 explanatory variables.' )
,  # , 
h6( 'Panel Navigation: ' )  
,  # ,
h6( 'For now, click on the tabs in sequence. Follow the blue tabs down the screen.' )
,  # ,
h6( 'As you become familiar with Lois, Wander at Leisure: Manually, toggle off the currently selected tab again . Then, advance to the next tab.' )
,  # ,
h6(p(' if you do not like leaving a trail of open panels, scroll back to the top of the panel and toggle off this panel. Or, use the red Close buttons at the bottom of each panel.'  )   )  #  end of h6  
,  # ,
h6( em('While the red close button will automatically close the current panel and open the next, there is a problem:  this widget has a nasty habit of leaving you at the bottom of this new panel so you must scroll back up to the top. Note: The mouse wheel is enabled for both scrolling and zooming, for ease of use.' )  ) 
,  # ,
actionButton( inputId='closeIntro', label='< close introduction and proceed > ' )

)  # end of well pan

,  #  ,
bsCollapsePanel( title='Variable Selection for Model',  
   h5( 'Change Model or Build Model' )
,  # ,
         radioButtons( input='bldModel', label='Action: Change Model or Build Model', 
                              choices = c( 'select new variables'=0,  'build predictive model'=1),
                              selected=0 )
,  # , 
        h6( 'selected variables for model construction' )
,  # ,
       textInput(inputId='responseSymbol', label='response', value = "ATVI", width = NULL, placeholder = NULL)
,  # ,
       textInput(inputId='eVarSymbol1', label='explanatory variable 1', value = "AKAM", width = NULL, placeholder = NULL)
,  # ,
       textInput(inputId='eVarSymbol2', label='explanatory variable 2', value = "ADBE
", width = NULL, placeholder = NULL)
,  # ,
    tableOutput(  'modelTable'   )
, # ,
h5( 'here are some stock symbols from the SP500' )

,  #  ,
actionButton( inputId = 'setResponseBtn', label='Set current row to response' )
,  # ,
actionButton( inputId = 'setExplBtn1', 
                       label='Set current row to an explanatory variable 1' )
,  # ,
actionButton( inputId = 'setExplBtn2', 
                       label='Set current row to an explanatory variable 2' )
,  # ,
textOutput( 'msg' )
,  # ,

h6( em('you can sort this long table by column; you can search by company name' )  )
, # ,
         DT::dataTableOutput( "sp500Table" ) 

    )  # end panel
,  # ,
bsCollapsePanel( title='Stop App',
actionButton( inputId = 'StopApp', label='Stop Application' )
)      # end coll pan
,      #,
bsCollapsePanel( title='Contact Info',
h5( 'please direct comments and suggestions to: ' )
,   # ,
#tags$address( href="mailto: lloyd.relaxed@gmail.com"    )  # end of html fun
h5( "mailto: lloyd.relaxed@gmail.com" )  
)      # end coll pan

  )    # end bsCollapse  set header
,  # ,
conditionalPanel( 'input.bldModel==1',
bsCollapse(  id='workHorse', multiple=TRUE,  
                    open="< Model Accuracy >", 
bsCollapsePanel( title="< Model Accuracy >", 
h6("This panel compares the model's ability to predict the response (fitting the response)
 using the past closing prices and past econometric indicators."),
h6( 'brushing: click the mouse (cursor is in the shape of a cross) in the plot and drag, you should see a gold panel, highlighting your selection' ), 
      plotOutput( 'viewPort2D',  
        brush = brushOpts(  id = 'plot_brush', fill='#EE9A00', opacity = 0.45, direction='x', resetOnNew=TRUE )  )

,  # concat, 
h6(
 'The blue line (the lois estimate) should closely resemble the red line (the response). To emphasize this resemblance between forecast curve and actual time series of the response, I have included a rug (in green2) at the bottom of the chart.  Each vertical line represents the absolute residual error between the lois model and the response on a particular date. Higher lines correspond to a higher absolute error between the red and blue curves. By brushing, select regions of high error on the 2D graph, see how the 3D plot responds. Conversely, try brushing regions with a good fit, i.e. low error.' )
,  #
h6( p( 'When you brush the comparison 2D plot ( brush region highlighted in gold ), the selected points will appear as golden spheres on the 3D plot. Beneath the 3D plot, the brushed data.frame will appear in spreadsheet format.') )
,  #
h6( em('errata: the gold brush will hang on this panel until you brush again. Yes,  resetOnNew=TRUE, and no i have not learned shinyjs, yet. Be patient.')  )
,  #
    
h6( p('Pls Scroll down to the 3D plot. Lois (loess) generated this surface. The colors indicate the level of support, with purple indicating the most support in a local context and white (or gray) being the least. The axis labels should match your selection from the Select Explanatory Variables  panel. The spheres represent the raw data with the 2 explanatory variables forming the floor and grid of the plot. The z-axis represents the response. Hence, the loess estimate produces a stimulus-response surface with the past pricing providing the stimulus to which the closing price of the response will be the approximate response, hopefully.' ))
,  # , 
      rglwidgetOutput("viewPort3D")
 ,  # ,  
h6( p('This 3D plot is interactive. Grab it, rotate it freely. The mouse wheel is enabled for zooming.  Zoom in. How far do spheres seem to float above and below the surface? Are there regions where the spheres are closer to the surface than others?  A good 3D measure of accuracy for your model: most of the spheres should be embedded in the surface; please zoom in at diverse angles in order to examine the lois surface and its point cloud. Look for knots and clusters of spheres, these form trust regions for loess, meaning loess will operate more effectively in regions with more data locally. Because loess is locally non-linear in this demo, areas of low support will generate wildly divergent surfaces.' ) ) 
,  # ,
h6( p('Which regions are linear, and which non-linear? Estimating non-linear processes cost many, many more degrees of freedom and system time. Non-linear models are also much more difficult to interpret. However, when called for, they are extremely effective. Like now.' ) )

,  # ,  
actionButton("showBrush", "show brushed data")
,  # ,
             bsModal( id="showBrushModal", title="Brushed Data Table", trigger="showBrush", 
                      style = "info", size = "small",     
             verbatimTextOutput( "brushTable" )
                             )  # end modal 
,  # ,
actionButton( inputId='closeAccuracy', label='< < Close Model Accuracy Panel > >' ) 
) # end of coll.pan.
, # ,
bsCollapsePanel( title='< Trust Regions >',  
h6('local support and trust regions' )
,  # ,
h6( 'loess (lois) forecasts depend on nearby past data for support. Consider the value of data from sources nearby (locality) as opposed to the weight you lend data from sources far far away in space and time. For example, let us say that you want to predict the closing price  tomorrow. Even with an econometric and financial database going back 20 years, you would still lend more weight to recent data. Note: the word recent means local in a temporal sense. If you wanted a reliable review for a plumber would you ask your neighbors or trust the internet? Any forecast without much data to support it should not receive much of your confidence. And you should usually give data from nearby sources more weight. This weight can be expressed as a density or trust regions, an indicator of sufficient nearby data. This package delivers a 2D and 3D perspective on confidence regions by color with the color legend at the bottom of this panel')
,  # ,
                   plotOutput( 'trustRegionsMap' )
,  # ,
h6( 'below are the trust regions in 3D. If you flip over this plot you will see the trustRegion Color Map at the bottom, The highest, purple peaks (or valleys, depending on perspective) occur where the knots and clusters also occur in the goodness of fit panel.'), 
h6('this graph can be rotated freely with the mouse. You can zoom with the mouse wheel.')
,  # ,
                   rglwidgetOutput( 'loSupport3D' )
,  # ,
h6('here is a legend relating level of confidence with color')
,  # ,  
actionButton("showLegend", "Show Color Legend")
,  # ,
     bsModal( id="showLegendModal", title="Color Legend", trigger="showLegend", 
                      style = "info", size = "small",     
                 tableOutput( 'colorLegend' )
                             )  # end modal                
,  # ,
actionButton( inputId='closeTrustPanel', label='< < Close Trust Panel > >' ) 
 )  # end coll pan
,   #  ,                 
 bsCollapsePanel( title='< Step Wise Regressions >',  
  h6('stepwise regression and chained simulations')
,  # ,
  radioButtons(inputId='smoother', label='select a residual smoother', 
                                     choices=list('Lois'='Lois', 'Spline'='Spline', 'Arima'='Arima' ),
                                     selected='Arima' ),
       bsTooltip("smoother", "These 3 smoothers fit the residuals(errors) over time, with lois and spline being non-linear, non-parametric and local",   "right", options = list(container = "body"))
  ,  # concat: , 
h6( 'this plot shows the probability density of the errors arising from the mismatch of a model prediction against the response. The more the density clusters around zero the better' ) 
,   # , 
                   plotOutput( 'errDensity' ),
h6( 'your selected secondary residual smoother will now attempt to model the lois residuals (errors arising from the difference between the response and the predictions arising from your lois model). Below, we see the results with the actual error in red and the model prediction in blue.'), 
                   plotOutput( 'residSmoothing' ), 
h6('the stepwise regression summary: first we applied lois with only two explanatory variables. This yielded a model estimate (which hopefully fits the response );  next, 2 local and one global smoother attempt to reduce these errors with a second regression. Note: the error scalar in the subTitle at the bottom displays the total error remaining after the application of this smoother. For empirical and theoretical reasons, you cannot reverse the order of these two powerful regressions.')
, # ,                 
  plotOutput( 'stepwiseRegression' ), 
h6('hopefully, you will see a dramatic decrease in the standard error')
,  # ,
actionButton( inputId='closeStepwisePanel', label='< < Close Stepwise Regression Panel > >' ) 

)    # end of coll pan
,    # ,
bsCollapsePanel( title="< Forecast >",

    numericInput(inputId='explPred1', 
                           label='Enter value for the first explanatory variable: ',
                           value=0, step=0.250, width = NULL ) 
,   # ,
    numericInput(inputId='explPred2', 
                           label='Enter value for the first explanatory variable: ',
                           value=0,  step = 0.250, width = NULL ) 
,  # ,
actionButton( inputId='submitForecast', label='< Submit Forecast >' ) 
,  # , 
    tableOutput( 'Prediction' )
,   #  ,
h6( 'Are these new values for the two explanatory variables in a high trust region?' ) 
,   # ,
                   rglwidgetOutput( 'supportMapPts' )
,   # ,
    h6('the algebraic total of rows 1 and 2 should yield the prediction. The se stands for the standard error of the residuals. A wider confidence interval merits less confidence in the estimates. Ordinarily, confidence intervals, based on se, are assumed to be drawn from a normal density')
,  # ,
actionButton( inputId='closeForecastPanel', label='< Close Forecast Panel >' ) 
)    # end of coll pan

)    # end of co panel

   )    # end bsCollapse  set header

)    # end of main panel
)   # end of fluid page

)   # end UI
