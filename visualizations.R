mandelbrot1 <- function(outputfilepath = "c:/temp/mandelbrot1.png")
{
        # Thanks to http://users.utu.fi/attenka/mandelbrot_set.R
    
    ####################
    ## MANDELBROT SET ##
    ####################
    
    Limits=c(-2,0.8)
    MaxIter=25
    cl=colours()
    Step=seq(Limits[1],Limits[2],by=0.0025)
    S=floor(length(cl)/MaxIter)
    Dist=0
    PointsMatrix=array(0,dim=c(length(Step)*length(Step),3))
    t=0
    
    
    for(a in Step)
    {
        for(b in Step+0.6)
        {
            x=0;y=0;n=0;Dist=0
            while(n<MaxIter & Dist<4)
            {
                n=n+1
                newx=a+x^2-y^2
                newy=b+2*x*y
                Dist=newx^2+newy^2
                x=newx;y=newy
            }
            if(Dist<4) colour=24 # black colour
            else colour=n*S
            t=t+1
            PointsMatrix[t,]=c(a,b,colour)
        }
    }
    
    png(filename = outputfilepath)
    plot(PointsMatrix[,1], PointsMatrix[,2], xlim=Limits, ylim=Limits+0.6, col=cl[PointsMatrix[,3]], pch=".", xaxt='n', yaxt='n', ann=FALSE)
    dev.off()
}

mandelbrot2 <- function(outputfilepath = "c:/temp/mandelbrot2.gif")
{
    #thanks to https://www.r-bloggers.com/mandelbrot-set-evolved/
    
    #library("devtools")
    #install_github('konoanalytics/KonostdlibR')
    library("KonostdlibR")
    
    loadpackage("caTools")  # external package providing write.gif function
    
    jet.colors = colorRampPalette(c("#00007F", "green", "#007FFF", "gold", "#7FFF7F", 
                                    "white", "#FF7F00", "purple", "#7F0000")) 
    m = 600     # define size
    C = complex( real=rep(seq(-1.8,0.6, length.out=m), each=m ), 
                 imag=rep(seq(-1.2,1.2, length.out=m), m ) ) 
    C = matrix(C,m,m)  # reshape as square matrix of complex numbers
    Z = 0     # initialize Z to zero
    X = array(0, c(m,m,50)) # initialize output 3D array
    for (k in 1:50) {  # loop with 50 iterations
        Z = Z^2+C    # the central difference equation 
        X[,,k] = exp(-abs(Z)) # capture results
    } 
    write.gif(X, outputfilepath, col=jet.colors, delay=10)
}