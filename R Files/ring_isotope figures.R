	
	
#### Isotope 13C precip figure	
# Plot the y1 data  
	2.	par(oma=c(2,2,2,4))               # Set outer margin areas (only necessary in order to plot extra y-axis)  
	3.	  
	4.	plot(ti, y1,                      # Data to plot - x, y  
	5.	     type="b",                    # Plot lines and points. Use "p" for points only, "l" for lines only  
	6.	     main="Time series plot",     # Main title for the plot  
	7.	     xlab="Time",                 # Label for the x-axis  
	8.	     ylab="Response (y1 & y2)",   # Label for the y-axis  
	9.	     font.lab=2,                  # Font to use for the axis labels: 1=plain text, 2=bold, 3=italic, 4=bold italic  
	10.	     ylim=c(0,20),                # Range for the y-axis; "xlim" does same for x-axis  
	11.	     xaxp=c(0,50,5),              # X-axis min, max and number of intervals; "yaxp" does same for y-axis  
	12.	     bty="l")                     # Box around plot to contain only left and lower lines  
	13.	  
	14.	# Add y2 data to the same plot  
	15.	points(ti, y2,  
	16.	       type="b",                  # Plot lines and points  
	17.	       lty=1,                     # Line type: 0=blank, 1=solid, 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash  
	18.	       lwd=1,                     # Line width  
	19.	       pch=19,                    # Point type: pch=19 - solid circle, pch=20 - bullet (smaller circle), pch=21 - circle, pch=22 - square, pch=23 - diamond, pch=24 - triangle point-up, pch=25 - triangle point down.  
	20.	       col="red")                 # Color of the plotted data  
	21.	  
	22.	# Add y3 data to the same plot, but on a different axis  
	23.	par(new=T,                        # The next high-level plotting command (actually plot.new) should not clean the frame before drawing ìas if it was on a new deviceî.  
	24.	    oma=c(2,2,2,4))               # Increase the size of the outer margins to accomodate second y axis  
	25.	  
	26.	plot(ti, y3,  
	27.	       yaxt="n",                  # Do not plot the y-axis  
	28.	       ylab="",                   # Do not plot the y-axis label  
	29.	       xlab="",                   # Do not plot the x-axis label  
	30.	       type="b",                  # Plot lines and points  
	31.	       lty=1,                     # Line type: 0=blank, 1=solid, 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash  
	32.	       lwd=1,                     # Line width  
	33.	       pch=19,                    # Point type: pch=19 - solid circle, pch=20 - bullet (smaller circle), pch=21 - circle, pch=22 - square, pch=23 - diamond, pch=24 - triangle point-up, pch=25 - triangle point down.  
	34.	       col="blue")                # Color of the plotted data  
	35.	  
	36.	axis(4,                           # Add a second axis: 1=below, 2=left, 3=above and 4=right  
	37.	    pretty(range(y3),10))         # Intervals for the second y-axis  
	38.	  
	39.	mtext("Response (y3)",            # Add second y-axis label  
	40.	      side=4,                     # Add to right hand side of plot  
	41.	      line=3,                     # Add to line 3 from the margin  
	42.	      font=2)                     # Print label in bold  
	43.	  
	44.	# Add a legend to the plot  
	45.	legend("topleft",                       # x-y coordinates for location of the legend  
	46.	       legend=c("y1", "y2", "y3"),      # Legend labels  
	47.	       col=c("black", "red", "blue"),   # Color of points or lines  
	48.	       pch=c(21,19,19),                 # Point type  
	49.	       lty=c(1,1,1),                    # Line type  
	50.	       lwd=c(1,1,1),                    # Line width  
	51.	       title="Time series")             # Legend title  
	52.	  