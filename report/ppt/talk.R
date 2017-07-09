Page 3 = {
    
    There are two main packages to support the graphics system in R. 
    The first one is the graphics. It provides the original GRZ graphics system from S. It is fast, and many other R packages build on top of it, for example, the plotrix and maps.
    The second one is the grid package. It is newer and more flexible compare with the graphics package. However, it is atcually slower. Also, there are many packages build at the top of it, for examkple ,the lattice and ggplot2.
    
}

Page 4 = {
    There is an other package, called gridgraphics package, which allows us to convert at plot drawn by graphics package to a identical plot drawn by grid. The package has a main function, called grid.echo().
	 allowing the plot to be manipulated using all of the tools available in grid
}

Page 5 = {
    Here is the picture shows the connection between graphics and grid. There is no way for a graphics plot to grid plot, expect the gridGraphics.
}

Page 6 = {
    Here is the scatter plot for the cars dataset. Where the x is the speed and y is the distance.
    The left plot is drawn by calling the plot function.
    Then the following codes is the example to redraw this scatter plot on grid, by using the grid.echo() on the gridGraphics.
    The left plot is drawn by graphics and the right plot is redrawn on grid by using grid.echo().
    These two plots are identical to each other.
}

Page 7~8 = {
    gridGraphics can emulate most of the graphics-plot. Howerer, there are two kinds of plot which the older version cannot emulate. The first one is the filled.contour plot, which for drawing the level-contour plot. And the second is the persp() plot, for drawing the 3-D surface. Here is the example, if we try the emulate the filled.contour() plot, then the result will almost be a blank page.
    And similar for the persp() plot
}


Page 9~10 = {
    Any plot been drawn by R can be saved as an R object, or the graphics display list, by using the recordPlot(). This object will act as recording all the inputs when calling the graphics functions. for example, the inputs for calling C_Plot_new, intpus for calling C_plotXY when drawing a scatter plot and so on.
    Then, the gridGraphics will access this R object, then converting a graphics plot to a grid plot by using the necessary information.
}

Page 11 = {
    Back to the pervious scatter plot example, it is possible to get the information of the plot.
    Here is the function call for drawing this plot, C_plot_new, for creating a new plot;
    palette2 for setting up the colors
    C_plot_window for creating a plot window
    C_plotXY for drawing the points
    C_axis for drawing both x and y axes.
    C_box for drawing a box
    C_title for creating the title.
}


Page 12 = {
    In more detail, here it the C function for the C_plot_new in the graphics package, for creating a new plot.
}

Page 13 = {
    There is the R function to provides the same action for the C_plot_new on grid. Although they will do the simliar jobs, but the code structure will be compeletely different.
}

Page 15 = {
    Since the persp() plot and filled.contour() plot is structured under the C codes. Therefore we need to understand the C code and reproduce the plot on R. Althought some of the C code is very simliar to R code, but some of them are quite different.
    Here is a part of the C code, which use for checking the limit is invalid or not.
    So, within the function, it will checking wheather the limit passing into the function is invalad or not, then it will return either 0 or 1. But also, it will modify the xc and xs as well. 
    &xc and &xs are the memory address, which their value will be modified in here.
    Therefore two action will happend within the function. checking the limit and modifying the xc and xs.
}

Page 16 = {
    Since R does not have the pointer structure, therefore we need to do it on a different way.
    Instead of checking the condiction outside the function, I do the condiction checking insider the function. so, inside the function if limit is not invalid, then stopped.
    Moreover, I assigned the value for the varuables xs and xc outside the function.
}

Page 17 = {
    Here it the C code for producting the persp() plot and filled.contour() plot. Actually, it is noly 8 pages out of 64 pages in total.
}

Page 19 = {
    Sometimes we need to make sure that the result that R code produce will be identical to the C code. In taht case we need to translate the C code directly.
    this example shows that I drawn two dotted lines, one from left to right and the other from right to left. and this two lines are not the same. 
    Hence it is necessary to do the directed translation, even we think that drawing a line from left to right is identical to drawing a line from right to left.\
    
    So, why not just coty? One major reason is that different programming will have different advantages and disadvantage. For exmaple, C is fimliar with for loops, but R is very slow.
}

Page 20 = {
    Here is the filled contour plot for the topography of maunga whau. within this plot, there are at most 100,0000 ploygons that we need to consider. 
    In the C code, it will looping from 1 to the total number of polygons, which is 100,0000 interaction in total.
    if we are writting a for loop version as the C code does for producing this plot, it will slow down the R, therefore it is necessary to optimize the codes to make it faster.
}

Page 21 = {
    Here is the timming for different metohd to reproduce the plot.
    I used the system.time to do the timming. By using the for loop version, it takes more than 10 seconds, but with the optimize version, or the vectorization version, it takes less than 2 seconds, which is much faster.
}

Page 22 = {
    In order to make sure that the plot that we reproduces on grid is idential to the plot been drawn by using graphics, we need to check the identity of these two plot. However, it is not a good idea by just looking these plots by our eyes because it is very slow and sometimes our eyes are not reliable.
}

Page 23 = {
    Here is two persp plot of the sinc surface, does anyone find any difference between these two plots?
}

Page 24 = {
    And here is the answer, the color on red indicates the difference.
    (back one slide)
    the color on red is very easy to see. However, the reset difference is very difficult to tell, even now you knew the difference, but they are stil looks identical.
    (forward one slide)
    because there are only one pixcel different on the blue channel.
    therefore, it is necessary to use the software to support to the identity test.
    The difference dected is using the ImageMagick.
}

Page 26 = {
    Here is the solution to the project. 
    Here is the torus plot drawing by persp()
    And here is the grid.echo()
    And, not difference dected
}

Page 27 = {
    Here is the filled.contour plot in the previous example
    And here is the grid.echo()
    And, not difference dected again
} 


Page 28~29 = {
    The reason for translating a graphics plot into grid plot is that grid with more flexibility than graphics. And here is an example.
    I want to draw two plots within one page, where the left one i want to draw the shape of the volcano and I want to draw the level of the volcano on the right.
    So, I am going to use the mforw() to split the page. then I draw a persp() on the left, and everything is fine.
    But, when I draw the filled.contour() it will broken, because the filled.contour() will reset the mforw, or the page within the function call. therefore it is difficult to do that on graphics.
    However, its possible to do it on grid.
    The solution will be creating a new grid Page, then push a viewport at left, then draw the shape of the volcano. Then up to the root viewport by using upViewort(). Then push a viewport on the right, draw the filled.contour by using grid.echo(), then up the root viewport. And finall, add a title to the plot.
} 

Page 30 = {
    A more advance example will be to export the R plot to svg plot. A svg plot can been interacting by the javascript, for example, its possible to creat the animation with a svg plot.
    The surface() will draw the sinc surface, and addFeatures() will creating the top-left pannel and some hidden object, which I will show it later.
    Then I connect a javascript to this plot, then finally I export it as a svg image.
}

Page extra = {
    Here is the svg image that I exported. with this image, it is possbile for doing a animation for changing the color. And here is the action.
    Also, it is also possible to change the opacity of the surface.
    Even more, I can shade this surface, with animation.
    And finally, I can heightlight the fragments of this surface and display the z-coordinate.
    
    if(moretime == TRUE)
    {
        The animation been create by changing the color within in a very small time interval.
        So, when the action been active, it will get all the colors for all polygons. then every time changing the colors to be a little close to the red. The loop will stopped until the color of the surface is red.
        And simliar to the opacity and the shading.
    }
    
}


## Important

page 10 = {
    + every plot drawn in R can be saved as an R object, by using the recordPlot().
    + This object is called the graphics-display-list
    + within the display list, you can see that the information of the C function will be recorded
    + example...
    + what gridGraphics dose is that it will pull out all the useful information.
    + then use this information to provides an identical plot on grid.
}

page 14~16 = {
	+ Since the persp plot and filled contoru plot is structured under the C code
	+ therefore if we want to emulate these two kinds of plot
	+ we need understand the structure of the C code
	+ One major difference between C and R is that the C code has the pointer data structure
	+ But R doesn't

    + function for checking the limit of the persp plot
    + within the function, it will check the condiction
    + if false, then return an error message
    + if true, then continuous to do the calculation
    + within the calculation, it will modify the xc and xs
    + where xc and xs are the memory address
    
    + instead of return an error message outside the funciton, I do it inside the function
    + instead of modified the xs and xs within the function, I do the assignment outsider the function
    
    + simliar jobs but different steps
}

page 22 = {
    + The reason for doing the testing is to make sure the plot drawn by grid will be identical to the plot drawn by graphics
    + It is not a good idea to to the test by our eyes because 
    + it is waste time and it is difficult to distignish some tiny difference 
    + therefore, we need a software to support us to do the testing
}