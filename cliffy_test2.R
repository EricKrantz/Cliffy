# Load packages
library(Rcpp)
library(viridis)
library(ggplot2)
library(dplyr)
library(RColorBrewer)


# output parameters
# output_width = 1920 * 4
# output_height = 1080 * 4


# C++ function to rapidly generate points
cliff_rcpp <- cppFunction(
  "
  NumericMatrix cliff(int nIter, double A, double B, double C, double D) { 
    NumericMatrix x(nIter, 2);
    for (int i=1; i < nIter; ++i) {
      x(i, 0) = sin(A * x(i - 1, 1)) + C * cos(A * x(i - 1, 0));
      x(i, 1) = sin(B * x(i - 1, 0)) + D * cos(B * x(i - 1, 1));
    }
    return x;
  }"
)

# C++ function to rapidly color points
color_rcpp <- cppFunction(
    "
  NumericVector nicecolor(NumericVector x, NumericVector y, int z) {
    NumericMatrix m(z, z);
    int n = x.size();
    int a;
    int b;
    NumericVector mycolor(x.size());
    for (int i=1; i<n; ++i) {
      a = x[i];
      b = y[i];
      ++m(a, b);
    }
    for (int i=1; i<n; ++i) {
      mycolor[i] = m(x[i], y[i]);
    }
    return mycolor;
  }"
)



# Output image directly to disk
#jpeg(
#  "clifford_attractor.jpg",
#  width = output_width,
#  height = output_height,
#  pointsize = 1,
#  bg = "black",
#  quality = 100
#)

N_points = 2000000
p_alpha = 0.1 #point transperancy

# Attractor parameters
params <- c(1.7, 1.7, 0.6, 1.2)

# Make the dataset
cliff_points <- cliff_rcpp(N_points, params[1], params[2], params[3], params[4])
points <- data.frame(cliff_points)
names(points) <- c('x', 'y')


maxx <- max(points$x)
minx <- min(points$x)
maxy <- max(points$y)
miny <- min(points$y)
xrange <- maxx - minx
yrange <- maxy - miny

n <- 600
points <- points %>% 
    mutate(newx = round((x - minx) / xrange * n, digits = 0) + 1,
           newy = round((y - miny) / yrange * n, digits = 0) + 1)
    

clrs <- color_rcpp(points$newx, points$newy, n)


points$color <- clrs
points <- points[-1, ]
ggplot(points, aes(x, y, color = color)) +
    geom_point(shape = ".", alpha = 0.1) +
    theme_void() +
    theme(legend.position = "none",
          panel.background = element_rect(fill = "black")) +
    scale_color_viridis(option = "magma", direction = -1) 
#    scale_color_distiller(type = "seq", palette = 'Blues', direction = -1)


# base R
cols <- brewer.pal(3, "Spectral")
pal <- colorRamp(cols)
points$newcolor <- points$color / max(points$color)
points$r <- pal(c(points$newcolor))[ , 1] / 255
points$g <- pal(c(points$newcolor))[ , 2] / 255
points$b <- pal(c(points$newcolor))[ , 3] / 255
points$rgb <- rgb(points$r, points$g, points$b, 0.02)
points$Col <- rbPal(50)[as.numeric(cut(points$newcolor, breaks = 50))]
par(bg = 'black')
plot(points$x, points$y, pch = ".", col = points$rgb)

