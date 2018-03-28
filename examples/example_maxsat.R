# An Example for Testing the Behavior of the Standard Models on Some Example Data
#
# The example data stems from an experiment with a m-flip hill climber on a MAX-SAT instance

# first we load the data
data   <- read.csv("examples/example_maxsat_uf225-02_mFlipHCrs_01.txt", sep="\t");
data.x <- data[[1]];
data.y <- data[[3]];

# make the default plot
plot(log(data.x), data.y);

# then we load the models and assign colors to them
models  <- regressoR.functional.models::FunctionalModel.all();
count   <- length(models);
colors  <- c("red", "blue", "green", "orange", "darkgreen", "violet",
             "cyan", "gold", "darkblue", "darkred", "darkmagenta",
             "aquamarine", "tan");
if(length(colors) < count) {
  colors <- unique(unlist(c(colors, palette(rainbow(count)))));
  if(length(colors) > count) {
    colors <- colors[1:count];
  }
}

# prepare the metric
metric  <- regressoR.quality::RegressionQualityMetric.default(data.x, data.y);

# add the lines
for(j in 1L:50L) {
  for(i in 1L:count) {
    result <- regressoR.functional::FunctionalModel.fit(metric, models[[i]]);
    if(!(is.null(result))) {
      lines(log(data.x), result@f(data.x), col=colors[[i]]);
    }
  }
}
