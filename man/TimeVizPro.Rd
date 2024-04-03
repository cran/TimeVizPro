\name{TimeVizPro}
\alias{TimeVizPro}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Dynamic Data Explorer:Visualize and Forecast with TimeVizPro
}
\description{
Unleash the power of time-series data visualization with ease using our package. 
 Designed with simplicity in mind, it offers three key features through the Shiny package. 
 The first output shows time-series charts with forecasts, allowing users to visualize trends 
 and changes effortlessly. The second one presents averages per country presented in tables 
 with accompanying sparklines, providing a quick and attractive overview of the data. 
 The last one displayed a customizable world map colored based on user-defined variables 
 for any chosen number of countries, offering an advanced visual approach to understanding 
 geographical data distributions.
}
\usage{
TimeVizPro(Edata, v1,v2,v3,v4,headr = "GPD", Number=32,Fyear=1990)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Edata}{
an input data.
}
  \item{v1}{
country name column in the input data.
}
  \item{v2}{
year column in the input data.
}
  \item{v3}{
the map coloring variable, present in the input data.
}
  \item{v4}{
the three-letter country code for each country variable in the input data.
}
  \item{headr}{
plot title.
}
  \item{Number}{
the number of real data.
}
  \item{Fyear}{
start date of data
}
}
\details{
The input data for this program must have four specific columns:

Full country name: This column should contain the full name of each country, as recognized by international standards.

Year: This column should contain the year for which the data is being presented.

Variable to color the map: This column should contain the numerical values that will be used to color the map.

Three-letter country code: This column should contain the three-letter country code for each country, as defined by ISO 3166-1 alpha-3.

The four columns are necessary to ensure that the program can correctly interpret the data and generate the desired output.

The full country name is used to identify the country on the map.
The year is used to filter the data to the specific year of interest.
The variable to color the map is used to assign colors to different countries on the map.
The three-letter country code is used to ensure that the data is correctly associated with the correct country.
}
\value{
Shiny app including three tabs.
}

\author{
Hossein Hassani and Fernando Cantu Bazaldua and Leila Marvian Mashhad 
}

\examples{
if(interactive()){
d <- read.xlsx("TimeVizPro_1.0.tar.gz/TimeVizPro/inst/extdata/Example.xlsx")

TimeVizPro(d,v1=d$desc,v2=d$year,v3=d$GDPv,v4=d$iso3)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
