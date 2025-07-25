# Import required packages
library(openeo)

# Connect to the back-end
connection = connect(host = "https://openeofed.dataspace.copernicus.eu")
# ToDo: Authentication with login()

p = processes()

load1 = p$load_collection(id = "COPERNICUS_VEGETATION_INDICES", spatial_extent = list("west" = 6.417643832909436, "east" = 20.163373036586588, "south" = 42.503401788916364, "north" = 51.09254148400274), temporal_extent = list("2017-01-01T00:00:00Z", NULL), bands = list("NDVI"))

reducer1 = function(data, context = NULL) {
  count1 = p$count(data = data)
  quantiles1 = p$quantiles(data = list("data", data), probabilities = list(0.95))
  return(quantiles1)
}
aggregate2 = p$aggregate_temporal(data = load1, intervals = list(list("2022-07-01T00:00:00Z", NULL)), reducer = reducer1)
save3 = p$save_result(format = "GTIFF", data = aggregate2)

# The process can be executed synchronously (see below), as batch job or as web service now
result = compute_result(graph = save3)
