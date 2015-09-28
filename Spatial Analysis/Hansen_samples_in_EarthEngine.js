// Note: This code must be executed within the Google Earth Engine environment.
// An equivalent copy should be available at the following URI:
//      https://ee-api.appspot.com/09247ffe6633c856ad73dce9209ee80e

// Random seeds; must set every time executed for independent results
var PREV = 1;
var RUNS = 50;

////////////////////////////////
// Hansen Deforestation Product

// Displaying forest, loss, gain, and pixels where both loss and gain occur.
var gfcImage = ee.Image('UMD/hansen/global_forest_change_2013');

// Single band image with loss=1; gain=2; gain&loss=3
// Forest cover classes are cut at <50%, 50-80%, and >80% according to
//  Hansen, Potapov et al. (2013)
//  <50% : 4, 50-80% : 5, >80% : 6
var classStack = gfcImage.select('loss')
  .add(gfcImage.select('gain').multiply(2));

////////////////////////////////////////////////
// Add the fusion table as a feature collection

// These Fusion Tables are accessed through the unique identifiers provided
var vellendPts = ee.FeatureCollection('ft:1cZKSFMY1CHdnI4gUNd5smZ17aFVWzX7VP6dLT9I4');
var worldLandArea = ee.FeatureCollection('ft:1f2i26R4fXy3VLDSYEi_pd8Zncq8n5gaVPH2Z5V_p');
//var worldLandAreaFine = ee.FeatureCollection('ft:1wNqdLzgAireuFJsm0sjIRyZ_mN18NFauXawHmwYx');

////////////////////////////
// Add buffer around points

/*
  A function to buffer the geometry of sample points.
 */
var buffFunc = function (feature) {
    return feature.buffer(200); // 200 meters
};

/*
  A function to make a histogram sample from the imagery.
 */
var makeHistogram = function (seed) {
  // Maximum error 30 meters (last argument to randomPoints()
  var samples = ee.FeatureCollection.randomPoints(worldLandArea, 346, seed);
  var samplesBuffered = samples.map(buffFunc);
  var histo = classStack.reduceRegion(ee.Reducer.histogram(4, 1),
    samplesBuffered, 5100);

  histo = ee.Dictionary(histo.get('loss'));
  // Package the histogram values as a Feature.
  return ee.Feature(null, {
    keys: histo.get('bucketMeans'),
    counts: histo.get('histogram')
  });
};

// This does the heavy lifting
var runs = ee.List.sequence(PREV, RUNS).map(makeHistogram);

// Run the function on  the Vellend pts.
var vellendBuffered = vellendPts.map(buffFunc);
var histo = classStack.reduceRegion(ee.Reducer.histogram(4, 1), vellendBuffered);
histo = ee.Dictionary(histo.get('loss'));
Export.table(ee.FeatureCollection(ee.Feature(null, {
    keys: histo.get('bucketMeans'),
    counts: histo.get('histogram')
  })),
  'Vellend_histogram_output', {
    driveFolder: 'EarthEngine',
    driveFileNamePrefix: 'Hansen_Vellend_pts_Loss_Gain_Gain-Loss',
    fileFormat: 'csv'
})

//////////////////
// Exporting Data

Export.table(ee.FeatureCollection(runs),
  'Test_histogram_output', {
  driveFolder: 'EarthEngine',
  driveFileNamePrefix: 'Hansen_Loss_Gain_Gain-Loss',
  fileFormat: 'csv'
});
