# Assignment 2: Estimate the melting point of Cerrolodium using calibrated measurements from a new sensor.
# Seth Kurtenbach

fm = fopen("A2-MeasurementData.bin");
fc = fopen("A2-CalibrationData.bin");

measurements = fread(fm, Inf, "float");
calibrations = fread(fc, Inf, "float");

meanC = (mean(calibrations));
printf("calibrations mean: %f\n", meanC);

bias = (31.006277) - meanC;

for i = 1:length(calibrations)
    newcals(i) = calibrations(i) + bias;
end

stddev = stderr(newcals);
printf("stddev: %f\n", stddev);





#meanCal = (mean(calibrations));
#printf("meanCal = %f\n", meanCal);
# meanCal = 30.770567



#bias = (31.006277) - meanCal;
# bias = 0.235710

#printf( "bias = %f\n", bias);

#estimate = (mean(measurements)) + bias;
# estimated melt point = 20.085607

#printf ("estimate = %f\n", estimate);

#for i = 1:length(calibrations)
#    errorCal(i) = (calibrations(i) + bias) - (meanCal + bias);
#    stdevCal = stderr(errorCal);
#end
#printf("stand dev Cal: %f\n", stdevCal);
#for i = 1:length(measurements) 
#    error(i) =  (estimate - measurements(i))**2;
#end

#stdeviation = sqrt(mean(error));
#printf("standard deviation = %g\n", stdeviation);

fclose("A2-MeasurementData.bin");
fclose("A2-CalibrationData.bin");

###### RESULTS #####################
# bias                    = 0.235710
# melting point           = 20.085607 degrees Celcius
# standard deviation      = 0.243939

