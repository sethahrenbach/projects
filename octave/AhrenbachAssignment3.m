# Assignment 3: Assess the calibration of a 3D sensor
# Seth Kurtenbach

actual = [12.9, 130.4, 23.5]; 
givenCo = [1,1,1;1,2,2;1,2,3];

fm = fopen("A3-MeasurementData(1).bin");

meas = fread(fm, [3, 100000], "float");
meas = meas';

meanM = mean(meas);
# meanM = 12.896, 130.398, 23.495

errorM = meanM .- actual;


varM = var(meas);

covarM = cov(meas);

eigs = eig(givenCo .- covarM);

# eigs = [ -0.0168728    
#           0.0012327
#			0.0030546 ]
