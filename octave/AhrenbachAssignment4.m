# Assignment 4: Implement a filter to process 3D sensor observations.
# Seth Kurtenbach

actual = [12.9; 130.4; 23.5];

R = [1,1,1;
     1,2,2;
     1,2,3];

fm = fopen("A3-MeasurementData(1).bin");

meas = fread(fm, [3, 100000], "float");


## Initialized Estimate (x, P) ##
x = [0;0;0];

P = [10^10,0,0;
     0,10^10,0;
     0,0,10^10];


for i = 1:100000
    z = [meas(1,i);meas(2,i);meas(3,i)];
    S = P + R;
    W = P * inv(S);
    
    innovX(i) = z(1) - x(1);
	innVarX(i) = sqrt(S(1,1));
	innVarXmin(i) = -(innVarX(i));
	
   	innovY(i) = z(2) - x(2);
    innVarY(i) = sqrt(S(2,2));
	innVarYmin(i) = - (innVarY(i));
   	
	innovZ(i) = z(3) - x(3);
    innVarZ(i) = sqrt(S(3,3));
	innVarZmin(i) = - (innVarZ(i));

    #newX(1) = x(1,1);
    #newX(2) = x(2,2);
    #newX(3) = x(3,3);

	euc(i) = norm(x - actual, 2); 
	expDist(i) = sqrt(sum(eig(P)));
	
	P = P - (W * S * W');
    x = x + (W * (z - x));
   
end
	







	 