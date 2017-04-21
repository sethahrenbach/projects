# Assignment 6: Implement routines necessary to estimate the state of a target moving with constant velocity in 2D.
# Seth Kurtenbach

data1 = textread("Target1.txt");
data2 = textread("Target2.txt");


function [x y vx vy P] = predict(dt, x, y, vx, vy, P, qconst)
    F = [ 1, 0, dt, 0;
          0, 1, 0, dt;
          0, 0, 1, 0;
          0, 0, 0, 1];
    Q = qconst .* [1, 0, 0, 0;
                   0, 1, 0, 0;
                   0, 0, 0, 0;
                   0, 0, 0, 0];
    #disp(x);
	est = [x;
	           y;
               vx;
               vy];
	est = F * est;
    x = est(1,1);
    y = est(2,1);
    vx = est(3,1);
    vy = est(4,1);
    P = F*P*F' + Q;  #When processing Target1, do not add Q.
endfunction

function [t x y R] = getObservation(data, i)
    t = data(i);
    x = data(i+1);
    y = data(i+2);
    m = [data(i+3), data(i+4); data(i+4), data(i+5)];
    R = m * m';
endfunction

function [x y vx vy P v normx normy] = update(x, y, vx, vy, P, xnew, ynew, R, dt)
    H = [1, 0, 0, 0;
         0, 1, 0, 0];
    est = [x;
           y;
           vx;
           vy];
		   
    obs = [xnew;
           ynew];
	S = (H*P*H') + R;
    W = (P*H'*inv(S));
    v = [obs - (H*est)];
    normx = v(1,1)/sqrt(S(1,1));
	normy = v(2,1)/sqrt(S(2,2));
    vSize = v'*inv(S)*v;
    P = P - W*S*W';
    est = est + W*(obs-(H*est));
    # vx = (xnew - x)/dt;
    #vy = (ynew - y)/dt;
	x = est(1,1);
	y = est(2,1);
	vx = est(3,1);
	vy = est(4,1);
endfunction


# Initializations
k = 1;	# counts each round
i = 1;  # tracks index in data
qconst = 0;
x = 0;
y = 0;
vx = 0;
vy = 0;
sizesx = [];
sizesy = [];
est = [x; y; vx; vy];
P = [10^10, 0, 0, 0; 0, 10^10, 0, 0; 0, 0, 10^10, 0; 0, 0, 0, 10^10];
oldt = 0;
while (i <= length(data2))
   #Change data1 or data2 here for Target1 and Target2 resp. Target1 qconst = 0.
        [t xnew ynew R] = getObservation(data2, i);   
        dt = t - oldt;
        oldt = t;
	
        [x y vx vy P]   = predict(dt, x, y, vx, vy, P, qconst);
        [x y vx vy P v normx normy]   = update(x, y, vx, vy, P, xnew, ynew, R, dt);		
		xinnov(k) = v(1,1);
		yinnov(k) = v(2,1);
		xnorms(k) = normx;
        ynorms(k) = normy;		

		if (v(1,1) <= 0)
		    sizesx(k) = 1;
		else
		    sizesx(k) = 0;
		endif
		
		if (v(2,1) <= 0)
		    sizesy(k) = 1;
		else
		    sizesy(k) = 0;
		endif
		runningPercX(k) = (sum(sizesx)/length(sizesx));
		runningPercY(k) = (sum(sizesy)/length(sizesy));
		
		
	    k = k + 1;
        i = i + 6;
end

xobs = (sum(sizesx)/600);
yobs = (sum(sizesy)/600);
printf("x = %f\n", x);
printf("y = %f\n", y);
printf("vx = %f\n", vx);
printf("vy = %f\n", vy);

#One Hour Prediction
[x1hr y1hr vx1hr vy1hr P1hr] = predict((3600-oldt), x, y, vx, vy, P, qconst);
OneHourEst = [x1hr; y1hr; vx1hr; vy1hr];
