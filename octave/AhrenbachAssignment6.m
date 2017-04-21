# Assignment 6: Maintain an estimate of the Rover's state as it traverses a 2D environment.
# Seth Kurtenbach

data = textread('RoverData2.txt');


x = [0;0];
P = sqrtm([0.00000000000000000001,0;0,0.00000000000000000001]);

function [u U] = command(data, i)
    u = [data(i+1);data(i+2)];
    R1 = [data(i+3), data(i+4); data(i+4), data(i+5)];
    U = R1 * R1';
endfunction

function [x P v] = moveRover(x, P, u, U)
    x = x + u;
    P = P + U;
    v = u - x;
endfunction

function [z R H] = getObservation(file, n)
    if (file(n) == 1)
        z = [file(n+1)];
        R = [file(n+2)] * [file(n+2)]';
        H = [file(n+3),file(n+4)];
    else
        z = [file(n+1);file(n+2)];
        R1 = [file(n+3),file(n+4);file(n+4),file(n+5)];
        R = R1 * R1';
        H = [file(n+6),file(n+7);file(n+8),file(n+9)];
    endif
endfunction

function [x P v1 vSize] = update(x, P, z, R, H)
    S = (H*P*H') + R;
    W = (P*H'*inv(S));
    v = [z - (H*x)];
    v1 = (S^(-0.5))*[z - (H*x)];
    vSize = v'*inv(S)*v;
    P = P - W*S*W';
    x = x + W*(z-(H*x));
endfunction
j = 1;
i = 1;
k = 1;
pos1(1,1) = 0;
pos2(1,1) = 0;
while (i <= 1960)
    if (data(i) == 0)
        [u U] = command(data,i);
        [x P v] = moveRover(x, P, u, U);
        pos(j,1:2) = x;
        traces(j) = sqrt(trace(P));
        j = j + 1;
        i = i + 6;
    elseif (data(i) == 1) 
        [z R H] = getObservation(data, i);
        [x P v1 vSize] = update(x, P, z, R, H);
        i = i + 5;
        vs(j,1:2) = [v1,v1];
        traces(j) = sqrt(trace(P));
        if (vSize <= 1)
            sizes(k) = 1;
        else
            sizes(k) = 0;
        endif
        j = j + 1;
        k = k + 1;
	else
        [z R H] = getObservation(data, i);
        [x P v1 vSize] = update(x, P, z, R, H);
        i = i + 10;
        vs(j,1:2) = [v1(1,1),v1(2,1)];
        if ((vSize/2) <= 1)
            sizes(k) = 1;
        else
            sizes(k) = 0;
        endif
        traces(j) = sqrt(trace(P));
        j = j + 1;	
       	k = k + 1;
    endif
endwhile
sigmas = sqrt(diag(P));
printf("x = %f %f\n",x(1,1),x(2,1));
printf("sigx = %f\nsigy = %f\n", sigmas(1,1), sigmas(2,1)); 
for i = 2 : length(pos)
    pos1(i) = pos(i,1);
    pos2(i) = pos(i,2);
end

obsPerc = ((sum(sizes))/(length(sizes)));
printf("Percentage of Observations <= 1: %f\n", obsPerc);