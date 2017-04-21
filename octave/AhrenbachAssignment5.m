# Assignment 5: Estimate the 3D position of a target by filtering sequence of 1D and 2D sensor observations.
# Seth Kurtenbach

data = dlmread('A5-MeasurementData.txt', ' ');


function [z R H] = getObservation(file,n,k)
    if (n == 1)
        z = [file(k+1)];
        R = [file(k+2)];
        H = [file(k+3),file(k+4),file(k+5)];
    else
        z = [file(k+1);file(k+2)];
        R = [file(k+3),file(k+4);file(k+4),file(k+5)];
        H = [file(k+6),file(k+7),file(k+8);file(k+9),file(k+10),file(k+11)];
    endif
endfunction

function [x P v] = update(x, P, z, R, H)
    S = (H*P*H') + R;
    W = (P*H'*inv(S));
    v = inv((S^(.5)))*[z - (H*x)];
    P = P - W*S*W';
    x = x + W*(z-(H*x));
endfunction

x = [0;0;0];
P = [10^10,0,0;0,10^10,0;0,0,10^10];
xs(1:3,1) = [x];
Ps(1:3,1:3) = [P];
i = 1;
j = 1;
k = 1;
while (i <= length(data))
    if ((data(i)==1) | (data(i)==2))
        [z R H] = getObservation(data,data(i),i);
        [x P v] = update(x,P,z,R,H);
        xs(1:3,j) = [x];
        Ps(:,j+3:j+5) = P;
        if (columns(v)==1)
            vs(1,k) = v(1,1);
            k = k+1;
        else
            vs(1,k) = v(1,1);
    	    vs(1,k+1)=v(1,2);
            k = k +2;
        endif	
        j=j+1;
    endif		
    i = i + 6;
endwhile	

