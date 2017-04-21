# assignment 1: compute and plot the successive mean of the file DataA1.txt
# Seth Kurtenbach
t1 = textread("DataA1.txt", "%f", "delimiter", "\n");
sum = [0];
vals = [];
for i = 1:length(t1)
	sum = sum .+ t1(i);
	mean = sum ./ i;
	printf("%f\n", mean);
    vals(i) = mean;
end
plot (vals);
printf ("\n");

####### RESULTS ##########
Mean = 0.008560