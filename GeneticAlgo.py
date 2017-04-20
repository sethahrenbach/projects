import os
import sys
import math
import random
import copy
import csv
import operator
import string


#______ TSP DATA __________
distanceMatrix = []
f = open('TSP.csv', 'rb')
tsp = csv.reader(f)
for row in tsp:
    distanceMatrix.append(row)

#____ Turn a string into a list of Ints_____
def floatify(row):
    for i in row:
        i = float(i)
    return row
    

	
#______Initialize Population___________	
sample_chrm = range(48)
init_population = []
random.seed(0)
population_size = 20

def initialPop(population_size):
    for i in xrange(population_size):
        new_chrm = copy.copy(sample_chrm)
        random.shuffle(new_chrm)
        init_population.append(new_chrm)

    return init_population
	
	
#________Fitness__________
def fitness(chrm):
    cost = 0
    last = chrm[0]
    for city in chrm:
        cost += float(distanceMatrix[last][city])
        last = city
    cost += float(distanceMatrix[chrm[-1]][chrm[0]])
    return cost	


#_____ SELECTION_______

###_____Roulette______
def rouSelect(fitlist):
    fitsum = 0
    for f in fitlist:
        fitsum += f
    probs = map((lambda x: (1-(x/fitsum))),fitlist)

    val = 0
    valprobs = []
    for p in probs:
        valprobs.append(val + p)
        val += p
    valprobs[-1] = 1.0
    selected = []
    size = 20
    for i in xrange(size):
        m = random.random()
        for (j,valprob) in enumerate(valprobs):
            if m <= valprob:
                selected.append(j)
                break
        		
    return selected				

###____Tournament_____	
def tourneyselect(fitlist):
    i = 0
    winners = []
    while i < 20:
        chrom1 = random.randrange(0, len(fitlist))
        chrom2 = random.randrange(0, len(fitlist))
        if fitlist[chrom1]>fitlist[chrom2]:
            winners.append(chrom2)
        else:
            winners.append(chrom1)
        i += 1			
    return winners	

	
#______CROSSOVER__________    

def crossover(parent1, parent2):
    j = random.randrange(0,len(parent1))
    k = random.randrange(0,len(parent1))
    if j < k:
        point1 = j
        point2 = k
    else:
        point1 = k
        point2 = j
	
    endlength = len(parent1) - point2

    p1Middle = parent1[point1:point2]
    p2Middle = parent2[point1:point2]

    p1Reorder = parent1[point2:] + parent1[:point2]
    p2Reorder = parent2[point2:] + parent2[:point2]

    p1filter = filter(lambda x: x not in p2Middle, p1Reorder)
    p2filter = filter(lambda x: x not in p1Middle, p2Reorder)

    child1 = p2filter[(-point1):] + p1Middle + p2filter[:endlength]
    child2 = p1filter[(-point1):] + p2Middle + p1filter[:endlength]
    children = []
    children.append(child1)
    children.append(child2)
    return children	

#_____MUTATION_________
def mutate(pop,rate):
    j = random.randrange(0,48)
    k = random.randrange(0,48)
    p = rate
    temp = 0
    for chrom in pop:
        for gene in chrom:
            q = random.random()
            if q < p:
                temp = chrom[k]
                chrom[k] = chrom[j]
                chrom[j] = temp
                break
    return pop	


#__________HELPER FUNCTIONS__________	
###___________Check Chromosome__________
def has48(chrom):
    for i in chrom:
        for j in range(48):
            if j not in i:
                #print i
                return 0
                
    return 1
###__________Convert to csv_______
def csvifytotal(list,n):
    file = open('tspTotalFitRate'+str(n)+'.csv', 'wb')
    writer = csv.writer(file, quoting = csv.QUOTE_ALL)
    for thing in list:
        writer.writerow(thing)
    print 'Wrote to tspRate',n,'.csv!'
	
def csvifybest(list,n):
    file = open('tspBestPath20pop2elite'+str(n)+'.csv', 'wb')
    writer = csv.writer(file, quoting = csv.QUOTE_ALL)
    writer.writerow(list)#for row in list:
       
    print 'Wrote to tspBestPath20pop2elite',n,'.csv!'
		
	
def evolve():	
    pop = initialPop(20)
    generation = 0
    allfits = []
    bestpaths = []
    while generation < 100:
        if has48(pop):
            totalfit = 0		
            fits = []
            fitnesslist = []
            newfitlist = []
            nextgen = []
            children = []
            parents = []    #print pop
            for ind in pop:
                ind = floatify(ind)#
                fit = fitness(ind)
                fitnesslist.append(fit)
            minfit = min(fitnesslist)
           
            mindex = fitnesslist.index(minfit)
            elite = pop[mindex]
            nextgen.append(elite)
            minfit2 = min(fitnesslist[:mindex]+fitnesslist[mindex+1:])
            mindex2 = fitnesslist.index(minfit2)
            elite2 = pop[mindex2]
            nextgen.append(elite2)			
          
            selectedfits = tourneyselect(fitnesslist)#rouSelect(fitnesslist)#
          
            for i in selectedfits:
                parents.append(pop[i])
   
            while len(children) < 19:
            
                j = random.randrange(0,len(parents)-1)
         
                k = random.randrange(0, len(parents)-1)
                children += crossover(parents[j],parents[k])
               
            children = mutate(children,0.12457) 
            nextgen += children[:]
            
            random.shuffle(nextgen)
            pop = nextgen
            totalfit=0
            for i in range(len(pop)):			
                k = fitness(pop[i])
                totalfit = totalfit + k
                
                
        bestpaths.append(minfit)    
			
        allfits.append(totalfit)
       
        generation += 1 
       
    print pop[mindex]	
    print bestpaths[-1]
    #csvifybest(bestpaths,1246)		
evolve()


#has48()