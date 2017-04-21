import sys
import os
import math
import csv
import string
import backprop4
import random

random.seed(0)

file = open('geister_data.csv')
geisterfile = csv.reader(file)
geisterdata = []
for row in geisterfile:
    list = []
    for item in row:
        item = float(item)
        list.append(item)
    geisterdata.append(list)

def geisterdesired():
    outs = []
    #print geisterdata
    for i in geisterdata:
        list = []
        list.append(i[-2])
        list.append(i[-1])
        outs.append(list[:])
    #print outs[:15]		
    return outs

def randomweights(ins, outs, n, m):
    matrix = []
    for i in range(outs):
        list = []
        for j in range(ins):
            weight = backprop4.rand(n,m)
            list.append(weight)
        matrix.append(list)
    return matrix
	
def randombias(outs, n, m):
    list = []
    for i in range(outs):
        k = backprop4.rand(n,m)
        list.append(k)
    return list
	
def randTraining():
    newgeister = []
    d = {}
    i = 0
    while i < 560:
        n = backprop4.rand(0,559)
        d[n]=geisterdata[i]
        i = i + 1
    for i in d.keys():
        newgeister.append(d[i])
    return newgeister
def randValid():
    validgeister = []
    d = {}
    i = 560
    while i < 679:
        n = backprop4.rand(560,679)
        d[n]=geisterdata[i]
        i = i + 1
    for i in d.keys():
        validgeister.append(d[i])
    return validgeister

def randTest():
    testgeister = []
    d = {}
    i = 680
    while i < 799:
        n = backprop4.rand(680,799)
        d[n]=geisterdata[i]
        i = i + 1
    for i in d.keys():
        testgeister.append(d[i])
    return testgeister    
	
def train(ins,hids,outs,pattern,inweights,inbias,outweights,outbias,N,M):
    weights = backprop4.neural(ins,hids,outs,pattern,inweights,inbias,outweights,outbias,N,M)    
    return weights
	
def validate(ins,hids,outs,pattern,inweights,inbias,outweights,outbias):
    errors = backprop4.validate(ins,hids,outs,pattern,inweights,inbias,outweights,outbias)
    return errors
	
def test(ins,hids,outs,pattern,inweights,inbias,outweights,outbias):
    errors = backprop4.feedforward(ins,hids,outs,pattern,inweights,inbias,outweights,outbias)
    return errors	
#neural(ins, hids, outs, pattern, inweights, inbias, outweights, outbias, N, M):
if __name__ == '__main__':
    w1 = randomweights(17,10, 0.2, -0.2)
    b1 = randombias(10,0.2,-0.2)
    w2 = randomweights(10, 2,0.2,-0.2)
    b2 = randombias(2, 0.2, -0.2)
    newdatageister = randTraining()	
    geisterValid = randValid()
    geisterTest = randTest()
    weights = train(17,10,2,newdatageister,w1,b1,w2,b2,0.4,0.7)
    results = test(17,10,2,geisterTest,weights[0],weights[1],weights[2],weights[3])

    fulltest = test(17,10,2,geisterdata,weights[0],weights[1],weights[2],weights[3])

 
    trueGood = []
    trueBad = []
    falseGood = []
    falseBad = [] 
    unsure = []    	
    desiredouts = geisterdesired()	
 
    for k in range(len(geisterdata)):
        if desiredouts[k][0] == 1:    # column 18, if the ghost actually is good
                if fulltest[0][k][0] >= 0.5 and fulltest[0][k][1] < 0.5:    # Network id'd it as good
                    trueGood.append(fulltest[0][k])
                elif fulltest[0][k][0] < 0.5 and fulltest[0][k][1] >= 0.5:
                    falseBad.append(fulltest[0][k])
                else:
                    unsure.append(fulltest[0][k])
        elif desiredouts[k][1] == 1:   # column 19, if the ghost actually is bad
                if (fulltest[0][k][1]) >= 0.5 and fulltest[0][k][0] < 0.5:
                    trueBad.append(fulltest[0][k])
                elif fulltest[0][k][1] < 0.5 and fulltest[0][k][0] >= 0.5:
                    falseGood.append(fulltest[0][k])
                else:
                    unsure.append(fulltest[0][k])

    print 'True Positives: ', len(trueGood)
    print 'False Positives: ', len(falseGood)
    print 'True Negatives: ', len(trueBad)
    print 'False Negatives: ', len(falseBad)
    print 'Undecided: ', len(unsure)	
    print 'Total Correct: ', len(trueGood) + len(trueBad)
    print 'Total: ', len(trueGood) + len(trueBad) + len(falseGood) + len(falseBad) + len(unsure)	
