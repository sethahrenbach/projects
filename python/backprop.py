import sys
import os
import math
import csv
import string

# I base a lot of this code on bpnn.py by Neil Schemenauer <nas@arctrix.com>

w1 = [
[0.4033, -1.0562],
[0.3900, 0.6544],
[0.6376, -0.0601],
[0.0064, -0.0462],
[0.0782, 0.2728],
[-0.2115, 1.0252],
[0.7298, -0.5047],
[-0.7109, 0.3490],
[-0.9315, 0.9867],
[0.8441, 0.4276],
]


b1 = [
-0.1220,
0.9401,
0.4271,
-0.1775,
-0.7019,
-0.3326,
-0.6961,
-0.9316,
-0.3681,
1.0695,
]

def combinew1b1():
    i = 0
    for pair in w1:
        w1[i].append(b1[i]) # Returns a 10 by 3 matrix
        i = i + 1
    return w1

def b1w1threebyten ():  # Returns a 3 by 10 matrix
    i = 0
    matrix = []
    x1 = []
    x2 = []	
    for i in w1:
        x1.append(i[0])
        x2.append(i[1])
    matrix.append(x1)
    matrix.append(x2)
    matrix.append(b1)
   # print matrix
    return matrix	

w2 = [
[0.0511], 
[0.1611], 
[0.0238], 
[-0.0267], 
[0.1089], 
[0.2381], 
[0.0784], 
[0.0030], 
[0.1646], 
[-0.1779]
]
	
b2 = [0.1131]

def combinew2b2(): # creates a 11 by 1 matrix
    w = []
    #wprime = []
    for i in w2:
        w.append(i)
    w.append(b2)
    #wprime.append(w)
    return w

#def combinew2b2():
#    i = 0
#    for pair in w2:
#        w2.append(b2[i])
#        i = i + 1
#    return w2

cross = [
[-1.992, -1.679, 1],
[-2.073, -1.888, 1],
[-1.303, -1.969, 1],
[-0.8231, 0.4556, 0],
[0.2458, 0.3668, 0],
[0.1761, -0.8369, 0],
[1.983, -2.047, 1],
[-1.566, -0.2537, 0],
[-0.4661, 1.926, 0],
[0.2789, -1.855, 0],
[1.386, 1.881, 1],
[-0.4837, 0.4049, 0],
[2.028, 0.1012, 0],
[-1.699, 0.1198, 0],
[-0.1898, 2.035, 0],
[-1.656, -1.04, 1],
[0.0799, -1.034, 0],
[1.205, 0.1798, 0],
[-0.01459, -1.368, 0],
[1.844, -0.4247, 0],
[-1.534, -1.423, 1],
[-0.9682, -0.4266, 0],
[-1.798, -0.2715, 0],
[-1.11, -1.779, 1],
[-0.5303, -1.161, 0],
[-0.3944, -0.01518, 0],
[1.218, -0.4224, 0],
[0.4926, 1.176, 0],
[0.3579, -0.8319, 0],
[1.885, 0.2564, 0],
[-1.667, -2.011, 1],
[-1.075, -1.49, 1],
[1.384, -1.297, 1],
[1.014, -1.551, 1],
[1.546, 0.371, 0],
[-1.413, 1.263, 1],
[1.974, -0.4662, 0],
[-0.174, 0.4172, 0],
[-1.258, 1.344, 1],
[0.341, -1.46, 0],
[-1.808, -1.811, 1],
[-1.506, -1.125, 1],
[-1.92, 1.404, 1],
[-1.074, -1.732, 1],
[0.3037, -0.6872, 0],
[-0.09194, 1.955, 0],
[-1.929, -1.3, 1],
[-0.01368, 0.4076, 0],
[-0.07602, 1.418, 0],
[-1.716, -0.3089, 0],
[1.328, -1.837, 1],
[1.062, -1.961, 1],
[-0.903, -0.09226, 0],
[0.881, -0.2841, 0],
[-0.2299, 1.111, 0],
[1.316, -1.541, 1],
[0.8228, 0.1823, 0],
[1.886, 1.772, 1],
[-1.841, 1.955, 1],
[0.5467, -0.08004, 0],
[-2.096, -0.4072, 0],
[-0.3814, -1.772, 0],
[1.237, 2.065, 1],
[0.1771, -1.7, 0],
[-0.3884, 1.224, 0],
[1.346, -0.2676, 0],
[-1.882, 0.2944, 0],
[2.074, 1.374, 1],
[1.354, -0.4964, 0],
[2.08, 2.064, 1],
[-1.807, 1.375, 1],
[-1.369, 0.01292, 0],
[2.02, -1.683, 1],
[-1.633, 1.62, 1],
[-0.2409, -0.1867, 0],
[-2.042, -0.2802, 0],
[0.349, 0.8931, 0],
[1.502, -1.443, 1],
[-1.678, -1.593, 1],
[0.4513, 0.2102, 0],
[-2.03, 1.612, 1],
[-1.964, 2.083, 1],
[-0.1889, -0.328, 0],
[0.3868, 2.092, 0],
[0.4403, -0.3477, 0],
[1.408, 1.592, 1],
[-1.28, -2.019, 1],
[-1.717, 0.002513, 0],
[-0.5136, 1.856, 0],
[-1.759, -1.935, 1],
[0.2609, 0.03325, 0],
[-0.2231, -1.097, 0],
[-1.947, 0.1652, 0],
[0.1949, 0.7547, 0],
[-1.238, 1.094, 1],
[-1.196, 1.672, 1],
[1.804, 1.546, 1],
[0.1467, 1.544, 0],
[0.2866, 1.044, 0],
[-1.15, 0.1417, 0],
[-1.426, 1.805, 1],
[-1.488, -1.808, 1],
[-0.358, 1.898, 0],
[0.1402, -0.9843, 0],
[-0.385, -1.017, 0],
[0.4075, 1.613, 0],
[1.699, -0.2624, 0],
[1.48, -0.0112, 0],
[-0.3553, -1.265, 0],
[-2.06, -1.127, 1],
[-0.2302, 0.2039, 0],
[-1.187, 0.05994, 0],
[1.612, -1.979, 1],
[-0.5435, 0.3073, 0],
[1.056, 0.05044, 0],
[-1.095, -0.141, 0],
[1.152, 0.4853, 0],
[1.558, 2.091, 1],
[-1.827, -1.504, 1],
[1.098, 1.618, 1],
[-1.107, 1.213, 1],
[0.5292, -0.1837, 0],
[-1.743, 1.016, 1],
[0.5086, -0.3063, 0],
[-1.063, -1.144, 1],
[0.8324, 0.4277, 0],
[1.172, -0.25, 0],
[0.216, 2.025, 0],
[0.4904, -0.2195, 0],
[-1.672, 0.4781, 0],
[0.2009, -2.047, 0],
[1.061, -0.4657, 0],
[1.434, 1.084, 1],
[-1.381, -1.135, 1],
[1.63, -0.07986, 0],
[-1.992, -2.009, 1],
[1.272, 1.806, 1],
[0.1537, -0.3032, 0],
[0.04358, 0.682, 0],
[2.02, 1.665, 1],
[1.809, 1.275, 1],
[1.16, -2.067, 1],
[-0.1161, 1.626, 0],
[1.729, 1.973, 1],
[0.07801, 1.087, 0],
[-0.04846, -1.54, 0],
[1.151, -1.454, 1],
[1.047, -1.745, 1],
[1.421, 0.4363, 0],
[-0.1455, -1.557, 0],
[0.2297, 0.6046, 0],
[0.7155, -0.4872, 0],
[-0.5983, -0.5193, 0],
[0.01554, -1.196, 0],
[-1.3, -1.681, 1],
[-1.528, 0.5067, 0],
[0.4498, 0.03486, 0],
[1.182, -1.228, 1],
[-1.421, -1.527, 1],
[1.587, 1.724, 1],
[-0.7534, 0.2819, 0],
[-1.187, 1.963, 1],
[-1.255, 1.872, 1],
[-0.463, 0.2529, 0],
[-1.193, -0.4569, 0],
[1.008, 1.294, 1],
[0.08386, -1.963, 0],
[-1.637, -1.774, 1],
[0.4684, -1.955, 0],
[-0.2349, -0.5391, 0],
[1.14, 1.218, 1],
[0.9152, 0.4255, 0],
[-0.2423, 1.878, 0],
[-1.693, -0.0763, 0],
[-0.391, -0.2725, 0],
[0.09341, -0.6507, 0],
[1.759, -1.725, 1],
[-0.413, 1.035, 0],
[1.427, -1.908, 1],
[0.7563, 0.02539, 0],
[-2.051, 0.3901, 0],
[-1.617, 2.096, 1],
[1.479, 1.298, 1],
[0.3573, 1.086, 0],
[0.468, -1.696, 0],
[-1.341, 2.078, 1],
[-1.298, 0.4793, 0],
[0.005047, -0.04917, 0],
[-1.873, 1.785, 1],
[-1.323, -0.1978, 0],
[2.042, 0.01091, 0],
[-1.342, -1.447, 1],
[0.1065, 1.741, 0],
[-0.08199, -1.761, 0],
[-2.003, 1.748, 1],
[1.398, -1.081, 1],
[0.05696, -0.1519, 0],
[1.941, 1.192, 1],
[-0.1016, 0.8007, 0],
[1.718, 0.3654, 0],
[1.683, 1.673, 1],
[-0.4939, 0.734, 0],
[1.545, -1.788, 1],
[1.174, 1.631, 1],
[0.1584, -1.321, 0],
[0.341, 1.866, 0],
[0.3461, 1.424, 0],
[-0.8003, -0.3535, 0],
[-1.78, 1.282, 1],
[1.907, -1.41, 1],
[0.5284, -2.085, 0],
[-1.594, 1.001, 1],
[1.965, -1.094, 1],
[-0.0132, -0.3978, 0],
[0.1208, 0.2452, 0],
[0.07869, 1.52, 0],
[-0.46, -0.752, 0],
[1.908, -0.1631, 0],
[1.013, 0.1572, 0],
[1.706, -0.2702, 0],
[-0.2887, -1.464, 0],
[-0.3722, 1.301, 0],
[-0.3313, -0.5214, 0],
[-1.929, 1.16, 1],
[-1.025, 0.4745, 0],
[-0.4932, 1.648, 0],
[0.5493, 0.1333, 0],
[0.3348, -1.024, 0],
[1.317, 1.487, 1],
[0.2198, 1.316, 0],
[-0.4072, 0.6509, 0],
[-0.07306, 1.286, 0],
[-1.29, -1.271, 1],
[1.013, 1.01, 1],
[-1.405, 0.4542, 0],
[-0.1784, -1.862, 0],
[1.131, 1.455, 1],
[-1.905, 0.0142, 0],
[1.824, -1.527, 1],
[-1.596, 1.123, 1],
[-0.4241, -1.946, 0],
[1.899, -1.174, 1],
[-0.1291, -1.103, 0],
[1.631, 1.481, 1],
[0.3564, 0.5615, 0],
[1.913, -1.209, 1],
[1.935, 0.3576, 0],
[-0.4153, -0.4948, 0],
[1.624, -0.4337, 0],
[-1.586, 1.398, 1],
[-0.9921, 0.2453, 0],
[-1.438, -1.034, 1],
[0.5341, 0.3204, 0],
[-0.7094, 0.06305, 0],
[1.826, -2.012, 1],
[-1.241, -1.433, 1],
[1.956, -1.833, 1],
[0.4084, 0.3103, 0],
[-0.2614, 1.367, 0],
[-1.025, 1.977, 1],
[-1.435, 1.577, 1],
[1.619, 1.242, 1],
[1.423, 1.977, 1],
[0.4243, -1.206, 0],
[-1.436, -0.3086, 0],
[0.08324, 0.7701, 0],
[-0.6677, -0.1148, 0],
[-0.1813, -2.028, 0],
[-0.05635, -0.7132, 0],
[1.749, 1.035, 1],
[1.051, -0.5092, 0],
[1.475, -1.443, 1],
[1.044, -1.198, 1],
[0.1468, -1.209, 0],
[1.856, 1.451, 1],
[1.108, -1.893, 1],
[-1.885, -1.253, 1],
[-0.2268, 0.05082, 0],
[1.521, -1.531, 1],
[-1.844, -1.384, 1],
[-1.617, 1.718, 1],
[-1.139, -0.1733, 0],
[-1.363, 0.2987, 0],
[1.206, -1.645, 1],
[0.219, -0.5012, 0],
[-1.701, 0.4297, 0],
[1.29, -1.073, 1],
[0.2245, -0.1071, 0],
[-0.2201, -0.892, 0],
[-0.7102, -0.2698, 0],
[0.03062, 1.773, 0],
[1.482, 0.2534, 0],
[1.244, -0.02705, 0],
[0.9058, -0.1186, 0],
[0.547, 0.5292, 0],
[-0.459, -1.488, 0],
[1.713, -0.09318, 0],
[-0.5729, -0.07661, 0],
[1.905, 1.808, 1],
[-1.577, -0.4876, 0],
[-0.1671, 0.5381, 0],
[-1.211, -1.051, 1],
[-1.84, 1.546, 1],
[1.608, 0.1573, 0],
[-0.5793, -0.3792, 0],
[0.1193, 1.256, 0],
[-0.03374, 0.2691, 0],
[-0.4863, -1.521, 0],
[0.4114, -1.733, 0],
[-0.5312, 0.1682, 0],
[-1.333, 1.382, 1],
[-0.1111, 0.9102, 0],
[1.71, -1.429, 1],
[1.512, -1.167, 1],
]

### convert crossdata to better format
def processCross():
    newcross = []
    for i in cross:
        inputs = []
        output = []
        eachpair = []
        inputs.append(i[0])
        inputs.append(i[1])
        output.append(i[2])
        eachpair.append(inputs)
        eachpair.append(output)  # Makes a list [[input1, input2],[output]]
        newcross.append(eachpair) # Appends to new cross list.
    return newcross

### Activation function phi ###
def phi(v):
	try:
		return (1 / (1 + math.exp(-v)))
	except OverflowError:
		return 0.0
		
### Derivative of phi ###
def dphi(v):
	return phi(v)*(1-phi(v))


def makeMatrix(I, J, fill=0.0):
    m = []
    for i in range(I):
        m.append([fill]*J)
    return m   

def listZero(J):
    m=[[0.0]]*J
    return m
    
    
### Neural Network ###
class Network:
    def __init__(self, inputNode, hiddenNode, outNode):
		# Number of each type of node
		self.inputNode  = inputNode + 1
		self.hiddenNode = hiddenNode
		self.outNode    = outNode 
		
		# Weights
		inweights = b1w1threebyten()
		outweights = combinew2b2()
		self.inWeights = inweights #makeMatrix(self.inputNode, self.hiddenNode, inweights)
		self.outWeights = outweights #makeMatrix(self.hiddenNode, self.outNode, outweights)
	
		# node activation
		self.inActivate     = [1.0] * self.inputNode # [1.0, 1.0, 1.0]
		self.hiddenActivate = [1.0] * self.hiddenNode # array of 11 1.0's
		self.outActivate    = [1.0] * self.outNode  # [1.0]
		#print 'inActivate1: ', self.inActivate
		
		# Previous weights
		listIn = listZero(self.inputNode)
		listOut = listZero(self.hiddenNode+1)
		self.lastInWeight = makeMatrix( self.inputNode, self.hiddenNode)
		#print 'lastInWeight: ', self.lastInWeight
		self.lastOutWeight = makeMatrix( self.hiddenNode+1, self.outNode)
		#print 'lastOutWeight: ', self.lastOutWeight
		
    def update (self, inputs):	
        if len(inputs) != self.inputNode-1:
            raise ValueError('wrong number of inputs')		

             # input activations
        for i in range(self.inputNode-1): # for i = 0 to 1
            #self.inActivate[i] = phi(inputs[i])
            self.inActivate[i] = inputs[i] #inActivate[i] = inputs[i]
            
             # hidden activations
        for j in range(self.hiddenNode):  #for j = 0 to 9
            sum = 0.0
            for i in range(self.inputNode): #for i = 0 to 2
               sum = sum + self.inActivate[i]*self.inWeights[i][j]  # v = sum(xi*w(i,j)) 
            self.hiddenActivate[j] = phi(sum)                       # phi(v)

        # output activations
        for k in range(self.outNode): #for k = 0
            sum = 0.0
            for j in range(self.hiddenNode): # for j = 0 to 9
                sum = sum + self.hiddenActivate[j] * self.outWeights[j][k] # v = sum(xj*w(j,k))
            self.outActivate[k] = phi(sum)                                 # phi(v)

        return self.outActivate[:]
    
	
    def backPropagate(self, targets, N, M):
        if len(targets) != self.outNode:
            raise ValueError('wrong number of target values')

        # calculate error terms for output
        output_deltas = [0.0] * self.outNode
        for k in range(self.outNode):
            error = targets[k]-self.outActivate[k]
            output_deltas[k] = dphi(self.outActivate[k]) * error

        # calculate error terms for hidden
        hidden_deltas = [0.0] * self.hiddenNode
        for j in range(self.hiddenNode):
            error = 0.0
            for k in range(self.outNode):
                error = error + output_deltas[k]*self.outWeights[j][k]
            hidden_deltas[j] = dphi(self.hiddenActivate[j]) * error

        # update output weights
        for j in range(self.hiddenNode): # for j = 0 to 10
            for k in range(self.outNode):			# for k = 0				
                change = output_deltas[k]*self.hiddenActivate[j]				
                self.outWeights[j][k] = self.outWeights[j][k] + N*change + M*self.lastOutWeight[j][k]
                self.lastOutWeight[j][k] = change
                

        # update input weights
        for i in range(self.inputNode): # for i = 0 to 2
            for j in range(self.hiddenNode): # for j = 0 to 9
                change = hidden_deltas[j]*self.inActivate[i]   # M = 0.7, N = 0.3
                self.inWeights[i][j] = self.inWeights[i][j] + N*change + M*self.lastInWeight[i][j]
                self.lastInWeight[i][j] = change

        # calculate error
        error = 0.0
        for k in range(len(targets)):
            error = error + 0.5*(targets[k]-self.outActivate[k])**2
        return error
	
		
    def test(self, patterns):
        print '.......INPUT ..............Output'        
        for p in patterns:
            print(p[0], '===', self.update(p[0]), 'wanted: ',p[1])		

    def weights(self):
        print('Input weights:')
        for i in range(self.inputNode):
            print(self.inWeights[i])
        print()
        print('Output weights:')
        for j in range(self.hiddenNode):
            print(self.outWeights[j])

    def train(self, patterns, iterations=5000, N=0.7, M=0.3):
        # N: learning rate
        # M: momentum factor
        count = 1 
        for i in range(iterations):
            error = 0.0
            for p in patterns:
                inputs = p[0]
                targets = p[1]
                self.update(inputs)
                error = error + self.backPropagate(targets, N, M)
            if i % 314 == 0:
                print 'epoch number: ', count
                count = count + 1
                print('error: %-.4f' % error)			

def check():
    # Teach network XOR function
    cross = processCross()
    pat = cross 
	#[
     #    [[0,0], [1]],
      #   [[0,1], [0]],
      #   [[1,0], [0]],
       #  [[1,1], [1]]
    #]#

    # create a network with two input, two hidden, and one output nodes
    n = Network(2, 10, 1)#(2,2,1)#
	
    print 'Network : Cross Data+++++++++++++++++++++++'
    # train it with some patterns
    n.train(pat)
    # test it
    n.test(pat)
    n.weights()	

if __name__ == '__main__':
    check()	
#makeMatrix(2,10,w1)