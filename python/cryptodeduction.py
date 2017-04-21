# -*- coding: utf-8 -*-
"""
Created on Thu Mar 02 11:33:43 2017

@author: Seth
"""
two_place = [".sdec",".senc",".aenc",".adec",".sign",".veri",".pair",".pi1",".pi2"]
one_place = [".ss",".pk",".vk",".sk",".pv",".pb",".va"]


two_place_p = ["sdec(","senc(","aenc(","adec(","sign(","veri(","pair("]
one_place_p = ["ss(","pk(","vk(","sk","pv(","pb(","va("]

############################################
####                                   #####
########  PARSE INTO A MINIMAL DAG   #######   
####                                   #####
############################################



class Tree(object):
    def __init__(self):
        self.left = None
        self.right = None
        self.data = None
        
class Dag(object):
    def __init__(self):
        self.in_edge = []
        self.out_edge = []
        self.term = None
        self.data = None
        self.left = None
        self.right = None
    def indegree(self):
        return(len(self.in_edge))
    def outdegree(self):
        return(len(self.out_edge))

def commas(string):
    count = 0
    for i in string:
        if i == ",":
            count += 1
    return(count)
    
def rparens(string):
    count = 0
    for i in string:
        if i == ")":
            count += 1
    return(count)
    
def lparens(string):
    count = 0
    for i in string:
        if i == "(":
            count += 1
    return(count)

def langle(string):
    count = 0
    for i in string:
        if i == "<":
            count += 1
    return(count)
    
def rangle(string):
    count = 0
    for i in string:
        if i == ">":
            count += 1
    return(count)
    

def getpair(string,front):
    
# count up by two place predicates and down by atoms until it gets to 1
    if front in two_place:
        s = string.split('.')
        j = 0
        k = 0
        i = 1
        for i in range(len(s)-1)[1:]:
            if s[i] in two_place_p and j==0:
                j += 2
            elif s[i] in two_place_p:
                j += 1
            elif s[i][:3] in one_place_p:
                j = j-1
            else:
                0==0
            k += 1
            if j == 1:
                break
        
        n = 1
        k = 0
        while n <= i+1:
            if s[n][-1] == ',':
                k += 1
            n += 1
        
    s= string.split(',')            
    s = ",".join(s[:k]), ",".join(s[k:])
    return(s)
dic = {}
nodes = []

dag = []
st=[]

def find_term_in_dag(term,dag):
    if term == dag.data:
        return(dag)
    else:
        if dag.left != None:
            find_term_in_dag(term,dag.left)
        if dag.right != None: 
            find_term_in_dag(term,dag.right)
        else:
            return(None)

def get_subterms_of_dag(dag,subs):
    if dag.data != None:
        if dag.data not in subs:
            subs.append(dag.data)
        if dag.left != None:
            get_subterms_of_dag(dag.left,subs)
        if dag.right != None:
            get_subterms_of_dag(dag.right,subs)
    return(subs)

def tree_to_dag(tree,dag):
    dagnode = Dag()
    term = get_term(tree)
   # print("before")
    subs = get_subterms_of_dag(dag,[])
   # print("after")
    if term in subs:
        f = find_term_in_dag(term,dag)  # function from term x dag to dag-node
        return(f)
    else:
        dagnode.data = term
        print(term)
        front = tree.data
        if front in two_place and front != ".pi1" and front != ".pi2":
            
            dagnode.left = tree_to_dag(tree.left,dag)
            dagnode.left.to_edge.append(dagnode)
            dagnode.from_edge.append(dagnode.left)
            
            dagnode.right = tree_to_dag(tree.right,dag)
            dagnode.right.to_edge.append(dagnode)
            dagnode.from_edge.append(dagnode.right)
        elif front == ".ss" or front == ".sk" or front == ".vk" or front == ".pk" or front == ".pi1" or front == ".pi2":
            dagnode.left = Dag()
            dagnode.left = tree_to_dag(tree.left,dag)
            dagnode.left.to_edge.append(dagnode)
            dagnode.from_edge.append(dagnode.left)
            
        else:
            return(dagnode)
            
    
def parsenc(string):
    root = Tree()
    rp = rparens(string)
    lp = lparens(string)
    ra = rangle(string)
    la = langle(string)
    spaces = string.split(' ')
    string = ""
    for i in range(len(spaces)):
        string += spaces[i] 
    if rp == lp and ra == la:
        front = string[0:5]
    
        if front in two_place: #=="sdec" or front=="senc" or front=="aenc" or front == "adec" or front=="sign" or front == "veri":
            root.data = front
            s=getpair(string,front)
           
            root.left = parsenc(s[0][6:])
         
            root.right = parsenc(s[1][:-1])
           
           
        elif front[:4]==".pi1" or front[:4]==".pi2":
         
            if string[5:10]==".pair":
              
                root.data = front[:-1]
               
                root.left = parsenc(string[5:-1])
                root.right = None
                
            else:
                print("bad projection")
                root.data=None
        elif front[:2]==".ss" or front[:2]==".pk" or front[:2]==".vk" or front[:2]=='.sk':
            root.data = front[:2]
            root.left = parsenc(string[3:-1])
            root.right = None
           
        else:
            root.data = string
            root.left = None
            root.right=None
        return(root)
    else:
        print("parens mismatch")
        root.data= "badwolf"
        print(root.data)
        
def get_term(tree):
    if tree != None:
        t = tree.data
        if t in two_place and t != '.pi1' and t != '.pi2':
            s = t + "(" + get_term(tree.left) + "," + get_term(tree.right) + ")"
            return(s)
        elif t in one_place and t != '.pv' and t!='.pb' or t=='.pi1' or t=='.pi2' :# == ".ss" or t==".pk" or t==".vk":
            s = t + "(" + get_term(tree.left) + ")"
            return(s)
        else:
            return(t)
    else:
        return("weird problem")
    
def get_subterms(tree,st):
    if tree != None:
        t=get_term(tree)
        if t not in st:
            st.append(t)
        if tree.left != None:
            get_subterms(tree.left,st)
        if tree.right != None:
            get_subterms(tree.right,st)
    return(st)
            
            
        
def copynodes(nodes):
    nd = []
    for i in range(len(nodes)):
        nd.append(nodes[i])
    return(nd)

def getnodes(tree,nodes):
    if tree.data != None:
#        nodes.append(tree.data)
        if tree.left != None or tree.right != None:
            #if tree.data not in nodes:
            nodes.append(tree.data)
            if tree.left != None:
                getnodes(tree.left,nodes)
                if tree.right != None:
                    getnodes(tree.right,nodes)
        else:
            if tree.data not in nodes:
                nodes.append(tree.data)
    return(nodes)



        

def dagit(nodes,tree,dag,top,beg):
    print(nodes)
    print(dag)
    
    if tree.data != None:
       i = nodes.index(tree.data)
       if not(tree.data in nodes[:len(dag)-beg]):
           dag.append([])
       k = len(dag)-1

       if top==True:
          dag[k].append("*")
          top = False
       if tree.left!=None:
           
           nodes[i]='a'
          
           dag[k].append(nodes.index(tree.left.data))
           dag = (dagit(nodes,tree.left,dag,top,beg))
           
       if tree.right != None:
           
           dag[k].append(nodes.index(tree.right.data))
           dag = (dagit(nodes,tree.right,dag,top,beg))
          # dag = dag[:len(nodes)]
#    atoms = len(nodes) - nodes.count('a')
#    print(atoms)
       
#    for i in range(len(nodes) - atoms):
#        if dag[-i] == [] and atoms >0:
#            dag = dag[-i:]+dag[:-(i+1)]
#            atoms = atoms -1
    
    #print(dag)   
    return(dag)

def min_dag(dag):
#    for i in range(len(dag)):
#        if i == []:
#            dag=dag[:i] + dag[i+1:]
    l = len(dag)
    got_match = 1
    matches=[1]
    changed = []
    while len(matches)>0:
        matches = []
        #print(dag)
        
        for i in range(l-1,-1,-1):
            for k in range(l-1,-1,-1):
                got_match=0
                if dag[i][0] == '*':
                    dagi = dag[i][1:]
                else:
                    dagi = dag[i][:]
                if dag[k][0] == '*':
                    dagk = dag[k][1:]
                else:
                    dagk = dag[k][:]
                
                if dagi==dagk and i >k:
                    matches.append([k,i])
                    got_match=1
                
      #  print(matches)#if got_match==1:
        for m in matches:
            for i in range(l-1,-1,-1):
                for k in range(len(dag[i])):
                    if dag[i][k]== m[1] :
                        changed.append(i)
                        dag[i][k]=m[0]
            if dag[m[0]][0]=='*':
                if dag[m[1]][0]!='*':
                    dag[m[1]] = ['*']+dag[m[1]]
            if dag[m[1]][0]=='*' :
                if dag[m[0]][0]!='*':
                    dag[m[0]] = ['*']+dag[m[0]]
        #    print(dag)#for m in matches:
            if m[1]<len(dag):
                #print(m[1])
            
                for r in range(l):
                    if len(dag[r])>3:
                        if dag[r][0]=="*":
                            if dag[r][1]>m[1]:
                                dag[r][1] = dag[r][1] -1
                            if dag[r][2]>m[1]:
                                dag[r][2] = dag[r][2]-1
                            
                                
                        else:
                            if dag[r][0]>m[1]:
                                dag[r][0] = dag[r][0] -1
                            if dag[r][1]>m[1]:
                                dag[r][1] = dag[r][1]-1  
                dag = dag[:m[1]]+dag[(m[1]+1):]
                l=len(dag)
            else:
                dag = dag[:m[1]]
                l=len(dag)  
                                      
        #print(matches)
        
        #print(changed)  
    return(dag)

def label_dag(tree,dag,top):
    #dag = []
    k = len(dag)
    nodes = getnodes(tree,[])
    n2 = copynodes(nodes)
    print(k)
    d = dagit(nodes,tree,dag,top,k)
  #  print(n2)
    
#    print(get_term(tree))
 #  print(len(d)-len(n2))
    if k == 0:
        for i in range(len(nodes)):
            d[i].append(n2[i])
    else:
        for i in range(len(nodes)):
            d[i+k].append(n2[i])
    for i in range(len(d)-1,-1,-1):
        if d[i] ==[]:
            d= d[:i]+d[i+1:]
    if d[-1]==[]:
        d = d[:-1]
    print(d)    
    return(d)

t1 = ".senc(.pair(.pair(.pv(k),.pv(k)),.senc(.va(k),.pv(k))),.pair(.pair(.pv(k),.pv(k)),.senc(.pv(k),.pv(k))))"
t2 = ".senc(.pair(.pv(k),.pv(k)),.pair(.pb(m),.pv(k)))"
tprime = ".pair(.pv(k),.pv(k))"
terms = [t1,t2]
c1 = parsenc(t1)
c2 = parsenc(t2)
cprime = parsenc(tprime)
l1 = label_dag(cprime,[],True)
#print(l1)
#l1 = l1[:-1]
#for i in range(len(l1)):
#    if i == []:
#        l1 = l1[:i]+l1[i+1:]
l2 = label_dag(c2,l1,True)
l3 = label_dag(c1,l2,True)
d = Dag()
#d2 = tree_to_dag(c2,d)
t = get_term(c1)

def terms_to_dag(terms,dag):
    print(dag)
    print(terms)
    if terms != []:
        t = parsenc(terms[0])
        l = label_dag(t,dag,True)
        return(terms_to_dag(terms[1:],l))
    else:
        return(dag)

t3=(terms_to_dag(terms,[]))

##
#  The following function still requires a helper function get_that_odag defined
##

def object_dag(dag,dagnode,odag):
    odag.data = dagnode[-1]
    dagnode.append('check')
    if dagnode[0]=='*':
        odag.term = True
        if len(dagnode)>3:
            if dag[dagnode[1]][-1]=='check' :
                # already an odag for the node
                lodag = get_that_odag(dag[dagnode[1]])
                odag.out_edge.append(lodag)
                odag.left = lodag
            else:
                #make a new odag for the node
                lodag = Dag()
                lodag.to_edge.append(odag)
                odag.out_edge.append(lodag)
                ldagnode = dag[dagnode[1]]
                odag.left = object_dag(dag,ldagnode,lodag)
            if dag[dagnode[2][-1]] == 'check':
                rodag = get_that_odag(dag[dagnode[2]])
                odag.out_edge.apend(rodag)
                odag.right = rodag
            else:
                rodag = Dag()
                rodag.to_edge.append(odag)
                odag.out_edge.append(rodag)
                rdagnode = dag[dagnode[2]]
                odag.right = object_dag(dag,rdagnode,rodag)
        elif len(dagnode) == 3:
            return(odag)
    else:
        odag.term = False
        if len(dagnode)>2:
            if dag[dagnode[0]][-1]=='check' :
                # already an odag for the node
                lodag = get_that_odag(dag[dagnode[0]])
                odag.out_edge.append(lodag)
                odag.left = lodag
            else:
                #make a new odag for the node
                lodag = Dag()
                lodag.to_edge.append(odag)
                odag.out_edge.append(lodag)
                ldagnode = dag[dagnode[0]]
                odag.left = object_dag(dag,ldagnode,lodag)
            if dag[dagnode[1]][-1] == 'check':
                rodag = get_that_odag(dag[dagnode[1]])
                odag.out_edge.apend(rodag)
                odag.right = rodag
            else:
                rodag = Dag()
                rodag.to_edge.append(odag)
                odag.out_edge.append(rodag)
                rdagnode = dag[dagnode[1]]
                odag.right = object_dag(dag,rdagnode,rodag)
        elif len(dagnode) == 2:
            return(odag)
            

def get_dag_term(dag,idx):
    
    t = dag[idx][-1]
   # print(t)    
    if t in two_place and t != '.pi1' and t != '.pi2':
        term = t + '(' + get_dag_term(dag,dag[idx][-3]) + ',' + get_dag_term(dag,dag[idx][-2]) + ')'
        return(term)
    elif t in one_place and t != '.pv' and t!='.pb' or t=='.pi1' or t=='.pi2' :
        s = t + "(" + get_dag_term(dag,dag[idx][-2]) + ")"
        return(s)
    else:
        return(t)
    
    
    

######################################################
#######     # # #   #        #          # #  #######
#############      REWRITING RULES      ##############
#######     # # #   #        #          # #  #######
######################################################

def proj1(string):
    t = parsenc(string)
    if t.data == ".pi1" and t.left.data == ".pair":
        tp = get_term(t.left.left)
        return(tp)
    else:
        return(False)

def proj2(string):    
    t = parsenc(string)
    if t.data == ".pi1" and t.left.data == ".pair":
        tp = get_term(t.left.right)
        return(tp)
    else:
        return(False)
   
def sdec_senc(string):
    t = parsenc(string)
    if t.data == ".sdec" and t.right.data == ".senc":
        t1 = get_term(t.left)
        t1p = get_term(t.right.left)
        if t1 == t1p:
            t2 = get_term(t.right.right)
            return(t2)
    else:
        return(False)

def position(string, binary_list):
    t = parsenc(string)
    if binary_list != []:    
        i = binary_list[0]
        if i == 0:
            t = t.left
            s = get_term(t)
            return(position(s,binary_list[1:]))
        elif i == 1:
            t = t.right
            s = get_term(t)
            return(position(s,binary_list[1:]))
    else:
        return(False)
    
def adec_aenc(string):
    t = parsenc(string)
    if t.data == ".adec" and t.right.data == ".aenc" and t.left.data==".sk" and t.right.left.data == ".pk":
        t1 = get_term(t.left.left)
        t1p = get_term(t.right.left.left)
        if t1 == t1p:
            t2 = get_term(t.right.right)
            return(t2)
    else:
        return(False)
    
def veri_sign(string):
    t = parsenc(string)
    if t.data == ".veri" and t.right.data == ".sign" and t.left.data==".vk" and t.right.left.data == ".ss":
        t1 = get_term(t.left.left)
        t1p = get_term(t.right.left.left)
        if t1 == t1p:
            t2 = get_term(t.right.right)
            return(t2)
    else:
        return(False)
  
            
    