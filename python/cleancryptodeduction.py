# -*- coding: utf-8 -*-
"""
Created on Wed May 03 13:11:55 2017

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
    
def term_to_tree(string):
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
           
            root.left = term_to_tree(s[0][6:])
         
            root.right = term_to_tree(s[1][:-1])
           
           
        elif front[:4]==".pi1" or front[:4]==".pi2":
         
            if string[5:10]==".pair":
              
                root.data = front[:-1]
               
                root.left = term_to_tree(string[5:-1])
                root.right = None
                
            else:
                print("bad projection")
                root.data=None
        elif front[:2]==".ss" or front[:2]==".pk" or front[:2]==".vk" or front[:2]=='.sk':
            root.data = front[:2]
            root.left = term_to_tree(string[3:-1])
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
    
def allsubs(terms,subterms):
    lt = len(terms)
    ls = len(subterms)
    if lt == 0:
        return subterms
    else:
        t = terms[0]
        tp = term_to_tree(t)
        st = get_subterms(tp, [])
        for i in st:
            if i not in subterms:
                subterms.append(i)
        return allsubs(terms[1:],subterms)

def ID_fullterms(terms, subterms):
    for i in terms:
        if i in subterms:
            ind = subterms.index(i)
            subterms[ind] = '*' + subterms[ind]
    return subterms
    
def newsort(terms):
    tlens = {}
    for t in terms:
        lt = len(t)
        tlens[t]=lt
   # newterms = []
    s = sorted(tlens.items(),key=lambda x: x[1])
    #s.reverse()
    s2 = []
    for i,k in s:
        s2.append(i)
    return s2    

def copysubterms(subterms):
    subscopy = []
    for i in subterms:
        if i[0]=='*':
            subscopy.append(i[1:])
        else:
            subscopy.append(i)
    return subscopy
    
# before calling newdagit, sort list of subterms from shortest to longest by calling newsort()    
def newdagit(subterms,dag,subtermscopy):
    ls = len(subterms)
#    print dag
    if ls == 0:
        return dag
    else:
        s = subterms[0]
        item = []
        if s[0] == '*':
            t = term_to_tree(s[1:])
            label = t.data
            item.append('*')
            item.append(label)
            if label in two_place and label != '.pi1' and label != '.pi2':
                tl = get_term(t.left)
                il = subtermscopy.index(tl)
                item.append(il)
                
                tr = get_term(t.right)
                ir = subtermscopy.index(tr)
                item.append(ir)
                dag.append(item)
                return newdagit(subterms[1:],dag,subtermscopy)                                  
            elif label == '.pi1' or label == '.pi2' or label == '.ss' or label == '.pk' or label == '.vk' or label == '.sk':
                tl = get_term(t.left)
                il = subtermscopy.index(tl)
                items.append(il)
                dag.append(item)
                return newdagit(subterms[1:],dag,subtermscopy)
                
            else:
                dag.append(item)
                return newdagit(subterms[1:],dag,subtermscopy)
        else:
            t = term_to_tree(s)
            label = t.data
            item.append(label)
            if label in two_place and label != '.pi1' and label != '.pi2':
                tl = get_term(t.left)
                il = subtermscopy.index(tl)
                item.append(il)
                
                tr = get_term(t.right)
                ir = subtermscopy.index(tr)
                item.append(ir)
                dag.append(item)
                return newdagit(subterms[1:],dag,subtermscopy)                                  
            elif label == '.pi1' or label == '.pi2' or label == '.ss' or label == '.pk' or label == '.vk' or label == '.sk':        
                tl = get_term(t.left)
                il = subtermscopy.index(tl)
                items.append(il)
                dag.append(item)
                return newdagit(subterms[1:],dag,subtermscopy)
            else:
                dag.append(item)
                return newdagit(subterms[1:],dag,subtermscopy)
t1 = ".senc(.pair(.pair(.pv(k),.pv(k)),.senc(.va(k),.pv(k))),.pair(.pair(.pv(k),.pv(k)),.senc(.pv(k),.pv(k))))"
t2 = ".senc(.pair(.pv(k),.pv(k)),.pair(.pb(m),.pv(k)))"
tprime = ".pair(.pv(k),.pv(k))"
terms = [t1,t2,tprime]
c1 = term_to_tree(t1)
c2 = term_to_tree(t2)
cprime = term_to_tree(tprime)

def terms_to_dag(terms):
    s = allsubs(terms,[])
    s2 = ID_fullterms(terms,s)
    s3 = newsort(s2)
    s4 = copysubterms(s3)
    d = newdagit(s3,[],s4)
    return d

dag = terms_to_dag(terms)
print(dag)    
    
    
######################################################
#######     # # #   #        #          # #  #######
#############      REWRITING RULES      ##############
#######     # # #   #        #          # #  #######
######################################################

def proj1(string):
    t = term_to_tree(string)
    if t.data == ".pi1" and t.left.data == ".pair":
        tp = get_term(t.left.left)
        return(tp)
    else:
        return(False)

def proj2(string):    
    t = term_to_tree(string)
    if t.data == ".pi1" and t.left.data == ".pair":
        tp = get_term(t.left.right)
        return(tp)
    else:
        return(False)
   
def sdec_senc(string):
    t = term_to_tree(string)
    if t.data == ".sdec" and t.right.data == ".senc":
        t1 = get_term(t.left)
        t1p = get_term(t.right.left)
        if t1 == t1p:
            t2 = get_term(t.right.right)
            return(t2)
    else:
        return(False)

def position(string, binary_list):
    t = term_to_tree(string)
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
    t = term_to_tree(string)
    if t.data == ".adec" and t.right.data == ".aenc" and t.left.data==".sk" and t.right.left.data == ".pk":
        t1 = get_term(t.left.left)
        t1p = get_term(t.right.left.left)
        if t1 == t1p:
            t2 = get_term(t.right.right)
            return(t2)
    else:
        return(False)
    
def veri_sign(string):
    t = term_to_tree(string)
    if t.data == ".veri" and t.right.data == ".sign" and t.left.data==".vk" and t.right.left.data == ".ss":
        t1 = get_term(t.left.left)
        t1p = get_term(t.right.left.left)
        if t1 == t1p:
            t2 = get_term(t.right.right)
            return(t2)
    else:
        return(False)


######################################################
######################################################
########                                       #######
########              DEDUCTION                #######
########              ALGORITHM                #######
########                                       ####### 
######################################################
######################################################


# Input: set of terms F, special term t. Decide if F |- t, attacker can deduce t from observed terms F.
# 1. Get the minimal DAG of F union {t}
# 2. Mark vertices of terms in F, mark vertices that contain public names
# 3. For each vertex v:
##   i. if v is marked and labeled 'pair' then mark v.left and v.right
##   ii. if v is marked and labeled 'senc' and v.left is marked, mark v.right
##   iii. if v is marked and labeled 'aenc', let t1 = v.left.left, if vertex for term = 'sk(t1)' is marked, mark v.right
##   iv. if v is unmarked and labeled 'senc' or 'pair', mark if both adjacent vertices are marked
##   v. if v is unmarked and labeld 'sk' or 'pk', mark if adjacent vertex is marked
# 4. If new vertices marked in step 3, repeat, else check vertex for t: if t is marked, output YES, else NO.


def marking(dag, marked):
    added = False

    for v in dag:
        if dag.index(v) in marked and v[0]=='.pair':
            l = v[1]
            r = v[2]
            if l not in marked:
                marked.append(l)
                added = True
            if r not in marked:
                marked.append(r)
                added = True
        elif dag.index(v) in  marked and v[0]=='.senc' and v[2] not in marked:
            if v[1] in marked:
                marked.append(v[2])
                added = True
        elif dag.index(v) in marked and v[0]=='.aenc':
            t1 = dag[dag[v[1]][1]]
            i = dag.index(t1)
            for u in dag:
                if u[0]=='.sk' and u[1]==i and v[2] not in marked:
                    marked.append(v[2])
                    added = True
        elif dag.index(v) not in marked and (v[0]=='.senc' or v[0]=='.pair'):
            if dag[v[1]] in marked and dag[v[2]] in marked and dag.index(v) not in marked:
                marked.append(dag.index(v))
                added = True
        elif dag.index(v) not in marked and v[0]=='.pk' and dag.index(v) not in marked:
            if dag[v[1]] in marked:
                marked.append(dag.index(v))
                added = True
    
    if added:
        return marking(dag,marked)
    else:
        return marked	

def deduce(terms, for_special):
    # terms.append(for_special)
    dag = terms_to_dag(terms)
    
    marked = []
 	# This preprocesses the dag
    for v in dag:
        if v[0] == '*':
            marked.append(dag.index(v))
        else:
            if v[0] == '.pb':
                marked.append(dag.index(v))
	# Strip out the stars
    for v in dag:
        if v[0] == '*':
            vp = v[1:]
            dagl = dag[:dag.index(v)] 
            dagl.append(vp) 
            dagr = dag[dag.index(v)+1:]
            dag = dagl + dagr		
	
# 	# Now recursively call a the marking function
    
    marked = marking(dag,marked)
    
    vfs = [for_special]
    if dag.index(vfs) in marked:
        return True
    else:
        return False

terms2 = ['.senc(.pv(k),.pv(s))', '.pv(k)']
fs = '.pv(s)'
b = deduce(terms2,fs)
print "list of terms: "
print terms2
print '\n'
print "special term: "
print fs
print '\n'
print "T/F: special term deducible from list of terms:\n"

print b 
