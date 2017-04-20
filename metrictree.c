//
// 
//  
//
//  Created by seth kurtenbach on 4/20/15.
//  Code based on Uhlmann (1991)
//  This code is implemented on an array of
//  ordered pairs. To implement on other objects,
//  make appropriate changes in the main() function and to the 
//  distance() function. 
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#define LENGTH 100000


// OBJECTS //
typedef struct {
    double fst;
    double snd;
} Pair;

typedef struct TreeStruct{
    void * object;
    double radius;
    struct TreeStruct *left;
    struct TreeStruct *right;
}Tree;

Tree * AllocateTree()
{
    return malloc(sizeof(Tree));
}

typedef struct LinkList{
    void * element;
    struct LinkList *next;
} List;

List * AllocateList()
{
    return malloc(sizeof(List));
}

double distance(void* obj_one, void* obj_two)
{
    // distance() will measure the distance between two objects
    
    Pair one;
    Pair two;
    one = *(Pair*)(obj_one);
    two = *(Pair*)(obj_two);
    return hypot((one.fst - two.fst), (one.snd - two.snd));
}


int partition(Tree * array[], int low, int high, int pivot)
{
    register int i, j;
    Tree * tree;
    double radius;
    
    if ((high - low) == 1) return low;
    high--;
    radius = array[pivot]->radius;
    tree = array[low];
    array[low] = array[pivot];
    array[pivot] = tree;
    i = low;
    j = high;
    if (array[high]->radius > radius)
    {
        tree = array[low];
        array[low]=array[high];
        array[high] = tree;
    }
    
    while(i<j)
    {
        tree = array[i];
        array[i] = array[j];
        array[j] = tree;
        i++;j--;
        while(array[i]->radius < radius) i++;
        while(array[j]->radius > radius) j--;
    }
    if (array[low]->radius == radius)
    {
        tree = array[low];
        array[low] = array[j];
        array[j] = tree;
    }
    
    else
    {
        j++;
        tree = array[high];
        array[high] = array[j];
        array[j] = tree;
    }
    return(j);
}


int selectmedian(Tree * array[], int low, int high)
{
    int partition(Tree * array[], int low, int high, int pivot);
    int middle, i;
    
    middle = (low + (high-1))/2;  
    do{
        i = partition(array, low, high, middle);
        if (i < middle) low = i + 1;
        else if (i > middle) high = i;
    } while (i != middle);
    return middle;
}



Tree * build_tree(Tree * array[], int high, int low, double (*distance)(void *, void*))
{
    Tree * tree = NULL;
    
    int size, i, medianIndex;
    Tree *tempnode;
    
    size = high - low;
    if (size == 0) tree = NULL;
    else if (size == 1)
    {
        tempnode = array[low];
        tempnode->object = array[low]->object;
        tempnode->radius = 0.0;
        tempnode->left = NULL;
        tempnode->right = NULL;
        tree = tempnode;
    }
    
    else
    {
        for(i=low+1;i<high;i++)
        {
            array[i]->radius = distance(array[low]->object, array[i]->object);
        }
       
        medianIndex = selectmedian(array,low+1,high);
        array[low]->radius = array[medianIndex]->radius;
        
        
        array[low]->left = build_tree(array, medianIndex+1, low+1,  distance);
        array[low]->right = build_tree(array, high, medianIndex+1, distance);
        
        tree = array[low];
		
        
    }
    return tree;
}

// Free the array's memory //

int free_array(Tree *array[])
{
    int i = 0;
	
	while (i <LENGTH)
	{
	    free(array[i]);
		i++;
	}
    return 0;
}


// mt_search finds a particular object in the tree, or returns NULL
Tree * mt_search(Tree *tree, void * search_object, double distance(void*,void*))
{
    Tree * retval = NULL;
  
    if (tree == NULL) retval = NULL;
    else
    {
	    if (distance(tree->object, search_object)==0.0) retval = tree;
        else if (distance(tree->object, search_object) < tree->radius) {
            retval = mt_search(tree->left, search_object, distance);
        }
         else if (distance(tree->object, search_object) > tree->radius)
	    {
			     retval = mt_search(tree->right, search_object, distance);
	    }
        else {
		    retval = mt_search(tree->left, search_object, distance);
            if (retval == NULL) retval = mt_search(tree->right, search_object, distance);
			
		}
	}

    return retval;
}

// radius_search returns a list of all objects within a given radius
List * radius_search(Tree *tree, void * search_obj, double search_radius, List *list, double distance(void*,void*))
{
   
    List *lptr = NULL;
    
    if (tree == NULL) return list;

	    
    else 
	{
	    if (distance(tree->object, search_obj) <= search_radius) 
		{
		
		    lptr = AllocateList();
		    lptr->element = tree->object;
		    lptr->next = list;
		    list = lptr;
			
        } 	
	    // search radius extends beyond median radius at node
	    if (distance(search_obj, tree->object) + search_radius > tree->radius) {
		    list = radius_search(tree->right, search_obj, search_radius, list, distance);
	    }
        // search radius overlaps with median radius at node
	    if (distance(search_obj, tree->object) - search_radius <= tree->radius) {
		    list = radius_search(tree->left, search_obj, search_radius, list, distance);
	    }
		
	}	
	
	return list;
}

void pretty_print_tree(Tree *t,int depth)
{
    int i;
    
    if(t==NULL) {
        for(i = 0; i < depth; i++)
            printf("  ");
        
        printf("*\n");
    }
    else {
        pretty_print_tree(t->right,depth+1);
        
        for (i = 0; i < depth; i++)
            printf("  ");
        
        printf("(%f,%f), %f\n", (*(Pair*)(t->object)).fst,(*(Pair*)(t->object)).snd, t->radius);
        
        pretty_print_tree(t->left,depth+1);
    }
}



// brute force algorithm
 
List * brute_radius_search(Tree *array[], int size, void * search_object, double search_radius, double distance(void*, void*))
{
    List *list = NULL;
	List *ptr = NULL;
	int i = 0;
	
	for(i=0;i<size;i++)
	{
	    if (distance(search_object, array[i]->object) <= search_radius)
		{
		     ptr = AllocateList();
			 ptr->element = array[i]->object;
			 ptr->next = list;
			 list = ptr;
		}
	}
	return list;
}

int compare_lists(List *list1, List *list2)
{
    int count1, count2 = 0;
	Pair pair;
	for(count1=0;list1->next != NULL; count1++) {list1 = list1->next;}
	for(count2=0;list2->next != NULL; count2++) {list2 = list2->next;}
	pair.fst = count1;
	pair.snd = count2;
	printf("lengths: (%f, %f)\n", pair.fst, pair.snd);
	return (pair.fst == pair.snd);
    	
}


int main()
{
   
    Pair *array = malloc((sizeof(Pair)) * LENGTH);
	int lists_eq;
    int i = 0;
	double search_rad;
    Tree ** ptr_array = malloc((sizeof(Tree))*LENGTH);
    
	Tree * tree_search = NULL;
    Tree * tree = NULL;
	List * list = NULL;
	List * brute_list = NULL;
    
    while(i<LENGTH)
    {
        array[i].fst = (rand() / (RAND_MAX + 1.0)); // rand() % 200;// 
        array[i].snd = (rand() / (RAND_MAX + 1.0)); //rand() % 20;//
        ptr_array[i] = AllocateTree();
        ptr_array[i]->object = &array[i];
        ptr_array[i]->left = NULL;
        ptr_array[i]->right = NULL;
        ptr_array[i]->radius = 0.0;
        i++;
    }
    
  
    tree = build_tree(ptr_array, LENGTH, 0, distance); //array, high, low, comp, distance
   // pretty_print_tree(tree,0);
	
	for(i=0;i<LENGTH;i++)
	{
	    tree_search = mt_search(tree, ptr_array[i]->object, distance);
	     /*if (tree_search != NULL) printf("found object: (%f,%f)\n", (*(Pair*)tree_search->object).fst, (*(Pair*)tree_search->object).snd);
	    else*/
		if (tree_search==NULL)  printf("couldn't find (%f,%f) i: %d \n", (*(Pair*)(ptr_array[i]->object)).fst, (*(Pair*)(ptr_array[i]->object)).snd, i);
	}
	tree_search = mt_search(tree, ptr_array[9]->object, distance);
	// Assign the search radius here
	search_rad = 0.1;
	list = radius_search(tree, ptr_array[3]->object, search_rad, list, distance);
	brute_list = brute_radius_search(ptr_array, LENGTH, ptr_array[3]->object, search_rad, distance);
	printf("search object: (%f, %f)\nsearch radius: %f\n", (*(Pair*)(ptr_array[3]->object)).fst, (*(Pair*)(ptr_array[3]->object)).snd, search_rad);
    
	lists_eq = compare_lists(list, brute_list);
	if (lists_eq == 1) 
	{
	    printf("lists are equal in length\n");
	}
	else printf("lists are not equal in length\n");
/*	if (list != NULL) 
	{
	    while (list != NULL)
		{
		    printf("rad search: (%f, %f) d: %f\n", (*(Pair*)(list->element)).fst, (*(Pair*)(list->element)).snd, distance(ptr_array[3]->object, list->element));
		    list = list->next;
			
		}
		
	}
	else printf("error\n");
	printf("\n\n");
	if (brute_list != NULL)
	{
	    while (brute_list != NULL)
		{
		    printf("brute srch: (%f, %f) d: %f\n", (*(Pair*)(brute_list->element)).fst, (*(Pair*)( brute_list->element)).snd, distance(ptr_array[3]->object, brute_list->element));
		    brute_list = brute_list->next;
			
		}
		
	}
	else printf("brute error\n");
	
*/
	
	free_array(ptr_array);
	
	return 0; 
} 