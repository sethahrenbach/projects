#include <stdio.h>


int main() {

   static char a[100] = "abc";
   int a_index = 0;
   FILE *fp;
   int sz;
   int i = 0;  
  
   fp = fopen ("a.out", "rb");
   fseek(fp, 0, SEEK_END);
   sz = ftell(fp); 
   unsigned char buffer[sz]; 
   rewind(fp);
   fread(buffer, 1, sz, fp);
   printf("size: %d\n", sz); 
   while (i<=sz) {
       if ((buffer[i] == 'a') && (buffer[i+1] == 'b') && (buffer[i+2] == 'c')) {
	       a_index = i;
               buffer[a_index] = 'x';
       }
	i++;		
   }
   printf("index: %d\n", a_index);
  fclose(fp);
  fp = fopen("a.out", "wb");  
  
  fwrite(buffer, 1, sz, fp);
  rewind(fp);
  fclose(fp);
   if (a[0] != 'a')
       printf("I've been modified\n");
   else
       printf("Situation normal\n");
     
  return 0;

}
