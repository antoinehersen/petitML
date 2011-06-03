#include<stdio.h>
#include <stdlib.h>

int petitML_entry(int *) ;

int main( int argc, char** argv){
  int *heap ;
  int res;
  // create heap
  heap = malloc( 500 * sizeof (int));
  res = petitML_entry( heap);
  printf("\n Result : %n \n",res);
  return 0;
}
