 #include<stdio.h>

int do_addition(int x, int y){
  int c;
  c = x+y ;
  return c ;
}

int do_substraction(int x, int y){
  int c;
  c = x+y ;
  return c ;
}

int main( int argc, char** argv){
  int a, b, c;
  int (*pt2funcA)(int, int);
  int (*pt2funcB)(int, int);
  a = 100 ;
  b = 200 ;

  pt2funcA = do_addition ;
  pt2funcB = do_substraction ;

  c = pt2funcA(a, b);
  c = (*pt2funcB) (b, a);
  return 0;
}
