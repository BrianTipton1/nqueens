#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define P_c 0.7     //crossover probability (typical val.)
#define P_m 0.001   //mutation probability  (typical val.)
#define N 8         //population size (change to something even)
#define L 8         //string length (don't change)
#define G 10000       //number of generations (something huge)


/* Prints the population strings in a line */
void print_population(unsigned char population_in[]){
  int iterator = 0;
  int member_count = 0;
  char cur_member;
  while (member_count < N){
    cur_member = population_in[member_count];
    while (iterator < L){
      if (cur_member & 0x80){
	printf("1");
      }
      else{
	printf("0");
      }
      cur_member = cur_member << 1;
      iterator++;
    }
    member_count++;
    iterator = 0;
    printf(" ");
  }
  printf("\n");
}

/* Fitness is determined by the number of 
   1's in the bitstring. */
int get_fitness(unsigned char string_in){
  int count = 0;
  unsigned char temp = string_in;
  while (temp){
    if (temp & 0x01){
      count++;
    }
    temp = temp >> 1;
  }
  return count;
}

/* Randomly initialize the first population */
void init_population(unsigned char* population){
  int i;
  for ( i=0; i<N; i++){
    population[i] = (char)(rand() % 0xFF);
  }
}


/* Perform selection of population members based on fitness */
void do_selection(unsigned char* population, int* selected){
  int fitness[N] = {0};
  int fitness_sum = 0;
  int i, j;
  // get fitness for all members of population
  for ( i=0; i<N; i++ ){
    fitness[i] = get_fitness(population[i]);
    fitness_sum += fitness[i];
  }
  

  // this is simple fitness proportional selection
  // (roulette wheel sampling)
  int roll;
  int temp_sum = 0;
  int selection;
  for ( i=0; i<N; i++ ){
    temp_sum = 0;
    roll = rand()%fitness_sum;
    for ( j=0; j<N; j++ ){
      temp_sum += fitness[j];
      if ( roll < temp_sum ){
	selection = j;
	break;
      }
    }
    selected[i] = selection;
  }
}


/* compute a mask to use when crossing over parents*/
unsigned char get_mask(int locus_in){
  int i = 0;
  unsigned char ret;
  for( i=0; i<locus_in; i++ ){
    ret = ret << 1;
    ret ^= 0x01;
  }
  return ret;
}

/* crossover members with probability P_c
   if no crossover, then clone parents   */
void do_crossover(unsigned char* population, int* selected){
  double crossover_roll;
  int crossover_locus;
  int i;
  unsigned char temp1;
  unsigned char temp2;
  unsigned char mask;
  unsigned char temp_population[N];

  for ( i=0; i<N; i+=2){ 
    crossover_roll = ((double)rand())/((double)RAND_MAX);
    temp1 = 0;
    temp2 = 0;
    if(crossover_roll <= P_c){  //crossover
      crossover_locus = rand()%L;
      mask = get_mask(crossover_locus);
      temp1 = population[selected[i]] & mask;
      temp1 ^= population[selected[i+1]] & ~mask;
      temp2 = population[selected[i+1]] & mask;
      temp2 ^= population[selected[i]] & ~mask;
      temp_population[i] = temp1;
      temp_population[i+1] = temp2;
    }
    else{  //clone
      temp_population[i] = population[selected[i]];
      temp_population[i+1] = population[selected[i+1]];
    }
  }
  
  //copy back to population
  for ( i=0; i<N; i++ ){
    population[i] = temp_population[i];
  }
}


void do_mutation(unsigned char* population){
  double mutation_roll;
  int i, j;
  for ( i=0; i<N; i++){
    for ( j=0; j<L; j++ ){
      mutation_roll = ((double)rand())/((double)RAND_MAX);
      if ( mutation_roll <= P_m ){
	population[i] ^= (1<<j);   //toggle bit
      }
    }
  }
}

int main(){
  unsigned char population[N] = {0};
  int selected[N] = {-1};
  int generation_count = 0;
  int i;
  srand(time(NULL));


  //basic genetic algorithm skeleton
  init_population(population);
  print_population(population);
  
  while( generation_count < G ){
    do_selection(population, selected);
    do_crossover(population, selected);
    do_mutation(population);
    printf("%4d: ", generation_count);
    print_population(population);
    for( i=0; i<N; i++){
      if (population[i]==0xFF){
	printf("Max fit reached.\n");
	return 0;
      }
    }
    generation_count++;
  }

  return 0;
}