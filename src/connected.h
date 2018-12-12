#ifndef CONNECTED_H
#define CONNECTED_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>

#define ROW_COL_TO_INDEX(row, col, n) ((n) * (col) + (row))

/*
  Global Constants
*/
extern const int BITS_PER_LONG;
extern const int BITS_PER_LONG_MINUS_1;
extern const long HIGHEST_LONG_BIT;

/*
  Global Variables
*/
extern int DEBUG;
extern int ERROR;
extern int MAX_LINKS_CONNECTED;
extern int MY_VERSION; // a global constant "VERSION"" causes trouble!
extern char message[];
extern char buffer[];

/*
  General functions
*/
void safe_free(int *pointer, char *name);

int* safe_malloc(char* variableName, int size);

long* safe_malloc_long(char* variableName, int size);

int safe_get(char* variableName, int array[], int index, int maxIndex);

void safe_set(char* variableName, int array[], int index, int value, 
  int maxIndex);
  
void print_if (int condition, char* message);

void showProgressIf(int condition, int i, int n, int step);

void printValues(int n, int* intValues);

void emptyString(char* string);

int findStringInStrings(int n, char* string, char** strings, int* indices, 
  int maxNumber);

void longToBitwiseString(char* buffer, unsigned long value);

int maximumOfInt(int* intValues, int n);

/*
  Related to the (Pseudo-) matrix used to store the information which links are
  connected to each other.
*/
void initMatrix(long* A, int numberOfRows, int numberOfColumns, long value);

long rowColumnToIndex(int row, int column, int numberOfRows);

void markInMatrix(long* A, int row, int bitNumber, int LONG_PER_ROW);
void markInMatrix2(long* A, int row, int bitNumber, int numberOfRows);

void bitwiseOrOfRows(long* A, int targetRow, int sourceRow, int numberOfRows, 
  int numberOfColumns);

int getBitsSet(int* bitnumbers, long value);

void print_matrix(long* A, int n, int LONG_PER_ROW);

/*
  Related to the queue containing the link indices not yet checked
*/
void initQueue(int *queuePosition, int *queueEnd, int n, int *isInQueue);

void addToQueue(int* queue, int* queueEnd, int numIndices, int* indices, 
  int startindex, int maxIndexIndices, int maxIndexQueue, int *isInQueue);

void print_queue(int* queue, int queuePosition, int queueEnd);

int copyIndicesOfConnectedLinks(int* indices, int startIndex, long* A, int row, 
  int LONG_PER_ROW, int numberOfRows, int targetSize);

int writeIndicesOfConnectedLinksToResultVector(
  long* A, int i, int LONG_PER_ROW, int n, int* numberOfTotalLinks, 
  int resultIndex, int resultSize, int* startIndex, int* result_linked);

/*
  Helper-functions for high-level functions
*/
int getIndicesOfDirectIlyConnected(int* indices, int* numberOfLinks, 
  int* indicesOfDirectLinks, int i, int n);

int getIndicesOfLinked(int* indices, long* A, int row, int LONG_PER_ROW,
  int numberOfRows);

/* 
  Functions intended to be called from within the R environment. 
  They all have no return value (void) and all parameters are pointers
*/

void R_getBitsSet(int* arg_bitnumbers, long* arg_value, int* arg_count);

void C_getConnectedLinks(int* in_n, int* in_numberOfDirectLinks, 
  int* in_indicesOfDirectLinks, int* out_numberOfTotalLinks, 
  int* inout_result_size, int* out_result_origin, int* out_result_linked,
  int* in_queueSize, int* in_dbg, int* in_version);

void C_getDirectLinks(int* in_length, char** in_upstreamNodes, 
  char** in_downstreamNodes, int* out_numberOfFound, int* out_indicesOfFound,
  int* inout_maxNumber, int* in_dbg);

int getConnectedLinks_C(int n, int* numberOfDirectLinks, 
  int* indicesOfDirectLinks, int* startIndexDirectLinks, 
  int* numberOfTotalLinks, int resultSize, int* indicesOfTotalLinks, 
  int* startIndexOfTotalLinks, int QUEUE_SIZE, int dbg, int version);

int getDirectLinksFromIDs(int n, char** upstreamNodes, char** downstreamNodes,
  int* numberOfFound, int* startIndex, int* indicesOfFound, int maxNumber,
  int dbg);

#endif
