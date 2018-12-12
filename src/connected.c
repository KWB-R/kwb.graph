//
// Use the following R command to compile and link:
// R CMD SHLIB connected.c helperFunctions.c
//
// in case of error, e.g. "error: bad value (core2) for -mtune= switch"
//
// R CMD SHLIB connected.c helperFunctions.c --dry-run 
//
// This will show the commands that you can then run individually:
// make -f "C:/Programme/R/R-3.0.1/etc/i386/Makeconf" -f 
// "C:/Programme/R/R-3.0.1/share/make/winshlib.mk" SHLIB="connected.dll" 
// OBJECTS="connected.o helperFunctions.o"

//#include <R.h>
//#include <Rinternals.h>

#include "connected.h"

int ERROR = 0;

/* 
   Functions intended to be called from within the R environment. 
   They all have no return value (void) and all parameters are pointers
*/

void R_alloc_test(int* result)
{
  for (int i = 0; i < 10; i++) {
    result[i] = 100*i;
  }
}

// R_getBitsSet
void R_getBitsSet(int* arg_bitnumbers, long* arg_value, int* arg_count)
{
  *arg_count = getBitsSet(arg_bitnumbers, *arg_value);
}

// C_getConnectedLinksAtOnce
void C_getConnectedLinksAtOnce
(
  int *in_n,
  char** in_upstreamNodes,
  char** in_downstreamNodes,
  int *out_total_number,
  int *out_total_indices,
  int *out_total_start,  
  int *in_expectedResultSize,
  int *in_queueSize,
  int *in_dbg,  
  int *in_version
)
{
  int n = *in_n;
  DEBUG = *in_dbg;
  MY_VERSION = *in_version;
  
  int direct_number[n];
  int direct_indices[*in_expectedResultSize];
  int direct_start[n];
  
  // updates maxNumber with the real maximum number of direct links
  getDirectLinksFromIDs(n, in_upstreamNodes, 
    in_downstreamNodes, direct_number, direct_start, direct_indices, 
    *in_expectedResultSize, DEBUG);
  
  getConnectedLinks_C(n, direct_number, direct_indices, 
    direct_start, out_total_number, *in_expectedResultSize, out_total_indices, 
    out_total_start, n*n, DEBUG, MY_VERSION
  );
}

// transform_R_to_C 
void transform_R_to_C(int n, int *number, int *matrix, int *vector, int *start)
{
  int vectorIndex = 0;
  int startIndex = 0;
  
  for (int i = 0; i < n; i++) {
    
    showProgressIf(DEBUG, i, n, 100);
      
    // convert from R's 1-based to C's 0-based index
    for (int j = 0; j < number[i]; j++) {      
      
      vector[vectorIndex++] = matrix[j*n + i] - 1;      
      
    }
    
    start[i] = startIndex;
    
    startIndex += number[i];
    
  } // end of for(i)  
}

// transform_C_to_R
void transform_C_to_R(int n, int *number, int *indices, int resultSize,
  int *result_origin, int *result_linked)
{  
  int index = 0;
  
  for (int i = 0; i < n; i++) {

    for (int j = 0; j < number[i]; j++) {

      int value = safe_get("indices", indices, index, resultSize);

      // add one to convert between 0-based index (C) and 1-based index (R)
      safe_set("result_linked", result_linked, index, value + 1, resultSize);
      safe_set("result_origin", result_origin, index, i + 1, resultSize);
  
      if (ERROR) return;
      
      index++;
    }
  }
}

// transform_C_vector_to_R_matrix
void transform_C_vector_to_R_matrix(int n, int *number, int *vector, 
  int *matrix)
{
  int resultIndex = 0;

  for (int i = 0; i < n; i++) {
    
    for (int j = 0; j < number[i]; j++) {
      
      matrix[j*n + i] = vector[resultIndex] + 1;
      
      resultIndex++;
    }
  }
}

// C_getConnectedLinks
void C_getConnectedLinks
(
  int *in_n,
  int *in_numberOfDirectLinks,
  int *in_indicesOfDirectLinks,
  int *out_numberOfTotalLinks,
  int *inout_result_size,
  int *out_result_origin,
  int *out_result_linked,
  int *in_queueSize,
  int *in_dbg,
  int *in_version
)
{  
  // set global variables
  DEBUG = *in_dbg;
  MY_VERSION = *in_version;
  
  print_if(DEBUG, "*** Begin of C_getConnectedLinks\n");  

  if (DEBUG) {
    printf("*in_n = %d\n", *in_n);
    printf("*inout_result_size = %d\n", *inout_result_size);
    printf("*in_queueSize = %d\n", *in_queueSize);
    printf("*in_dbg = %d\n", *in_dbg);
    printf("*in_version = %d\n", *in_version);
    printf("sizeof(in_numberOfDirectLinks) = %d\n", sizeof(in_numberOfDirectLinks));
    printf("sizeof(in_indicesOfDirectLinks) = %d\n", sizeof(in_indicesOfDirectLinks));    
  }
  
  int n = *in_n;
  
  int size_startIndex           = n * sizeof(int);
  int size_indicesOfDirectLinks = n * maximumOfInt(in_numberOfDirectLinks, n) * sizeof(int);
  
  int* startIndex           = safe_malloc("startIndex", size_startIndex);
  int* startIndexTotalLinks = safe_malloc("startIndexTotalLinks", n * sizeof(int));
  int* indicesOfTotalLinks  = safe_malloc("indicesOfTotalLinks", *inout_result_size * sizeof(int));
  int* indicesOfDirectLinks = safe_malloc("indicesOfDirectLinks", size_indicesOfDirectLinks);
  
  if (ERROR) return;
  
  // fill startIndex and indicesOfDirectLinks
  print_if(DEBUG, "*** Filling startIndex and indicesOfDirectLinks...\n");
  
  transform_R_to_C(n, in_numberOfDirectLinks, in_indicesOfDirectLinks,
    indicesOfDirectLinks, startIndex);
  
  print_if(DEBUG, "*** startIndex and indicesOfDirectLinks filled.\n");

  int actualResultSize = getConnectedLinks_C(
    *in_n, 
    in_numberOfDirectLinks, 
    indicesOfDirectLinks, 
    startIndex, 
    out_numberOfTotalLinks, 
    *inout_result_size,
    indicesOfTotalLinks, 
    startIndexTotalLinks, 
    *in_queueSize, 
    DEBUG, 
    MY_VERSION
  );
    
  if (ERROR) return;

  // C-structure to R-structure, e.g. conversion of 0-based to 1-based index
  print_if(DEBUG, "*** Filling out_result_origin and out_result_linked... ");
  
  transform_C_to_R(n, out_numberOfTotalLinks, indicesOfTotalLinks, 
    actualResultSize, out_result_origin, out_result_linked);
  
  print_if(DEBUG, "ok.\n");

  *inout_result_size = actualResultSize;  

  safe_free(startIndex, "startIndex");
  safe_free(startIndexTotalLinks, "startIndexTotalLinks");
  safe_free(indicesOfTotalLinks, "indicesOfTotalLinks");
  safe_free(indicesOfDirectLinks, "indicesOfDirectLinks");

  print_if(DEBUG, "*** End of C_getConnectedLinks\n");    
}

// getConnectedLinks_C
int getConnectedLinks_C
(
  int n,
  // number of links = length of array numberOfDirectLinks
  int* numberOfDirectLinks,
  // array of length n, holding for each link i (at index i) the number of links
  // to which link i is directly connected. 
  int* indicesOfDirectLinks,
  int* startIndexDirectLinks,
  int* numberOfTotalLinks,
  // output array of length n in which the numbers of links to which each link i
  // is connected (directly or indirectly via other links), are returned
  int resultSize,
  int* indicesOfTotalLinks,
  int* startIndexTotalLinks,
  int QUEUE_SIZE,
  int dbg,
  int version
)
{  
  // set global variable
  DEBUG = dbg;
  MY_VERSION = version;
  
  if (DEBUG) {
    printf("QUEUE_SIZE: %d\n", QUEUE_SIZE);
    printf("startIndexDirectLinks (first 100 values):\n");
    printValues(100, startIndexDirectLinks);    
  }
  
  // prepare some "quasi-constants"
  int BYTES_PER_ROW = (n - 1) / 8 + 1;
  int LONG_PER_ROW = (BYTES_PER_ROW - 1) / sizeof(long) + 1;
  
  MAX_LINKS_CONNECTED = 2 * n;
  
  // allocate memory for dynamic arrays

  // (pseudo-)two dimensional array where the rows represent the link indices 
  // and each row is an array of long where each bit represents the index of a 
  // connected link. The array is pseudo-two dimensional because in fact it
  // is one dimensional but we calculate the index in this one dimensional
  // array from a row and a column index (using the macro ROW_COL_TO_INDEX.
  long* A = safe_malloc_long("A", n * LONG_PER_ROW * sizeof(long));
  int* queue = safe_malloc("queue", QUEUE_SIZE * sizeof(int));
  int isInQueue[n];

  int queuePosition = 0;
  int queueEnd = 0;
  int linkIndex = 0;  
  int resultIndex = 0;
  
  print_if(DEBUG, "*** Begin of getConnectedLinks_C\n");

  // initialise A with 0
  print_if(DEBUG, "initMatrix");
  initMatrix(A, n, LONG_PER_ROW, 0);
  print_if(DEBUG, "initMatrix ok.");

  // Loop through the array containing information on directly connected links
  // for each link
  for (int i = 0; i < n; i++) {

    showProgressIf(DEBUG, i, n, 1000); // n/100);
    
    // initialise the queue...
    print_if(0, "\n* Initialising queue... ");
    initQueue(&queuePosition, &queueEnd, n, isInQueue);
    print_if(0, "* Initialising queue ok.\n");
    // ... and add the indices of the directly connected links to the queue
      
    print_if(0, "* Adding direct links to queue... ");
    
    addToQueue(
      queue, 
      &queueEnd, 
      safe_get("numberOfDirectLinks", numberOfDirectLinks, i, n), 
      indicesOfDirectLinks, 
      safe_get("startIndexDirectLinks", startIndexDirectLinks, i, n),
      5*n, 
      QUEUE_SIZE,
      isInQueue
    );

    if (ERROR == 1) return 0;

    print_if(0, "* Adding direct links to queue ok.\n");
    
    while ((queueEnd - queuePosition > 0) && queuePosition < QUEUE_SIZE) {
      
      // get the index of the link to be followed (upstream/downstream), from 
      // the queue
      //linkIndex = queue[queuePosition];
      linkIndex = safe_get("queue", queue, queuePosition, QUEUE_SIZE);
      queuePosition++;

      // mark in matrix A that the link with index linkIndex is connected to 
      // the currently considered link i
      //markInMatrix(A, i, linkIndex, n);
      markInMatrix2(A, i, linkIndex, n);
        
      // if we already know all the links to which the link with index linkIndex
      // is connected, mark in A that the same links are also connected to the
      // currently considered link i
      if (i > linkIndex) {
        bitwiseOrOfRows(A, i, linkIndex, n, LONG_PER_ROW);  
      }
      else {

        // add the indices of the links that are directly connected to the
        // currently considered link i, to the queue
        //addToQueue(queue, &queueEnd, numberOfDirectLinks[linkIndex], 
        //  indicesOfDirectLinks, startIndexDirectLinks[linkIndex]);
        addToQueue(
          queue, 
          &queueEnd, 
          safe_get("numberOfDirectLinks", numberOfDirectLinks, linkIndex, n),
          indicesOfDirectLinks, 
          safe_get("startIndexDirectLinks", startIndexDirectLinks, linkIndex, n),
          5*n,
          QUEUE_SIZE,
          isInQueue
        );
      }
      
      if (ERROR == 1) return 0;
      
    } // end of while    
    
    print_if(0, "* Writing indices of connected links to result vector... ");
    
    resultIndex = writeIndicesOfConnectedLinksToResultVector(
      A, i, LONG_PER_ROW, n, numberOfTotalLinks, resultIndex, 
      resultSize, startIndexTotalLinks, indicesOfTotalLinks);
      
    print_if(0, "ok.\n");
    
  } // end of for (i)

  if (resultIndex >= resultSize) {
    printf("resultIndex reached %d! Increase the result vector %s",
      resultSize, "indicesOfTotalLinks!");
    
    printf("Needed length of result vectors: %d.\n", resultIndex + 1);
  }
  else {
    if (DEBUG) {
      printf("\n***\n*** resultIndex: %d\n***\n", resultIndex);
    }
  }

  safe_free(queue, "queue");
  safe_free((int*) A, "A");

  print_if(dbg, "*** End of getConnectedLinks_C\n");
  
  return resultIndex;
}

// C_getDirectLinks
void C_getDirectLinks
(
  int* in_length, 
  char** in_upstreamNodes,
  char** in_downstreamNodes,
  int* out_numberOfFound,
  int* out_indicesOfFound,
  int* inout_maxNumber,
  int* in_dbg
) 
{
  // set global variable
  DEBUG = *in_dbg;
  
  int n = *in_length;
  int maxNumber = *inout_maxNumber;
  
  int indicesOfFound[n * maxNumber];
  int startIndex[n]; // TODO: later as argument to this function
  
  print_if(DEBUG, "*** Begin of C_getDirectLinks\n");
  
  // updates maxNumber with the real maximum number of direct links
  *inout_maxNumber = getDirectLinksFromIDs(n, in_upstreamNodes, 
    in_downstreamNodes, out_numberOfFound, startIndex, indicesOfFound, 
    maxNumber, DEBUG);

  // convert C structure (one dimensional array) to R structure ("pseudo"-
  // matrix) and convert from zero-based (C) to one-based (R) indices
  transform_C_vector_to_R_matrix(n, out_numberOfFound, indicesOfFound,
    out_indicesOfFound);
  
  print_if(DEBUG, "*** v1 End of C_getDirectLinks\n");
}

// getDirectLinksFromIDs: returns maxNumber, the maximum number of direct links 
// that occurred
int getDirectLinksFromIDs
(
  int n, 
  char** upstreamNodes,
  char** downstreamNodes,
  int* numberOfFound,
  int* startIndex, 
  // array of length n containing the first index at which the first link index
  // is stored in indicesOfFound
  int* indicesOfFound, 
  // vector of indices of connected links. The indices of link i+1 follow 
  // directly the indices of link i. You need to interpret the array 
  // numberOfFound in order to relate the indices to the links that they belong 
  // to.
  
  int maxNumber, // expected maximum number of directly connected links
  int dbg
) 
{ 
  // set global variable
  DEBUG = dbg;
  
  int indices[maxNumber]; // array in which indices of links that are direcly 
                          // connected to the current link i are stored
  int realMax = 0;        // real maximum number that occurred
  int resultIndex = 0;    // index in result vector indicesOfFound
  
	print_if(DEBUG, "*** Begin of getDirectLinksFromIDs\n");
  
  int startind = 0;
  
  for (int i = 0; i < n; i++) {
    
    showProgressIf(DEBUG, i, n, n/100);
    
    numberOfFound[i] = findStringInStrings(
      n, upstreamNodes[i], downstreamNodes, indices, maxNumber);
    
    // update real maximum number that occurred
    if (numberOfFound[i] > realMax) {
      realMax = numberOfFound[i];
    }
    
    // copy link indices into result vector
    for (int j = 0; j < numberOfFound[i]; j++) {
      indicesOfFound[resultIndex++] = indices[j];
    }
    
    startIndex[i] = startind;
    startind += numberOfFound[i];
    
	} // end of for(i)
  
  print_if(DEBUG, "*** End of getDirectLinksFromIDs\n");

  return realMax;
}
