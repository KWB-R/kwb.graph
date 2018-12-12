#include "connected.h"

const int BITS_PER_LONG = 8*sizeof(long);
const int BITS_PER_LONG_MINUS_1 = 8*sizeof(long) - 1;
const long HIGHEST_LONG_BIT = (1 << (8*sizeof(long) - 1));

int DEBUG = 0;
int MAX_LINKS_CONNECTED;
int MY_VERSION = 0; // a global constant "VERSION"" causes trouble!

char message[1024];

char buffer[1024*1024];

/*
  General Functions
*/

// safe_free
void safe_free(int *pointer, char *name) 
{
  sprintf(message, "deallocating memory of '%s'...", name);
    
  print_if(DEBUG, message);
  
  if (pointer != NULL) {
    free(pointer);
  }
  
  print_if(DEBUG, "ok.\n");
}

// safe_malloc
int* safe_malloc(char* variableName, int size) {
  
  if (DEBUG == 0) {
    return malloc(size);
  }
  
  sprintf(message, "allocate %d Bytes (%0.1f kB) for variable '%s'", 
    size, (double) size / (double) 1024, variableName);
    
  printf("Trying to %s ... ", message);
  
  int *pointer = malloc(size);
  if (pointer == NULL) {
    printf("\nCould not %s!", message);
    ERROR = 1;
  } 
  else {
    printf("ok.\n");    
  }
  return pointer;
}

// safe_malloc
long* safe_malloc_long(char* variableName, int size) {
  
  if (DEBUG == 0) {
    return malloc(size);
  }
  
  sprintf(message, "allocate %d Bytes (%0.1f kB) for variable '%s'", 
  size, (double) size / (double) 1024, variableName);
    
  printf("Trying to %s ... ", message);
  
  long *pointer = malloc(size);
  if (pointer == NULL) {
    printf("\nCould not %s!", message);
    ERROR = 1;
  } 
  else {
    printf("ok.\n");    
  }
  return pointer;
}

// safe_get
int safe_get(char* variableName, int array[], int index, int maxIndex) {
  
  if (DEBUG == 0) {
    return array[index];
  }
  
  int value = 0;
  
  if (index > maxIndex) {
    printf("\nsafe_get(%s): The requested index (%d) exceeds the maximum allowed index (%d)!\n",
      variableName, index, maxIndex);
    ERROR = 1;
  }
  else {
    value = array[index];
    //printf("ok.\n");
  }
  
  return value;
}

// safe_set
void safe_set(char* variableName, int array[], int index, int value, 
  int maxIndex) 
{  
  if (DEBUG == 0) {
    array[index] = value;
    return;
  }
  
  if (index > maxIndex) {
    printf("safe_set(%s): The requested index (%d) exceeds the maximum allowed index (%d)!\n",
      variableName, index, maxIndex);
    ERROR = 1;
    return;
  }
  else {
    array[index] = value;    
    //printf("ok.\n");
  }
}

// print_if 
void print_if (int condition, char* message)
{
  if (condition) {
    puts(message);
  }
}

// showProgressIf
void showProgressIf(int condition, int i, int n, int step)
{
  if (condition && (i % step == 0)) {
    printf("i = %d / %d\n", i, n);
  }
}

// printValues
void printValues(int n, int* values)
{
  printf("values: ");
  for (int i = 0; i < n; i++) {
    printf(" %d", values[i]);
  }
  printf(" (%d)\n", n);  
}

// emptyString
void emptyString(char* string)
{
  strcpy(string, "");
}

// findStringInStrings
int findStringInStrings (
  int n, char* string, char** strings, int* indices, int maxNumber) 
{
  int found = 0;
  
  for (int i = 0; i < n; i++) {
    if (strcmp(string, strings[i]) == 0) {
      assert(found < maxNumber);
      indices[found++] = i;
    }
  }
  
  return found;
}

// longToBitwiseString
void longToBitwiseString(char* buffer, unsigned long value)
{
  emptyString(buffer);
  
  for (int bitNumber = 0; bitNumber < BITS_PER_LONG; bitNumber++) {

    if (bitNumber % 8 == 0) {
      strcat(buffer, " ");
    }
    
    int shiftBy = BITS_PER_LONG_MINUS_1 - bitNumber;
    long bitPattern = (1 << shiftBy);
    
    if ((value & bitPattern) == 0) {
      strcat(buffer, "0");
    }
    else {
      strcat(buffer, "1");          
    }
  }  
}

// maximumOfInt
int maximumOfInt(int* values, int n)
{
  int max = -1;
  
  for (int i = 0; i < n; i++) {
    if ((i == 0) | (values[i] > max)) {
      max = values[i];
    }
  }
  
  return max;
}

/*
  Related to the (Pseudo-) matrix used to store the information which links are
  connected to each other.
*/

// initMatrix
void initMatrix(
  long* A, int numberOfRows, int numberOfColumns, long value
)
{
  for (int i = 0; i < numberOfRows; i++) {
    for (int j = 0; j < numberOfColumns; j++) {
      A[ROW_COL_TO_INDEX(i, j, numberOfRows)] = 0;
    }
  }
}

// markInMatrix
void markInMatrix(long* A, int row, int bitNumber, int numberOfRows)
{  
  int indexOfLong = bitNumber / BITS_PER_LONG;
  int offset = bitNumber % BITS_PER_LONG;
  
  int index = ROW_COL_TO_INDEX(row, indexOfLong, numberOfRows);
  
  // left shift 1 to the position corresponding to bitNumber
  int shiftBy = BITS_PER_LONG_MINUS_1 - offset;
  
  A[index] = A[index] | (1 << shiftBy);
}

// markInMatrix2
void markInMatrix2(long* A, int row, int bitNumber, int numberOfRows)
{  
  int indexOfLong = bitNumber / BITS_PER_LONG;
  
  // modulo calculated manually
  int offset = bitNumber - indexOfLong * BITS_PER_LONG;
  
  int index = ROW_COL_TO_INDEX(row, indexOfLong, numberOfRows);
  
  // OR with bit pattern matching the bitNumber (1 shifted left by 
  // (BITS_PER_LONG_MINUS_1 - offset) bits
  A[index] = A[index] | (1 << (BITS_PER_LONG_MINUS_1 - offset));
}

// bitwiseOrOfRows
void bitwiseOrOfRows(
  long* A, int targetRow, int sourceRow, int numRows, int numColumns)
{
  // array index representing the top row of the current column
  int baseIndex = 0;
  
  for (int column = 0; column < numColumns; column++) {

    int targetIndex = baseIndex + targetRow;
    
    // set matrix element to result of bitwise OR
    A[targetIndex] = A[targetIndex] | A[baseIndex + sourceRow];
    
    baseIndex += numRows; // jump to the top row of the next column
  }
}

// getBitsSet
int getBitsSet(int* bitnumbers, long value)
{
  int numberOfBitsSet = 0;
  long bitPattern = 1;
  
  for (int i = 0; i < BITS_PER_LONG; i++) {
    
    if ((value & bitPattern) != 0) {
      bitnumbers[numberOfBitsSet++] = i;
    }
    
    // shift the "1" in bit pattern one position to the left
    bitPattern = (bitPattern << 1); 
  }
  
  return numberOfBitsSet;
}


// print_matrix
void print_matrix(long* A, int n, int numberOfColumns) 
{
  emptyString(message);
  emptyString(buffer);
  
  print_if(1, "A:");
  
  for (int i = 0; i < n; i++) {
    sprintf(message, "[%8d]: ", i);
    strcat(buffer, message);
    
    for (int j = 0; j < numberOfColumns; j++) {      
      long longValue = A[ROW_COL_TO_INDEX(i, j, n)];
      
      emptyString(message);   
      longToBitwiseString(message, longValue);
      
      strcat(buffer, message);      
    }
    print_if(1, buffer);
    emptyString(buffer);
  }
}

/*
  Related to the queue containing the indices of the links that still need to be
  followed (upstream/downstream)
*/

// initQueue
void initQueue(int *queuePosition, int *queueEnd, int n, int *isInQueue)
{
  *queuePosition = 0;
  *queueEnd = 0;

  if (MY_VERSION == 2) {
    for (int z = 0; z < n; z++) {
      isInQueue[z] = 0;
    }
  }
}

// addToQueue
void addToQueue(int *queue, int *queueEnd, int numIndices, int *indices, 
  int startindex, int maxIndexIndices, int maxIndexQueue, int *isInQueue) 
{
  //printf("addToQueue: numIndices = %d, startindex = %d, *queueEnd: %d\n", numIndices, startindex, *queueEnd);
  for (int i = startindex; i < startindex + numIndices; i++) {
    //queue[*queueEnd] = indices[i];
    
    int value = safe_get("addToQueue::indices", indices, i, maxIndexIndices);
    
    if (MY_VERSION == 1) {
      safe_set("queue", queue, *queueEnd, value, maxIndexQueue);      
      (*queueEnd)++;
    }
    else {
      if (isInQueue[value] == 0) {
        safe_set("queue", queue, *queueEnd, value, maxIndexQueue);
        isInQueue[value] = 1;
        (*queueEnd)++;
      }
    }    
  }
}

// print_queue
void print_queue(int* queue, int queuePosition, int queueEnd)
{
  strcpy(message, "");
  strcpy(buffer, "");
  
  print_if(1, "Queue:");
  
  for (int i = queuePosition; i < queueEnd; i++) {
    sprintf(message, "(%6d)", queue[i]);
    strcat(buffer, message);
  }
    
  print_if(1, buffer);
}

/*
  Helper-functions for high-level functions
*/

// copyIndicesOfConnectedLinks
int copyIndicesOfConnectedLinks(int* indices, int startIndex, long* A, int row, 
  int LONG_PER_ROW, int numberOfRows, int targetSize)
{
  int numberOfBitsSet = 0;
  int baseindex = 0;
  int endBitInColumn = 0;
  
  for (int column = 0; column < LONG_PER_ROW; column++) {
    
    long longValue = A[baseindex + row];

    // see getBitsSet
    unsigned long bitPattern = HIGHEST_LONG_BIT;
    
    for (int i = 0; i < BITS_PER_LONG; i++) {

      if ((longValue & bitPattern) != 0) {
        
        //indices[numberOfBitsSet] = endBitInColumn + i;
        
        int targetIndex = startIndex + numberOfBitsSet;
        
        if (targetIndex < targetSize) {
          indices[targetIndex] = endBitInColumn + i;
        }
        numberOfBitsSet++;
      }
      
      // shift the "1" in bit pattern one position to the right
      // Actively set the highest bit to 0 (~ = bitwise negation) because
      // the right shift introduces a "1" at the highest bit
      bitPattern = (bitPattern >> 1) & (~HIGHEST_LONG_BIT);
    }
    
    baseindex += numberOfRows;
    endBitInColumn += BITS_PER_LONG;
  }
  
  return numberOfBitsSet;
}

// writeIndicesOfConnectedLinksToResultVector ----------------------------------
int writeIndicesOfConnectedLinksToResultVector(
  long* A, int i, int LONG_PER_ROW, int n, int* numberOfTotalLinks, 
  int resultIndex, int resultSize, int* startIndex, int* result_linked)
{
  startIndex[i] = resultIndex;

  numberOfTotalLinks[i] = copyIndicesOfConnectedLinks(result_linked, 
    resultIndex, A, i, LONG_PER_ROW, n, resultSize);
  
  return resultIndex + numberOfTotalLinks[i];
}
