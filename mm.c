/*
 * mm.c
 *
 * Implemented Solution: Explicit free list with constant time
 * coalescing and first fit.
 * 
 */
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "mm.h"
#include "memlib.h"

/* If you want debugging output, use the following macro.  When you hand
 * in, remove the #define DEBUG line. */
#define DEBUG
#ifdef DEBUG
# define dbg_printf(...) printf(__VA_ARGS__)
#else
# define dbg_printf(...)
#endif


/* do not change the following! */
#ifdef DRIVER
/* create aliases for driver tests */
#define malloc mm_malloc
#define free mm_free
#define realloc mm_realloc
#define calloc mm_calloc
#endif /* def DRIVER */

typedef unsigned long long pt64;

/* single word (4) or double word (8) alignment */
#define ALIGNMENT 8

/* rounds up to the nearest multiple of ALIGNMENT */
#define ALIGN(p) (((size_t)(p) + (ALIGNMENT-1)) & ~0x7)

// Private gvars
static char *mem_heap; // Points to the first byte of the heap
char *mem_brk; // points to (last byte of heap)
static pt64 hiBits; // high 32 mem address bits, never change
static pt64 *invalidBits; // Signifies invalid address

// Basic constants and macros
#define WSIZE 4 //Word size in bytes
#define DSIZE 8 //Double word size in bytes (We use this size for words)
#define MINBLOCKSIZE 24 //Includes H,F, and 8 Byte Payload
#define CHUNKSIZE (1<<11) // Default amount to extend heap when needed

#define MAX(x,y) ((x) > (y)? (x) : (y))

//Packs SIZE and free/alloc'd bit into a word
#define PACK(size, alloc) ((size|alloc))
// EXPPACK for explicit list headers & footers
#define EXPPACK(pointer,size,alloc) (((pt64)pointer<<32)\
    |(pt64)((unsigned int)size|(unsigned int)alloc))

// Read/Write word starting at pointer p
#define GET(p) (*(pt64 *)(p))
#define PUT(p, val) (*(pt64 *)(p) = (val))

// Read size or allocated bit from word at address p
#define GET_SIZE(p) (GET(p) & (pt64)~0x7)
#define GET_SIZE_CLEAN(p) (unsigned int)\
    (GET_SIZE(p) & (pt64)0x0000FFFFFFFF)
#define GET_ALLOC(p) (GET(p) & 0x1)

/* Given a pointer to a block (bp), compute block's header or footer address
 Note that we are using DSIZE as our WORD SIZE */
#define HDRP(bp) ((char *)(bp) - DSIZE)
#define FTRP(bp) ((char *)(bp) + GET_SIZE_CLEAN(HDRP(bp)) - 2*DSIZE)

/* Given a block pointer bp, compute a pointer to 
 * the next or previous block (PHYSICAL) */
#define NEXT_BLKP(bp) ((char *)(bp) + GET_SIZE_CLEAN(((char *)(bp) - DSIZE)))
#define PREV_BLKP(bp) ((char *)(bp) - GET_SIZE_CLEAN(((char *)(bp) - 2*DSIZE)))

/* Next & Prev Blocks for an Explicit List 
 * (LINKED but NOT neccessarily ADJACENT) */
#define EXPPTR(p) (pt64 *) ((((pt64)(GET(p)))>>32))
#define NXTEXP(bp) (pt64*)\
    ((hiBits)|(pt64)(EXPPTR(HDRP(bp))))
#define PRVEXP(bp) (pt64*)\
    ((hiBits)|(pt64)(EXPPTR(FTRP(bp))))

/* Given a payload pointer to a block that has just been allocated,
 * handle removing that block from the explicit free list */
void remFromFreelist(void *bp)
{
    pt64 *nextFoot = (pt64*)FTRP(NXTEXP(bp));
    pt64 *prevHead = (pt64*)HDRP(PRVEXP(bp));

    pt64 *toRemovePrev = PRVEXP(bp);
    pt64 *toRemoveNext = NXTEXP(bp);

    PUT(nextFoot, EXPPACK(toRemovePrev,
        GET_SIZE_CLEAN(nextFoot),GET_ALLOC(nextFoot)));
    PUT(prevHead, EXPPACK(toRemoveNext,
        GET_SIZE_CLEAN(prevHead),GET_ALLOC(prevHead)));
}

/* Adds a free block to the front of the free list (connected to
 * the prologue header, with the new block's footer connected to
 * the prologue's footer */
void addToFreeList(void *bp, size_t size, int alloc)
{
    // header of block where the prologue header is pointing
    pt64 *oldNext = NXTEXP(mem_heap);
    
    // Make the new block's header point to the next one
    PUT(HDRP(bp),EXPPACK(oldNext,size,alloc));

    // point the footer of the old next block to the new one
    PUT(FTRP(oldNext),EXPPACK(bp,GET_SIZE_CLEAN(
        HDRP(oldNext)),GET_ALLOC(HDRP(oldNext))));

    // point prologue block header to new
    PUT(HDRP(mem_heap),EXPPACK(bp,GET_SIZE_CLEAN(HDRP(mem_heap)),1));

    // point new footer to prologue footer
    PUT(FTRP(bp),EXPPACK(mem_heap,size,alloc));
}

/* Determines where to put a payload in the list
 * CURRENTLY: FIRST FIT */
static void *find_fit(size_t asize)
{
    void *bp;

    for (bp = mem_heap; 
        (NXTEXP(bp))!=(pt64*) hiBits; bp = NXTEXP(bp))
    {
        if (!GET_ALLOC(HDRP(bp)) && (asize <= GET_SIZE_CLEAN(HDRP(bp))))
        {
            return bp;
        }
    }

    return NULL; // There was no fit.
}

/* Update the list, since we are putting something in the heap
 * splits blocks if there is space to do so */
static void place(void *bp, size_t asize)
{
    size_t csize = GET_SIZE_CLEAN(HDRP(bp));
    pt64* prev = PRVEXP(bp);
    pt64* next = NXTEXP(bp);

    if ((csize - asize) >= (MINBLOCKSIZE))
    {
        PUT(HDRP(bp), EXPPACK(next, asize, 1));
        PUT(FTRP(bp), EXPPACK(prev, asize, 1));
        remFromFreelist(bp);

        //add back the free space
        bp = (pt64*) ((pt64)bp 
            + (pt64)asize);
        addToFreeList(bp, (csize-asize), 0);
    }
    else
    {
        PUT(HDRP(bp), EXPPACK(next, csize, 1));
        PUT(FTRP(bp), EXPPACK(prev, csize, 1));
        remFromFreelist(bp);
    }
}

static void *coalesce(void *bp)
{
    pt64* prev = (pt64*)PREV_BLKP(bp);
    pt64* next = (pt64*)NEXT_BLKP(bp);
    size_t prev_alloc = GET_ALLOC(HDRP(prev));
    size_t next_alloc = GET_ALLOC(HDRP(next));
    size_t size = GET_SIZE_CLEAN(HDRP(bp));

    if (prev_alloc && next_alloc) // Case 1
    { 
        // Can't do anything
        addToFreeList(bp, size, 0);
    }

    else if (prev_alloc && !next_alloc) // Case 2
    { 
        // Remove next block and add to front
        size += GET_SIZE_CLEAN(HDRP(next));
        remFromFreelist(next);
        addToFreeList(bp, size, 0);
    }

    else if (!prev_alloc && next_alloc) // Case 3
    {
        // Remove the prev block from free list and add to the front
        size += GET_SIZE_CLEAN(HDRP(prev));
        remFromFreelist(prev);
        bp = prev;
        addToFreeList(bp, size, 0);
    }

    else // Case 4
    {
        size += GET_SIZE_CLEAN(HDRP(prev)) +
            GET_SIZE_CLEAN(FTRP(next));
        // Get rid of both blocks and re-add the space at the front
        remFromFreelist(prev);
        remFromFreelist(next);
        bp = prev;
        addToFreeList(bp, size, 0);
    }

    return bp;
}

/* Wrapper for the mem_sbrk function to extend the heap and keep it
 * boundary aligned, unfragmented, and consistent (invariant maintanence) */
static void *extend_heap(size_t words)
{
    pt64 *bp = (pt64*)
        ((void*)(mem_sbrk(words)) - DSIZE);
    pt64 *prev = PRVEXP(bp);
    pt64 *next = (pt64*)((void*)bp + words);
    int quad = 2*DSIZE;

    PUT(HDRP(bp), EXPPACK(next, words, 0));
    PUT(FTRP(bp), EXPPACK(prev, words, 0));
    //Take care of Epilogue block
    PUT(HDRP(next), EXPPACK(invalidBits, quad, 1));
    PUT(FTRP(next), EXPPACK(bp, quad, 1));
    remFromFreelist(bp);

    return coalesce(bp);
}

/*
 * Initialize: return -1 on error, 0 on success.
 */
int mm_init(void) {
    /* Initialize gvars */
    mem_heap = mem_heap_lo();
    mem_brk = mem_heap_hi();
    hiBits = (((pt64)mem_heap >> 32)<<32);
    invalidBits = 0;
    // Initial H&F pointers
    pt64 *epilogue = (pt64*)(mem_heap + 3*DSIZE);
    pt64 *prologue = (pt64*)(mem_heap + 1*DSIZE);

    /* Create the initial empty heap */
    if ((mem_heap = mem_sbrk(4*DSIZE)) == (void *)-1) return -1;
    mem_heap += DSIZE;
    
    // PROLOGUE block HEADER
    PUT(HDRP(mem_heap), EXPPACK(epilogue,2*DSIZE,1));
    // PROLOGUE block FOOTER
    PUT(FTRP(mem_heap), EXPPACK(invalidBits,2*DSIZE, 1));
    // EPILOGUE block HEADER
    PUT(HDRP(epilogue), EXPPACK(invalidBits,2*DSIZE, 1));
    // EPILOGUE block FOOTER
    PUT(FTRP(epilogue), EXPPACK(prologue,2*DSIZE, 1));

    /* Extend empty heap w/ free block of CHUNKSIZE bytes */
    if (extend_heap(CHUNKSIZE) == NULL) return -1;
    
    return 0;
}

/*
 * malloc
 */
void *malloc (size_t size) {
    size_t asize; /* Adjusted block size */
    size_t extendsize; /* Amount to extend heap if no fit */
    char *bp;

    /* Ignore spurious requests */
    if (size == 0) return NULL;

    /* Adjust block size to include overhead and alignment reqs. */
    if (size <= DSIZE) asize = DSIZE;
    else asize = ALIGN(size); // H&F Space

    asize += 2*DSIZE;

    /* Search the free list for a fit */
    if ((bp = find_fit(asize)) != NULL)
    {
        place(bp, asize);
        return bp;
    }

    /* No fit found. Get more memory and place the block */
    extendsize = MAX(asize,CHUNKSIZE);
    if ((bp = extend_heap(extendsize)) == NULL) return NULL;
    place(bp, asize);
    return bp;
}

/*
 * free
 */
void free (void *ptr) {
    if (ptr==NULL) return;
    /* coalesce frees ptr no matter what, and perhaps merges it with
     * more free space */
    coalesce(ptr);
}

/*
 * realloc - Attempts to assign new data of potentially different size
 * to the given starting address. If this is impossible, the data is placed
 * according to the place(size) function.
 */
void *realloc(void *oldptr, size_t size) {
    size_t oldsize;
    void *newptr;

    // If size is 0 then free and return NULL
    if (size == 0)
    {
        free(oldptr);
        return NULL;
    }

    // If oldptr is NULL then simply malloc
    if (oldptr == NULL)
    {
        return malloc(size);
    }

    // Begin realloc routine proper
    
    /* perhaps rewrite this to see if there is currently space to in-place
     * replace the block with new memory */

    newptr = malloc(size);

    // Check to see if realloc() failed
    if (newptr == NULL)
    {
        return NULL;
    }

    // Otherwise malloc'd succesfully, copy data
    oldsize = GET_SIZE(HDRP(oldptr));
    if (size < oldsize) oldsize = size;
    memcpy(newptr, oldptr, oldsize);

    // now free the old block
    free(oldptr);

    return newptr;
}

/*
 * Thin wrapper around malloc to initialize memory
 */

void *calloc (size_t nmemb, size_t size) {
    size_t bytesToAlloc = nmemb*size;
    void *newptr;

    newptr = malloc(bytesToAlloc);
    memset(newptr, 0, bytesToAlloc);

    return newptr;
}


/*
 * Return whether the pointer is in the heap.
 * May be useful for debugging.
 */
//static int in_heap(const void *p) {
//    return p <= mem_heap_hi() && p >= mem_heap_lo();
//}

/*
 * Return whether the pointer is aligned.
 * May be useful for debugging.
 */
//static int aligned(const void *p) {
//    return (size_t)ALIGN(p) == (size_t)p;
//}