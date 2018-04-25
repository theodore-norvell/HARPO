#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef int_fast8_t Int8 ;
typedef int_fast16_t Int16 ;
typedef int_fast32_t Int32 ;
typedef int_fast64_t Int64 ;

typedef int bool ;
#define true 1
#define false 0

#define MAX 100

// A Continuation is the representation of the state of one Harpo/L thread executing within
// one Harpo/L object.
// A Continuation is a pair consisting of a pointer to a continuation function and a context pointer.
// The continuation function represents one chunk of a Harpo/L thread.  The context
// pointer points to the representation of the thread's object.
// A ContFuncP is a pointer to a continuation function.
// The parameter to a continuation function is a pointer k to a Continuation.
// This k pointer plays two roles:
//    --At the start of the function, its context pointer, k->context, provides the
//      function with the address of the "this" object.
//    --At the end of the function, either the k->fp field is null,
//       which signals to the C-thread that this Harpo/L thread is
//       stuck, or k->fp is not null and points to the next continuation function
//       that the C-thread should execute.  In the latter case, the
//       context pointer, k->context, should point to the "this" object for that
//       continuation function.  
struct Continuation ;

typedef void (*ContFuncP)(struct Continuation *k) ;

typedef struct Continuation {
    // Invariant: If fp != 0 then context!=0.
    ContFuncP fp ;
    void *context ; } Continuation ;

// We assume there is a global heap (or queue, but it need not be FIFO) of
// continuations.  This heap records the states of threads that are ready
// to run.
Continuation *heap[MAX];
int heapIndex = 0; 

// Initialize the Heap
void InitHeap() {
	heapIndex = 0;
	for (int i = 0; i < MAX; ++i) {
		heap[i] = 0;
	}
}

// Get one continuation from the global heap and copy it to *k.
// If there is no continuation available, then the thread waits until there
// is one.
void GetContinuationFromHeap( Continuation *k ) {
	if (heapIndex != 0) {
		// Make the function wait with sleep
		// Don't do this with single-threaded, it'll deadlock.
		if (k->fp == 0 && heap[0] != 0) {		
			k->fp = heap[0]->fp;
			k->context = heap[0]->context;
		
			heap[0]->fp = heap[heapIndex - 1]->fp;
			heap[0]->context = heap[heapIndex - 1]->context;
			free(heap[heapIndex - 1]);
			heap[heapIndex - 1] = 0;
			heapIndex--;
		}
	}
};

// Add a continuation to the global heap. Note that the data must be
// copied out of *k, as k may point to a local variable.
void PutContinuationOnHeap( Continuation *k ) {
	if (heapIndex == MAX) {
		printf("ERROR: Tried to put Continuation on Heap but it was full!");
		fflush(stdout);
	}
	else {
		heap[heapIndex] = malloc(sizeof(Continuation));
		heap[heapIndex]->fp = k->fp;
		heap[heapIndex]->context = k->context;
		heapIndex++;
	}
};


// A ContinuationQ is a queue of continuations. This is protected by a lock.

typedef struct ContinuationQ {
	// state: 0 = empty, 1 = partially filled, 2 = full
	int state;
	int head;
	int tail;
	Continuation *c[MAX];
} ContinuationQ ;

// Initialize the queue
void InitQueue( ContinuationQ *q ) {
	q->state = 0;
	q->head = 0;
	q->tail = 0;
	for (int i = 0; i < MAX; ++i) {
		q->c[i] = 0;
	}
};

bool isEmpty( ContinuationQ *q ) {
	if (q->state == 0) return true;
	return false;
};

// Add a continuation to a FIFO queue of continuations.
void Enqueue( Continuation *k, ContinuationQ *q ) {
	if (q->state == 2) {
		printf("ERROR: Trying to enqueue but ContinuationQ is full!\n");
		fflush(stdout);
	}
	else
	{
		q->c[q->tail] = malloc(sizeof(Continuation));
		q->c[q->tail]->fp = k->fp;
		q->c[q->tail]->fp = k->context;
		q->tail++;
		if (q->tail == MAX) q->tail = 0;
		if (q->tail == q->head) q->state = 2;
		else q->state = 1;
	}
};

// Remove the head continuation from the queue.
void Dequeue( Continuation *k, ContinuationQ *q ) {
	if (q->state != 0) {
		k->fp = q->c[q->head]->fp;
		k->context = q->c[q->head]->context;
		free(q->c[q->head]);
		q->c[q->head] = 0;
		q->head++;
		if (q->head == MAX) q->head = 0;
		if (q->head == q->tail) q->state = 0;
		else q->state = 1;
	}
};

// If there is one, remove the head continuation from the queue and
// put it on the global heap. If the queue is empty, do nothing.
void ScheduleHead( ContinuationQ *q  )
{
	if (!isEmpty(q)) {
		Continuation c;
		Dequeue(&c, q);
		PutContinuationOnHeap(&c);
	}
};

// Atomically reads *p and sets *p to 1.
// Returns the value of *p read.
int testAndSet( volatile int *p ) ;

// Counter object/operation for parallel (co & co for) blocks
// The counter is used to track running threads.  It counts up until it reaches
// the max value (meaning all threads except 1 have completed).  Once the last thread
// counts, the count is set to 0 meaning all threads are now completed.
// At that point the remaining thread proceeds to the merge function.
typedef struct Counter {
	int count ;
	int max ;
} Counter;

// Initializes a new Counter object
// states = number of parallel accesses at one time
// e.g. a basic (co P || Q co) would have 2 states
void InitCounter( int states, Counter *c ) {
	c->count = 0;
	c->max = states - 1;
}

// Progresses the counter by 1 step
// Returns true if there are still threads running
// Returns false if the current thread is the last one running
bool count( Counter *c ) {
	if (c->count < c->max) {
		c->count++;
		return true;
	}
	else if (c->count == c->max) {
		c->count = 0;
		return false;
	}
}

// Stores the information of a procedure
typedef struct ProcStruct {
	volatile int *procLockP ;
	int state ;
	Continuation next ;
	ContinuationQ clientQueue ;
} ProcStruct ;

// Initializes all values in a ProcStruct
void InitProcStruct( ProcStruct *ps ) {
	ps->procLockP = malloc(sizeof(int));
	*(ps->procLockP) = 0;
	ps->state = 0;
	ps->next.context = 0;
	ps->next.fp = 0;
	InitQueue(&(ps->clientQueue));
}

// Lock operations
// I'm assuming that the lock operations somehow ensure sequential
// consistency.  I.e. if thread A obtains a lock L writes x to v and then unlocks L, and
// then thread B locks L, a read of v will result in x and a write to v will overwrite x.

void lock( volatile int *p )
{
	(*p)++;
};

void unlock( volatile int *p ) {
	(*p)--;
};

// The main loop of each C-thread does roughly the following.
// By the way, this technique is known as "trampolining", I
// learned.
void MainLoop() {
    Continuation currentContinuation ;
    currentContinuation.fp = 0 ;
    currentContinuation.context = 0 ;
    
    while( 1 ) {
        if( currentContinuation.fp == 0 )
            GetContinuationFromHeap( & currentContinuation ) ;
		// assert currentContinuation.fp !=0
        // Call the next function. Its job is to execute one chunk of a
        // Harpo thread and to fill currentContinuation.fp either 0 to indicate
        // that the thread must wait, or with the pointer to the function representing
        // the next chunk of this thread (or another thread)
		if ( currentContinuation.fp != 0 )
			(*currentContinuation.fp)( &currentContinuation ) ;
		else break;
    }
}

void initialize() ;

int main() {
	InitHeap() ;
	initialize() ;
    MainLoop() ;
	fflush(stdout);
    return 0 ;
}
typedef struct A_uoutput_{
ProcStruct ps;
Int32 outArg_;
}A_uoutput_;


typedef struct A_{
volatile int procLock ;
Int32 i_ ;
Int32 j_ ;
Int32 k_ ;
Counter cnt_uA_;
A_uoutput_ output_;
} A_;

void FuncThread_uAt0_(Continuation *k);
void FuncThread_uAt1_(Continuation *k);

void c_uA_(A_ *initA_){
initA_->i_ = 0;
initA_->j_ = 0;
initA_->k_ = 0;
InitProcStruct(&(initA_->output_.ps));
Continuation cont_t0;
cont_t0.context = initA_;
cont_t0.fp = &FuncThread_uAt0_;
PutContinuationOnHeap(&cont_t0);
InitCounter(2, &initA_->cnt_uA_);
Continuation cont_t1;
cont_t1.context = initA_;
cont_t1.fp = &FuncThread_uAt1_;
PutContinuationOnHeap(&cont_t1);
}

A_ a_ ;
void initialize(){
c_uA_( &a_ );
}

void FuncThread_uAt0_(Continuation *k);
void FuncCo_uA0_(Continuation *k);
void FuncMerge_uA1_(Continuation *k);
void FuncCall_uA2A_(Continuation *k);
void FuncCall_uA2B_(Continuation *k);
void FuncCo_uA3_(Continuation *k);
void FuncThread_uAt1_(Continuation *k);
void FuncMeth_uAoutput4_(Continuation *k);
void FuncMerge_uA5_(Continuation *k);

void FuncThread_uAt0_(Continuation *k){
A_ *context = (A_*) k->context ;
goto LABEL1;
LABEL1: ;
k->fp = 0 ;return;}

void FuncCo_uA0_(Continuation *k){
A_ *context = (A_*) k->context ;
goto LABEL1;
LABEL1: ;
context->i_=2;
goto LABEL2;
LABEL2: ;
bool stillRunning = count(&context->cnt_uA_);
if (stillRunning) {
k->fp = 0;
return;
} else {
k->fp = &FuncMerge_uA1_;
k->context = context;
return;
}
}

void FuncMerge_uA1_(Continuation *k){
A_ *context = (A_*) k->context ;
  unlock( & context->procLock ) ;
k->fp = 0;
return;goto LABEL1;
LABEL1: ;
context->k_=(context->i_+context->j_);
goto LABEL2;
LABEL2: ;
//create procP here!!!!
A_uoutput_ *procP = &(context->output_);
lock( procP->ps.procLockP ) ;
if(  procP->ps.state == 0 ) {
k->fp = &FuncCall_uA2A_ ;
return;
} else {
Continuation cont ;
cont.context = k->context ;
cont.fp = &FuncCall_uA2A_ ;
Enqueue( &cont, & procP->ps.clientQueue) ;
unlock( procP->ps.procLockP ) ;
 k->fp = 0 ;
return;
}
}

void FuncCall_uA2A_(Continuation *k){
A_ *context = (A_*) k->context ;
goto LABEL1;
LABEL1: ;
//create procP here!!!!
A_uoutput_ *procP = &(context->output_);
procP->outArg_ = context->k_;
LABEL2: ;
procP->ps.state = 1 ;
 Continuation temp = procP->ps.next ;
 procP->ps.next.context = k->context ;
procP->ps.next.fp = &FuncCall_uA2B_;
if( temp.fp == 0 ) {
 unlock( procP->ps.procLockP ) ;
 k->fp = 0 ;
return;
}
else {
*k = temp ;}
}

void FuncCall_uA2B_(Continuation *k){
A_ *context = (A_*) k->context ;
goto LABEL1;
LABEL1: ;
goto LABEL2;
LABEL2: ;
k->fp = 0 ;return;}

void FuncCo_uA3_(Continuation *k){
A_ *context = (A_*) k->context ;
goto LABEL1;
LABEL1: ;
context->j_=2;
goto LABEL2;
LABEL2: ;
bool stillRunning = count(&context->cnt_uA_);
if (stillRunning) {
k->fp = 0;
return;
} else {
k->fp = &FuncMerge_uA1_;
k->context = context;
return;
}
}

void FuncThread_uAt1_(Continuation *k){
A_ *context = (A_*) k->context ;
goto LABEL1;
LABEL1: ;
 lock( & context->procLock );
if (context->output_.ps.state == 1 ) {
k->fp = &FuncMeth_uAoutput4_;
return;
}
context->output_.ps.next.fp = &FuncMeth_uAoutput4_ ;
context->output_.ps.next.context = context ;
unlock( & context->procLock ) ;
k->fp = 0 ;
return ;
}

void FuncMeth_uAoutput4_(Continuation *k){
A_ *context = (A_*) k->context ;
goto LABEL1;
LABEL1: ;
context->output_.ps.state = 2;
printf("%i\n", context->output_.outArg_);
fflush(stdout);
unlock( & context->procLock ) ;
goto LABEL2;
LABEL2: ;
lock( & context->procLock ) ;
PutContinuationOnHeap( & context->output_.ps.next ) ;
context->output_.ps.state = 0 ;
context->output_.ps.next.fp = 0;
if( ! isEmpty( & context->output_.ps.clientQueue ) ) {
context->output_.ps.next.fp = & FuncMerge_uA5_;
context->output_.ps.next.context = k->context;
 Dequeue(k, & context->output_.ps.clientQueue ) ;
return;
} else {
k->fp = & FuncMerge_uA5_ ;
return;
}
k->fp = 0;
return;
}

void FuncMerge_uA5_(Continuation *k){
A_ *context = (A_*) k->context ;
  unlock( & context->procLock ) ;
k->fp = 0;
return;goto LABEL1;
LABEL1: ;
goto LABEL2;
LABEL2: ;
}

