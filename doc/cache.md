/--- 8< --
XXX The text below was written before John Plaice answered on 21 Nov
2013 some questions by Ed and Luca. Some relevant answers by
J. Plaice:


Q4. In the Feb 2013 cache semantics paper, why are instructions
find/add/collect in section 14.4 the way they are? Were any formal
methods applied for optimizing some sort of objective function?

J. Plaice answers Q4:
  The add and find functions are defined to be used with a set of
  threads, written w, tagged as lists (the original thread is the
  empty list).  It is possible to convert the denotational semantics
  into an operational semantics which keeps track of the active
  threads. The first attempt to write this down was in my Habilitation
  thesis, available on my Web page, 2010.  Now it would look
  different, but it would be straightforward to write it out.

Comment by Luca:
  He did not mention formal methods so I still suspect the rules are
  heuristic. I should have a look at it, link below for convenience:
    http://plaice.web.cse.unsw.edu.au/archive/JAP/M-10-JAP_HDR.pdf


Q5. In the Feb 2013 cache semantics paper, are instructions
find/add/collect supposed to be executed sequentially or in parallel?

J. Plaice answers Q5:
  So long as the (variable,context) pairs do not clash, several
  parallel finds and adds are possible. Some local locking would be
  necessary.

Comment by Luca:
  Ed, you were right when interpreting the following sentence:
    "Should beta receive more than one beta.find(x,k,w_i,t_i)
    instruction at the same instant beta.ck, the w is randomly chosen
    from the w_i"

Answer of J. Plaice to Q5 continues:
  The collect cannot be run simultaneously. I do have some ideas about
  how this could be done.

Comment by Luca:
  Ahah - he does not want concurrent traversal (with modifications) of
  the data structure. He did not put much thought on GC, he is at the
  same level of us apparently. And still - no formal methods
  mentioned.
\- >8 ----


# Notes on the Feb 2013 paper

## Note 1 - Need of GC is expressed in terms of number of nodes

The first thing to notice in the Feb 2013 paper is that the authors indicate the need for the cache to be garbage collected in terms of number of nodes - and not in terms of actual memory consumption.
This means that the beta.add() instruction does not change the need (if any) of the cache to be garbage collected, as it does not increase the number of nodes but only replaces a calc<w> value with a value (either ground value or list of missing dimensions), even if the memory used by the written value could potentially be bigger than a calc<w> value.

## Note 2 - Node may become GC-able with an age that is already positive

A second thing to notice in the Feb 2013 paper is that:
* The age of a node gamma_j.age is initialized to 0 when the calc<w> is stored;
* If a node holding calc<w> is not garbage collected, its age is incremented;
* beta.add() does not change the age of the updated node.
Therefore, beta.add() could leave the updated node with a positive gamma_j.age.

## Note 3 - Creation of garbage collact-able nodes not garbage collected soon

A third thing to notice in the Feb 2013 paper is that the authors define that each execution of the beta.find() instruction should (1) compute the number of nodes and (2) run beta.collect() before returning.
In particular, beta.collect() is not invoked inside beta.add(), that generates for sure a garbage collect-able node (updating from calc<w> to a value), but inside beta.find(), that could either not write anything or insert a calc<w> node, that is not garbage collect-able. Details to note:
* This behaviour clearly indicates the intent of the authors to adhere to beta.limit as the maximum number of nodes in the cache, considering as nodes all types of nodes (i.e. also calc<w> nodes);
* This behaviour also suggests that the authors wanted to couple the traversal of beta.data in beta.find(), traversal needed for finding the right node corresponding to beta.data(x,k), with the update of the beta.age and gamma_j.age variables, in order to (1) potentially optimize the beta.add() instruction in order to avoid a traversal of beta.data and (2) keep the invocation of beta.collect() right after the update of all the age-related variables.

Therefore, multiple invocations of beta.add() not interleavead by beta.find() would change lots of nodes from calc<w> values to values (either ground values or list of missing dimensions) without triggering a garbage collection of the just created garbage collect-able nodes.
Such an interaction with the cache is feasible as per rule (9), and the number of garbage collect-able nodes generated (from existing calc<w> values) and not yet collected may be higher if multiple threads execute concurrently rule (9) for different (x,k), even in presence of a sequential cache.
Nodes would be garbage collected -if needed- at the invocation of the first beta.find() (even unrelated with the beta.add() invocations).
Rule (9) invokes (wrongly) beta.find() at the end of the rule, anticipating the first invocation of beta.collect() -if needed-, but that call does not structurally change the fact that concurrent evaluations of rule (9) could create a high number of garbage collect-able nodes not yet garbage collected (even in presence of a sequential cache) - it may only be considered a sort of best-effort mitigation.

## Note 4 - beta.collect() invocations are distinct and should traverse multi-tree in order without overtaking each other

A fourth thing to notice in the Feb 2013 paper is about GC:
* When a thread invoking beta.find() discovers that beta.collect() shall be run, it shall run it regardless if another thread is already garbage collecting;
* Multiple threads could garbage collect concurrently, potentially without garbage collecting anything. In particular, if multiple threads evaluate concurrently rule (9) -or better (10)- for distinct variables, they may end up creating lots of nodes containing calc<w>, trying to garbage collect after the creation of each without managing to collect any, and then create lots of garbage-collectable nodes via beta.add() with no one garbage collecting them;
* Multiple invocations of beta.collect() would lead to multiple threads trying to garbage collect but each distinct from the others as potentially using a different beta.age and finding different gamma_j.age. The intention of the authors seems to be that, if no operations incremented beta.age since thread 1 started beta.collect(), thread 2 starting beta.collect() should use the beta.age used by thread 1 decremented by 1 and, in addition to that, it would find gamma_j.age just incremented by thread 1; on this point the Feb 2013 paper is not fully specified as, in order to achieve such behaviour, if thread W1 starts beta.collect() first and thread W2 starts beta.collect() just afterwards, thread W2 shall traverse the multi-tree just 'behind' thread W2, *without overtaking it*.

## Note 5 - Rule (9) is buggy as it may return calc<w>

A fifth thing to notice in the Feb 2013 paper is that rule (9) is wrong, as it could return a calc<w> value.
The problem is that in rule (9) there is a separate final call to beta.find() that assumes that the chain just written by rule (10) -and, ultimately, by rule (11)- was not garbage collected, but this assumption is wrong because of concurrency among threads in the interaction with the cache (even if were sequential).

### Aims of rule (9): Build longest chain, Find value, Reset age of queried chain, Trigger GC

Notes re intents of the authors in rule (9):
* Rule (9) passes the full context to rule (10), suggesting that the authors wanted to compute the longest possible chain hoping to exploit locality (of any sort), even if the dimensions known in rule (9) were not enough for making full use of the built chain and a new invocation of rule (9) were needed in order to exploit a bigger part of the chain;
* Rule (9) throws away the result of rule (10) as the context passed to rule (10) does not reflect the dimensions known by rule (9), hence the need for a (buggy) final invocation of beta.find();
* The consequences of the (buggy) final beta.find() invocation in rule (9) are:
  * (1) Getting the value from the cache (hopefully not calc<w>) with the correct set of known dimensions;
  * (2) Resetting to 0 the age of the queried value (and upstream chain); this consequence may try to cater for the cache activity of concurrent threads that incremented the age of the to-be-queried-by-rule-9 cache node after the beta.find() invocation in rule (11) - age potentially incremented when the node was still containing calc<w>; note also the the invocation of rule (9) in rule (11) could take time;
  * (3) Triggering a garbage collection if needed.
