# LambdaNet

![LambdaNet-logo](images/Logo.png)


### Why Typescript?
The most popular option for adding types to Javascript codebases. Has a large number of real-world projects. Its optional gradual typing approach seems to be the simplest among different gradual typing paradigms. Also has a clear theoretical foundation (Siek et al).

### Typing Objective
Maximize the number of correct (user-annotated) type annotations.

### Why use machine learning?
 * Typescript has a very expressive and complex type system (to stay compatible with Javascript) that makes type inference a very challenging task. There is no efficient type inference algorithm for TS that is both sound and complete. 
 * No principle typing.
 * A human programmer can usually guess the correct types using naming information, textual documentation, and probabilistic reasoning (recognizing familiar code pattern). A machine learning algorithm might also learn to utilize these features.

### Tentative Solution
 * Encode Javascript programs as various typing constraints imposed on unknown variable types. 
 * Aggregate these constraints using a graph neural network. 
 * Use character-level naming information as additional features. 
 * Decode Typescript types from the vector embedding output by the GNN.


### Formalizations
See [typingNet-Formalizations.pdf](typingNet-Formalizations.pdf).


### Where are things
**Surface Language**: [GStmt](src/main/scala/gtype/GStmt.scala), [GExpr]([GStmt](src/main/scala/gtype/GExpr.scala))

**Intermediate Language**: [IR]([GStmt](src/main/scala/infer/IR.scala)), [IRTranslation]([GStmt](src/main/scala/infer/IRTranslation.scala))

**Relation Graph**: [RelationGraph](src/main/scala/infer/RelationGraph.scala)

**Graph Neural Network Embedding/Decoding**: [GraphEmbedding](src/main/scala/infer/GraphEmbedding.scala)

**Training Loop**: [TrainingCenter](src/main/scala/infer/TrainingCenter.scala)

### Current Progress

#### Fine-tuning training loop and neural architecture

Todo:

- [ ] translate real-world TS projects into our DSL

- [ ] implement predicate-level loss

Done:

- [X] make the model capable of overfitting to a single program

- [X] investigate abnormal jumps in training loss 

(solution: updating with an RNN unit)

- [X] implement the training loop

#### Inference Graph Construction

- [X] implement type decoding from predicate graphs

- [X] implement user-defined type encoding

- [X] implement graph embedding of predicate graphs

- [X] generate predicate graph from IR

- [X] implement an IR in single assignment form (SAF)

- [X] translate programs ([GStmt](src/main/scala/gtype/GStmt.scala)) 
into IR

- [X] trace type variables back into source code