# TypingNet

Deep learning-based type inference for Javascript/Typescript


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


### Current Progress
See [Agenda](Agenda.md).

### Formalizations
See [typingNet-Formalizations.pdf](typingNet-Formalizations.pdf).


### Where are things
**Surface Language**: [GStmt](src/main/scala/gtype/GStmt.scala), [GExpr]([GStmt](src/main/scala/gtype/GExpr.scala))

**Intermediate Language**: [IR]([GStmt](src/main/scala/infer/IR.scala)), [IRTranslation]([GStmt](src/main/scala/infer/IRTranslation.scala))

**Relation Graph**: [RelationGraph](src/main/scala/infer/RelationGraph.scala)

**Graph Neural Network Embedding**: TODO

**Type Decoding**: TODO
