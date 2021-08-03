<img src="images/Header.png" width="800" alt="LambdaNet Header"/>

This is the source code repo for the ICLR paper [*LambdaNet: Probabilistic Type Inference using Graph Neural Networks*](https://openreview.net/forum?id=Hkx6hANtwH). For an overview of how LambdaNet works, see [our video from ICLR 2020](https://iclr.cc/virtual_2020/poster_Hkx6hANtwH.html).

This branch contains the latest improvement and features. To produce the results presented by the paper, please see the `ICLR20` branch.

## Instructions
After cloning this repo, here are the steps to reproduce our experimental results:

 1. Install all the dependencies (Java, sbt, TypeScript, etc.) See the "Using Docker" section below.
    1. If you are not using docker, you will need to either set the environment variable `OMP_NUM_THREADS=1` or prefix all the commands in the following step with `export OMP_NUM_THREADS=1;`.
 2. To run pre-trained model
    1. download the model using [this link (predicts user defined type)](https://drive.google.com/file/d/13guFN8sDxPCqK_iWvgn3_4uQah5HlH34/view?usp=sharing) and unzip the file. 
    2. To run the model in interactive mode, which outputs `(source code position, predicted type)` pairs for the specified files:
        1. If not exits, create the file `<project root>/configs/modelPath.txt` and write the location of the model directory into it. The location should be the directory that directly contains the `model.serialized` file.
        2. Under project root, run `sbt "runMain lambdanet.TypeInferenceService"`.
        3. After it finishes loading the model into memory, simply enter the directory path that contains the TypeScript files of interest. Note that in this version of LambdaNet, the model will take any existing user type annotations in the source files as part of its input and only predict the type for the locations where a type annotation is missing.
        4. Note that currently, LambdaNet only works with TypeScript files and assumes the files follow a TypeScript project structure, so if you want to run it on JavaScript files, you will need to change the file extensions to `.ts`.
    3. Alternatively, to run the model in batched mode, which outputs human-readable HTML files and accuracy statistics, use the code from the `ICLR20` Git branch.
 3. To train LambdaNet from scratch 
    1. Download the TypeScript projects used in our experiments and prepare the TS projects into a serialization format. This can be achieved using the `main` function defined in `src/main/scala/lambdanet/PrepareRepos.scala`. Check the source code and make sure you uncomment all the steps (Some steps may have been commented out), then run it using `sbt "runMain lambdanet.PrepareRepos`.
    3. Check the `main` defined in `src/main/scala/lambdanet/train/Training.scala` to adjust any training configuration if necessary, then run it using `sbt "runMain lambdanet.train.Training"`.

The TypeScript files used for manual comparison with JSNice are put under the directory `data/comparison/`.


### Using Docker
We also provide a Docker file to automatically download and install all the dependencies. (If on linux, you can also install the dependencies into your system by manually running the commands defined inside `Dockerfile`.) Here are the steps to run pre-trained LambdaNet model inside a Docker Container: 

  1. First, make sure you have [installed Docker](https://www.docker.com/get-started).
  
  2. Put pre-trained model weights somewhere accessible to the docker instance (e.g., inside the lambdanet project root). Then config the model path following the instruction above (instruction 2.2.1).
   
  3. Under project root, run `docker build -t lambdanet:v1 .
  && docker run --name lambdanet --memory 14g -t -i lambdanet:v1 `. (Make sure the machine you are using has enough memory for the `docker run` command.)

  4. After the Docker container has successfully started, follow the steps described above.
  
