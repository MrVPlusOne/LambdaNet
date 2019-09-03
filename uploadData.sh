#!/usr/bin/env bash

#scp data/predicateGraphs.serialized titan4:~/TypingNet/data/predicateGraphs.serialized;
echo "upload to utopia1...";
scp -r data/parsedRepos utopia1:~/TypingNet/data/;
echo "upload to utopia2...";
scp -r data/parsedRepos utopia2:~/TypingNet/data/;