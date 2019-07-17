#!/usr/bin/env bash

#scp data/predicateGraphs.serialized titan4:~/TypingNet/data/predicateGraphs.serialized;
echo "upload to utopia2...";
scp data/predicateGraphs.serialized utopia2:~/TypingNet/data/predicateGraphs.serialized;
echo "upload to utopia1...";
scp data/predicateGraphs.serialized utopia1:~/TypingNet/data/predicateGraphs.serialized;