#!/usr/bin/env bash

#scp data/predicateGraphs.serialized titan4:~/TypingNet/data/predicateGraphs.serialized;
echo "upload to utopia1...";
scp data/parsedDataSet.serialized utopia1:~/TypingNet/data/parsedDataSet.serialized;
echo "upload to utopia2...";
scp data/parsedDataSet.serialized utopia2:~/TypingNet/data/parsedDataSet.serialized;