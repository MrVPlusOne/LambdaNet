#!/usr/bin/env bash

echo "Compressing parsed repo chunks..."
zip parsedRepos-no_any.zip -r data/parsedRepos-no_any
echo "Uploading to AWS..."
scp -r parsedRepos-no_any.zip aws:~/Jiayi/LambdaNet/
