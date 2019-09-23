#!/usr/bin/env bash

echo "upload to lambdanet1...";
gcloud compute scp --recurse data/parsedRepos wjydzh1@lambdanet1:~/TypingNet/data/;