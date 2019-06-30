#!/usr/bin/env bash
# ssh into the remote machine
# start tmux by typing tmux into the shell
# start the process you want inside the started tmux session
# leave/detach the tmux session by typing Ctrl+b and then d

# When you come back again and want to check the status of your process you
# can use tmux attach to attach to your tmux session.
cd ./scripts/ts/
tsc
cd ../..
sbt "train" |& tee console.txt;sbt "runMain lambdanet.utils.ReportFinish"

