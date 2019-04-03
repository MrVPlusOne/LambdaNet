#!/usr/bin/env bash
# ssh into the remote machine
# start tmux by typing tmux into the shell
# start the process you want inside the started tmux session
# leave/detach the tmux session by typing Ctrl+b and then d

# When you come back again and want to check the status of your process you
# can use tmux attach to attach to your tmux session.

export JAVA_OPTS="-Xms1G -Xmx4G -Dorg.bytedeco.javacpp.maxbytes=20G -Dorg.bytedeco.javacpp.maxphysicalbytes=28G";sbt "runMain infer.TrainingCenter" |& tee console.txt;sbt "runMain infer.ReportFinish"

