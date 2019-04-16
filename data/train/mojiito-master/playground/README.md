# Playground

* First build Mojiito by executing `npm run build` in your command line tool
* Now run `npm run build-playground` to build the playground
* Or `npm run serve-playground` to start a server

## Troubleshooting

#### Error `listen EADDRINUSE 0.0.0.0:4200`
Sometimes if you start the server the following Error occurs: `Error: listen EADDRINUSE 0.0.0.0:4200`.
This happens if there is already a server running on Port `4200` or you didn't quit the process correctly (e.g. Ctrl+Z instead of Ctrl+C).    
You can check for running processes on this port by executing `lsof -i tcp:4200 ` in your command line tool.    
If there are processes up, please close them (as a last ressort you can kill all node processes with `killall -9 node` 
or just a specific one with `killall -15` and the process id)