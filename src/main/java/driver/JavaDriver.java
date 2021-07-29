package driver;


import funcdiff.SimpleMath$;
import lambdanet.Model;

public class JavaDriver {
    public static void main(String[] args) {
        var api = lambdanet.JavaAPI$.MODULE$;
        var workDir = api.pwd();

        var modelDir = api.joinPath(workDir,
                "models/newParsing-GAT1-fc2-newSim-decay-6");
        var modelCachePath = api.joinPath(modelDir, "model.serialized");

        var model = (Model)SimpleMath$.MODULE$.readObjectFromFile(modelCachePath);
        var predService = api.predictionService(model, 8, 5);
        System.out.println("Type Inference Service successfully started.");
        System.out.println("Current working directory: " + workDir);

        for (int i = 0; i < 1000; i++) {
            // limit max loop iterations to 1000 in case something wrong happens
            System.out.print("Enter project path: ");
            System.out.flush();
            var line = api.readLine();
            try {
                assert !line.strip().isEmpty() : "Specified path should not be empty.";
                var sourcePath = line.startsWith("/") ?
                        api.absPath(line) :
                        api.joinPath(workDir, line);
                String[] skipSet = {"node_modules"};
                var results =
                        predService.predictOnProject(sourcePath, false, skipSet);
                results.prettyPrint();
                System.out.println("DONE");
            } catch (Throwable e) {
                System.out.println("Got exception: " + e.getMessage());
                e.printStackTrace();
            }
        }
    }
}
