package driver;


import ammonite.ops.Path;
import funcdiff.SimpleMath$;
import lambdanet.JavaAPI$;
import lambdanet.Model;
import lambdanet.TypeInferenceService;
import lambdanet.TypeInferenceService$;

public class JavaDriver {
    public static void main(String[] args) {
        JavaAPI$ api = lambdanet.JavaAPI$.MODULE$;
        Path workDir = api.pwd();

        Path modelDir = api.joinPath(workDir,
                "models/newParsing-GAT1-fc2-newSim-decay-6");
        Path modelCachePath = api.joinPath(modelDir, "model.serialized");

        Model model = (Model) SimpleMath$.MODULE$.readObjectFromFile(modelCachePath);
        Model.PredictionService predService = api.predictionService(model, 8, 5);
        System.out.println("Type Inference Service successfully started.");
        System.out.println("Current working directory: " + workDir);

        for (int i = 0; i < 1000; i++) {
            // limit max loop iterations to 1000 in case something wrong happens
            System.out.print("Enter project path: ");
            System.out.flush();
            String line = api.readLine();
            try {
                assert !line.trim().isEmpty() : "Specified path should not be empty.";
                Path sourcePath = line.startsWith("/") ?
                        api.absPath(line) :
                        api.joinPath(workDir, line);
                String[] skipSet = {"node_modules"};
                TypeInferenceService.PredictionResults results =
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
