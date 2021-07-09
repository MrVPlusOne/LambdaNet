package driver;


import ammonite.ops.Path;
import lambdanet.JavaAPI$;
import lambdanet.Model;
import lambdanet.TypeInferenceService;
import lambdanet.TypeInferenceService$;
import lambdanet.train.TopNDistribution;
import lambdanet.translation.PredicateGraph;
import scala.collection.immutable.Map;

public class JavaDriver {
    public static void main(String[] args) {
        JavaAPI$ api = lambdanet.JavaAPI$.MODULE$;
        TypeInferenceService$ typeInfer = TypeInferenceService$.MODULE$;
        Path workDir = api.pwd();

        Path modelDir = api.joinPath(workDir,
                "models/newParsing-GAT1-fc2-newSim-decay-6");
        Path paramPath = api.joinPath(modelDir, "params.serialized");
        Path modelCachePath = api.joinPath(modelDir, "model.serialized");
        TypeInferenceService.ModelConfig modelConfig = api.defaultModelConfig();
        Path parsedReposDir = api.joinPath(workDir, "data/parsedRepos");

        Model model = typeInfer.loadModel(paramPath, modelCachePath, modelConfig, 8, parsedReposDir);
        Model.PredictionService predService = api.predictionService(model, 8, 5);
        System.out.println("Type Inference Service successfully started.");
        System.out.println("Current working directory: " + workDir);

        for (int i = 0; i < 1000; i++) {
            // limit max loop iterations to 1000 in case something wrong happens
            System.out.print("Enter project path: ");
            System.out.flush();
            String line = api.readLine();
            try {
                assert !line.isEmpty() : "Specified path should not be empty.";
                Path sourcePath = line.startsWith("/") ?
                        api.absPath(line) :
                        api.joinPath(workDir, line);
                String[] skipSet = {"node_modules"};
                Map<PredicateGraph.PNode, TopNDistribution<PredicateGraph.PType>> results =
                        predService.predictOnProject(sourcePath, false, skipSet);
                new TypeInferenceService.PredictionResults(results).prettyPrint();
                System.out.println("DONE");
            } catch (Throwable e) {
                System.out.println("Got exception: " + e.getMessage());
                e.printStackTrace();
            }
        }
        var r = api.returnMissing();
    }
}
