package Lambdanet;

import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ForkJoinWorkerThread;

public class LambdanetForkJoinWorkerThreadFactory implements ForkJoinPool.ForkJoinWorkerThreadFactory {

    @Override
    public final ForkJoinWorkerThread newThread(ForkJoinPool pool) {
        return new LambdanetForkJoinWorkerThread(pool);
    }

    private static class LambdanetForkJoinWorkerThread extends ForkJoinWorkerThread {

        private LambdanetForkJoinWorkerThread(final ForkJoinPool pool) {
            super(pool);
            setContextClassLoader(this.getClass().getClassLoader());
        }
    }
}


