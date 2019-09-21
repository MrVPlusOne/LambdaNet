// Same for Eval

import {
  ICancelable,
  Cancelable,
  StackedCancelable,
  Scheduler,
  Future, ExecutionModel,
  execInternals, Duration
} from "funfix-exec"

export class IO<A> {
  /**
   * Triggers the asynchronous execution.
   *
   * Without invoking `run` on a `IO`, nothing gets evaluated, as an
   * `IO` has lazy behavior.
   *
   * ```typescript
   * // Describing a side effect
   * const io = IO.of(() => console.log("Hello!"))
   *   // Delaying it for 1 second, for didactical purposes
   *   .delayExecution(1000)
   *
   * // Nothing executes until we call run on it, which gives
   * // us a Future in return:
   * const f: Future<void> = io.run()
   *
   * // The given Future is cancelable, in case the logic
   * // decribed by our IO is cancelable, so we can do this:
   * f.cancel()
   * ```
   *
   * Note that `run` takes a
   * [Scheduler](https://funfix.org/api/exec/classes/scheduler.html)
   * as an optional parameter and if one isn't provided, then the
   * default scheduler gets used. The `Scheduler` is in charge
   * of scheduling asynchronous boundaries, executing tasks
   * with a delay (e.g. `setTimeout`) or of reporting failures
   * (with `console.error` by default).
   *
   * Also see {@link IO.runOnComplete} for a version that takes a
   * callback as parameter.
   *
   * @return a `Future` that will eventually complete with the
   *         result produced by this `IO` on evaluation
   */
  run(ec: Scheduler = Scheduler.global.get()): Future<A> {
    return taskToFutureRunLoop(this, ec)
  }

  /**
   * Triggers the asynchronous execution.
   *
   * Without invoking `run` on a `IO`, nothing gets evaluated, as an
   * `IO` has lazy behavior.
   *
   * `runComplete` starts the evaluation and takes a callback which
   * will be triggered when the computation is complete.
   *
   * Compared with JavaScript's `Promise.then` the provided callback
   * is a function that receives a
   * [Try](https://funfix.org/api/core/classes/try.html) value, a data
   * type which is what's called a "logical disjunction", or a "tagged
   * union type", a data type that can represent both successful
   * results and failures. This is because in Funfix we don't work
   * with `null`.
   *
   * Also the returned value is an
   * [ICancelable](https://funfix.org/api/exec/interfaces/icancelable.html)
   * reference, which can be used to cancel the running computation,
   * in case the logic described by our `IO` is cancelable (note that
   * some procedures cannot be cancelled, it all depends on how the
   * `IO` value was described, see {@link IO.async} for how cancelable
   * `IO` values can be built).
   *
   * Example:
   *
   * ```typescript
   * // Describing a side effect
   * const io = IO.of(() => console.log("Hello!"))
   *   .delayExecution(1000)
   *
   * // Nothing executes until we explicitly run our `IO`:
   * const c: ICancelable = io.runOnComplete(r =>
   *   r.fold(
   *     err => console.error(err),
   *     _ => console.info("Done!")
   *   ))
   *
   * // In case we change our mind and the logic described by
   * // our `IO` is cancelable, we can cancel it:
   * c.cancel()
   * ```
   *
   * Note that `runOnComplete` takes a
   * [Scheduler](https://funfix.org/api/exec/classes/scheduler.html)
   * as an optional parameter and if one isn't provided, then the
   * default scheduler gets used. The `Scheduler` is in charge
   * of scheduling asynchronous boundaries, executing tasks
   * with a delay (e.g. `setTimeout`) or of reporting failures
   * (with `console.error` by default).
   *
   * Also see {@link IO.run} for a version that returns a `Future`,
   * which might be easier to work with, especially since a `Future`
   * is `Promise`-like.
   *
   * @param cb is the callback that will be eventually called with
   *        the final result, or error, when the evaluation completes
   *
   * @param ec is the scheduler that controls the triggering of
   *        asynchronous boundaries (e.g. `setTimeout`)
   *
   * @return a cancelable action that can be triggered to cancel
   *         the running computation, assuming that the implementation
   *         of the source `IO` can be cancelled
   */
  runOnComplete(
    cb: (result: Try<A>) => void,
    ec: Scheduler = Scheduler.global.get()): ICancelable {

    const ref = ioGenericRunLoop(this, ec, null, cb, null, null, null)
    return ref || Cancelable.empty()
  }


  /**
   * Ensures that an asynchronous boundary happens before the
   * execution, managed by the provided scheduler.
   *
   * Alias for {@link IO.fork}.
   *
   * Calling this is equivalent with:
   *
   * ```typescript
   * IO.shift(ec).flatMap(_ => self)
   *
   * // ... or ...
   *
   * IO.shift(ec).followedBy(self)
   * ```
   *
   * See {@link IO.fork}, {@link IO.asyncBoundary} and {@link IO.shift}.
   */
  executeForked(ec?: Scheduler): IO<A> {
    return IO.fork(this, ec)
  }

  /**
   * Override the `ExecutionModel` of the default scheduler.
   *
   * ```typescript
   * import { ExecutionModel } from "funfix"
   *
   * io.executeWithModel(ExecutionModel.alwaysAsync())
   * ```
   */
  executeWithModel(em: ExecutionModel): IO<A> {
    return IO.asyncUnsafe<A>((ctx, cb) => {
      const ec = ctx.scheduler.withExecutionModel(em)
      const ctx2 = new IOContext(ec, ctx.connection, ctx.options)
      ec.trampoline(() => IO.unsafeStart(this, ctx2, cb))
    })
  }

  map<B>(f: (a: A) => B): IO<B> {
    return new IOFlatMap(this, (a: A) => IO.now(f(a)))
  }

  /**
   * Memoizes (caches) the result of the source `IO` and reuses it on
   * subsequent invocations of `run`.
   *
   * The resulting task will be idempotent, meaning that
   * evaluating the resulting task multiple times will have the
   * same effect as evaluating it once.
   *
   * @see {@link IO.memoizeOnSuccess} for a version that only caches
   *     successful results.
   */
  memoize(): IO<A> {
    switch (this._tag) {
      case "pure":
        return this
      case "always":
        const always = (this as any) as IOAlways<A>
        return new IOOnce(always.thunk, false)
      case "memoize":
        const mem = (this as any) as IOMemoize<A>
        if (!mem.onlySuccess) return mem
        return new IOMemoize(this, false)
      default: // flatMap | async
        return new IOMemoize(this, false)
    }
  }

  /**
   * Memoizes (caches) the successful result of the source task
   * and reuses it on subsequent invocations of `run`.
   * Thrown exceptions are not cached.
   *
   * The resulting task will be idempotent, but only if the
   * result is successful.
   *
   * @see {@link IO.memoize} for a version that caches both successful
   *     results and failures
   */
  memoizeOnSuccess(): IO<A> {
    switch (this._tag) {
      case "pure":
      case "once":
      case "memoize":
        return this
      case "always":
        const always = (this as any) as IOAlways<A>
        return new IOOnce(always.thunk, true)
      default: // flatMap | async
        return new IOMemoize(this, true)
    }
  }

  /**
   * Creates a new `IO` that will mirror the source on success,
   * but on failure it will try to recover and yield a successful
   * result by applying the given function `f` to the thrown error.
   *
   * This function is the equivalent of a `try/catch` statement,
   * or the equivalent of {@link IO.map .map} for errors.
   *
   * ```typescript
   * io.recover(err => {
   *   console.error(err)
   *   fallback
   * })
   * ```
   */
  recover<AA>(f: (e: Throwable) => AA): IO<A | AA> {
    return this.recoverWith(a => IO.now(f(a)))
  }


  /**
   * Identifies the `IO` reference type, useful for debugging and
   * for pattern matching in the implementation.
   *
   * @hidden
   */
  readonly _tag!: "pure" | "always" | "once" | "flatMap" | "async" | "memoize"

  // Implements HK<F, A>
  /** @hidden */ readonly _URI!: "funfix/io"
  /** @hidden */ readonly _A!: A

  // Implements Constructor<T>
  /** @hidden */ static readonly _Class: IO<any>

}

function taskToFutureRunLoop(
  start: IO<any>,
  scheduler: Scheduler): Future<any> {

  let current: Current | Try<any> = start
  let bFirst: BindT | null = null
  let bRest: CallStack | null = null

  const modulus = scheduler.executionModel.recommendedBatchSize - 1
  let frameIndex = scheduler.batchIndex

  while (true) {
    if (current instanceof Try) {
      if (current.isSuccess()) {
        const bind = _ioPopNextBind(bFirst, bRest)
        if (!bind) {
          scheduler.batchIndex = frameIndex
          return Future.pure(current.get())
        }

        try {
          current = bind(current.get())
        } catch (e) {
          current = new IOPure(Try.failure(e))
        }
      } else {
        const err = (current as Try<never>).failed().get()
        const bind = _ioFindErrorHandler(bFirst, bRest)
        if (!bind) {
          scheduler.batchIndex = frameIndex
          return Future.raise(err)
        }

        try {
          current = bind(err)
        } catch (e) {
          current = new IOPure(Try.failure(e))
        }
      }

      bFirst = null
      const nextIndex = (frameIndex + 1) & modulus
      // Should we force an asynchronous boundary?
      if (nextIndex) {
        frameIndex = nextIndex
      } else {
        return ioToFutureGoAsync(current, scheduler, bFirst, bRest, true)
      }
    }
    else switch (current._tag) {
      case "pure":
        current = (current as IOPure<any>).value
        break

      case "always":
        current = Try.of((current as IOAlways<any>).thunk)
        break

      case "once":
        current = (current as IOOnce<any>).runTry()
        break

      case "flatMap":
        const flatM: IOFlatMap<any, any> = current as any
        if (bFirst) {
          if (!bRest) bRest = []
          bRest.push(bFirst)
        }

        bFirst = !flatM.g ? flatM.f : [flatM.f, flatM.g]
        current = flatM.source
        break

      case "async":
      case "memoize":
        return ioToFutureGoAsync(current, scheduler, bFirst, bRest, false)
    }
  }
}