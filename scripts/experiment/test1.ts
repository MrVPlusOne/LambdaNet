import * as ts from 'typescript';

interface MediaStreamError {
  readonly constraintName: string | null;
  readonly message: string | null;
  readonly name: string;
}

declare var MediaStreamError: {
  prototype: MediaStreamError;
  new(): MediaStreamError;
};

interface MediaStreamErrorEvent extends Event {
  readonly error: MediaStreamError | null;
}

declare var MediaStreamErrorEvent: {
  prototype: MediaStreamErrorEvent;
  new(typeArg: string, eventInitDict?: MediaStreamErrorEventInit): MediaStreamErrorEvent;
};

/** The MediaStreamEvent interface represents events that occurs in relation to a MediaStream. Two events of this type can be thrown: addstream and removestream. */
interface MediaStreamEvent extends Event {
  readonly stream: MediaStream | null;
}

let storage = top.caches;
let keys = storage.keys();
console.log(keys);
storage = 4;